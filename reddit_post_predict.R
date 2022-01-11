#  Reddit Post Topic Model
# Yichen Shi 

library(httr)
library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ggplot2)
library(ClusterR)
library(Metrics)
library(xgboost)

rm(list=ls())
set.seed(3)

test<-fread('./project/volume/data/raw/test_file.csv')
train<-fread('./project/volume/data/raw/training_data.csv')
train_emb<-fread('./project/volume/data/raw/training_emb.csv')
test_emb<-fread('./project/volume/data/raw/test_emb.csv')
submit<-fread('./project/volume/data/raw/example_sub.csv')

# change train to long format
train$ID<- 1:nrow(train)
train <- melt(train, id.vars = c("id", "text","ID"),
             measure.vars = c(3:12),variable.name = "reddit")
train<-setorder(train[value==1],"ID")[,c("ID","value"):=NULL]

r<-train$reddit
train$reddit<-as.integer(train$reddit)-1

# create a master data that combine train and test
test$reddit<-0
train<-cbind(train,train_emb)
test<-cbind(test,test_emb)
master<-rbind(train,test)
data<-master[,c(4:515)]

# perform pca and tsne

pca<-prcomp(data)
pca_dt<-data.table(unclass(pca)$x)

ggplot(pca_dt,aes(x=PC1,y=PC2))+geom_point()

tsne<-Rtsne(pca_dt,pca = F,perplexity=50,check_duplicates = F)

tsne_dt<-data.table(tsne$Y)

ggplot(tsne_dt,aes(x=V1,y=V2,col=master$reddit))+geom_point()

#
k_bic<-Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 100,criterion = "BIC")

delta_k<-c(NA,k_bic[-1] - k_bic[-length(k_bic)])

del_k_tab<-data.table(delta_k=delta_k,k=1:length(delta_k))

ggplot(del_k_tab,aes(x=k,y=-delta_k))+geom_point()+geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label=k),hjust=0, vjust=-1)

opt_k<-4
gmm_data<-GMM(tsne_dt[,.(V1,V2)],opt_k)

# convert log-likelihood into a probability

l_clust<-gmm_data$Log_likelihood^10
l_clust<-data.table(l_clust)
net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})
cluster_prob<-1/l_clust/net_lh

# split train and test from the cluster probability data
train2<-cluster_prob[1:200]
test2<-cluster_prob[201:nrow(master)]
train2$reddit<-train$reddit
test2$reddit<-test$reddit

# do xgboost
y.train<-train$reddit
dummies <- dummyVars(reddit~ ., data = train2)
x.train<-predict(dummies, newdata = train2)
x.test<-predict(dummies, newdata = test2)

dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)

hyper_perm_tune<-NULL

param <- list(  objective           = "multi:softprob",
                gamma               =0.02,
                booster             = "gbtree",
                eval_metric         = "mlogloss",
                eta                 = 0.02,
                max_depth           = 5,
                min_child_weight    = 1,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                tree_method = 'hist',
                num_class =10
)


XGBm<-xgb.cv( params=param,nfold=5,nrounds=10000,missing=NA,data=dtrain,print_every_n=1,early_stopping_rounds=25)

best_ntrees<-unclass(XGBm)$best_iteration

new_row<-data.table(t(param))

new_row$best_ntrees<-best_ntrees

test_error<-unclass(XGBm)$evaluation_log[best_ntrees,]$test_rmse_mean
new_row$test_error<-test_error
hyper_perm_tune<-rbind(new_row,hyper_perm_tune)


watchlist <- list( train = dtrain)

XGBm<-xgb.train( params=param,nrounds=best_ntrees,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1)

# create the submission file
pred<-predict(XGBm, newdata = dtest,reshape=T)
pred<-cbind(submit$id,pred)
colnames(pred)<-colnames(submit)
pred<-data.table(pred)
fwrite(pred,"./project/volume/data/processed/submit.csv")
