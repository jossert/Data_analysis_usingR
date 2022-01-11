library(caret) 
library(data.table)
library(glmnet)
library(Metrics)

rm(list=ls())
set.seed(77)

train<-fread('./project/volume/data/interim/train.csv')
test<-fread('./volume/data/interim/test.csv')

train_y<-train$result
test_y<-test$result

train$team_1<-NULL
train$team_2<-NULL
test$team_1<-NULL
test$team_2<-NULL



train$result<-NULL
test$result<-NULL
train<-as.matrix(train)
test<-as.matrix(test)

cv_model<-cv.glmnet(train, train_y, alpha = 1, type.measure='auc',family = "binomial")
bestlam<-cv_model$lambda.min
gl_model<-glmnet(train, train_y, alpha = 1,type.measure='auc',family = "binomial")
pred<-predict(gl_model,s=bestlam, newx = test,type="response")


#save the result to test
test<-fread('./project/volume/data/interim/test.csv')
test$result<-pred
#mean(ll(test_y,test$result))

#make submit
sub<-fread('.project/volume/data/raw/MSampleSubmissionStage2.csv')
sub$order<-1:nrow(sub)

teams<-data.table(matrix(unlist(strsplit(sub$ID,"_")),ncol=3,byrow=T))
setnames(teams,c("V1","V2","V3"),c("Season","team_1","team_2"))

sub$team_1<-teams$team_1
sub$team_2<-teams$team_2

sub$team_1<-as.character(sub$team_1)
sub$team_2<-as.character(sub$team_2)
test$team_1<-as.character(test$team_1)
test$team_2<-as.character(test$team_2)
submit<-merge(sub,test,all.x=T, by=c("team_1","team_2"))
submit<-submit[order(order)]
submit<-submit[,.(ID,result)]
setnames(submit,c("result"),c("Pred"))

fwrite(submit,"./project/volume/data/processed/submit_POM.csv")

