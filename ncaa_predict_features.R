library(data.table)
set.seed(77)
rm(list=ls())

test<-fread('./project/volume/data/raw/MSampleSubmissionStage2.csv')
season<-fread('./project/volume/data/raw/MRegularSeasonDetailedResults.csv')
tourney<-fread('./project/volume/data/raw/MNCAATourneyDetailedResults.csv')
ranks<-fread('./project/volume/data/raw/MMasseyOrdinals.csv')


test<-data.table(matrix(unlist(strsplit(test$ID,"_")),ncol=3,byrow=T))
setnames(test,c("V1","V2","V3"),c("Season","team_1","team_2"))
test$DayNum<-135

test<-test[,.(team_1,team_2,Season,DayNum)]

test$result<-0.5

# row bind the tournament and season detailed results tables to make an all_games_table

all_games_table<-rbind(season,tourney)

# split the all_games_table into two tables, one with all the stats for the winning team and the other with all the stats for the loosing team.

W_stats<-all_games_table[,.(Season,DayNum,WTeamID,WScore,WFGM,WFGA,WFGM3,WFGA3,WFTM,WFTA,WOR,WDR,WAst,WTO,WStl,WBlk,WPF)]
L_stats<-all_games_table[,.(Season,DayNum,LTeamID,LScore,LFGM,LFGA,LFGM3,LFGA3,LFTM,LFTA,LOR,LDR,LAst,LTO,LStl,LBlk,LPF)]

# Use set names to change the column names of both tables so that they are all the same.

setnames(W_stats,c( "WTeamID","WScore","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF"),
         c("TeamID","Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
setnames(L_stats,c( "LTeamID","LScore","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF"),
         c("TeamID","Score","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))

#  row bind W_stats and L_stats to make a master table

master<-rbind(W_stats,L_stats)
master$result<-1
master$TeamID<-as.character(master$TeamID)

# use a loop to aggregate each of the stats for each of the teams, seasons, and days to make a new table with stats_by_day
stats_by_day<-NULL
for (i in 1:max(master$DayNum)){
  sub_master_stats <- master[DayNum < i]
  team_stats_by_day<-dcast(sub_master_stats,TeamID+Season~.,mean,value.var=c("Score","FGM","FGA","FGM3","FGA3","FTM",
                                                                             "FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
  team_stats_by_day$DayNum<- i
  stats_by_day<-rbind(stats_by_day,team_stats_by_day)
}

# make train
train<-all_games_table[,.(WTeamID,LTeamID,Season,DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))
train$result<-1

master<-rbind(train,test)
master$team_1<-as.character(master$team_1)
master$team_2<-as.character(master$team_2)
master$Season<-as.integer(master$Season)

system_lst<-c("Score","FGM","FGA","FGM3","FGA3","FTM",
              "FTA","OR","DR","Ast","TO","Stl","Blk","PF")
stats_by_day$DayNum<-stats_by_day$DayNum+1

for (i in 1:length(system_lst)){
  x<-c("Season","DayNum","TeamID",system_lst[i])
  one_x<-stats_by_day[, ..x]
  setnames(one_x,"TeamID","team_1")
  
  one_x$team_1<-as.character(one_x$team_1)
  
  setkey(master,Season,team_1,DayNum)
  setkey(one_x,Season,team_1,DayNum)
  
  # merge master and one_rank on daynum
  master<-one_x[master,roll=T]
  setnames(master,system_lst[i],paste0("team_1_",system_lst[i]))
  
  
  setnames(one_x,"team_1","team_2")
  setkey(master,Season,team_2,DayNum)
  setkey(one_x,Season,team_2,DayNum)
  
  master<-one_x[master,roll=T]
  
  setnames(master,system_lst[i],paste0("team_2_",system_lst[i]))
  y1<-c( paste0("team_1_",system_lst[i]))
  y2<-c( paste0("team_2_",system_lst[i]))
  master$dif<-master[,..y2]-master[,..y1]
  
  master[, c(paste0("team_2_",system_lst[i])):=NULL]
  master[, c(paste0("team_1_",system_lst[i])):=NULL]
  setnames(master,"dif",paste0(system_lst[i],"_dif"))
  
}

system_lst2<-c("POM","SAG","MOR","DOK","WLK")
ranks$DayNum<-ranks$RankingDayNum+1

for (i in 1:length(system_lst2)){
  
  one_rank<-ranks[SystemName==system_lst2[i]][,.(Season,DayNum,TeamID,OrdinalRank)]
  setnames(one_rank,"TeamID","team_1")
  
  one_rank$team_1<-as.character(one_rank$team_1)
  
  setkey(master,Season,team_1,DayNum)
  setkey(one_rank,Season,team_1,DayNum)
  
  master<-one_rank[master,roll=T]
  
  setnames(master,"OrdinalRank","team_1_rank")
  setnames(one_rank,"team_1","team_2")
  
  setkey(master,Season,team_2,DayNum)
  setkey(one_rank,Season,team_2,DayNum)
  
  master<-one_rank[master,roll=T]
  
  setnames(master,"OrdinalRank","team_2_rank")
  
  master$rank_dif<-master$team_2_rank-master$team_1_rank
  
  master$team_1_rank<-NULL
  master$team_2_rank<-NULL
  
  setnames(master,"rank_dif",paste0(system_lst2[i],"_dif"))
  
}

master<-master[order(Season,DayNum)]

master<-master[,.(team_1,team_2,Score_dif,FGM_dif,FGA_dif,FGM3_dif,FGA3_dif,FTM_dif,
                  FTA_dif,OR_dif,DR_dif,Ast_dif,TO_dif,Stl_dif,Blk_dif,PF_dif,
                  POM_dif, SAG_dif,MOR_dif,DOK_dif,WLK_dif,result)]

master<-master[!is.na(master$Score_dif)]
master<-master[!is.na(master$FGM_dif)]
master<-master[!is.na(master$FGA_dif)]
master<-master[!is.na(master$FGM3_dif)]
master<-master[!is.na(master$FGA3_dif)]
master<-master[!is.na(master$FTM_dif)]
master<-master[!is.na(master$FTA_dif)]
master<-master[!is.na(master$OR_dif)]
master<-master[!is.na(master$DR_dif)]
master<-master[!is.na(master$Ast_dif)]
master<-master[!is.na(master$TO_dif)]
master<-master[!is.na(master$Stl_dif)]
master<-master[!is.na(master$Blk_dif)]
master<-master[!is.na(master$PF_dif)]


master<-master[!is.na(master$POM_dif)]
master<-master[!is.na(master$SAG_dif)]
master<-master[!is.na(master$MOR_dif)]
master<-master[!is.na(master$DOK_dif)]
master<-master[!is.na(master$WLK_dif)]


test<-master[result==0.5]
train<-master[result==1]


rand_inx<-sample(1:nrow(train),nrow(train)*0.5)
train_a<-train[rand_inx,]
train_b<-train[!rand_inx,]

train_b$result<-0
train_b$Score_dif<-train_b$Score_dif*-1
train_b$FGM_dif<-train_b$FGM_dif*-1
train_b$FGA_dif<-train_b$FGA_dif*-1
train_b$FGM3_dif<-train_b$FGM3_dif*-1
train_b$FGA3_dif<-train_b$FGA3_dif*-1
train_b$FTM_dif<-train_b$FTM_dif*-1
train_b$FTA_dif<-train_b$FTA_dif*-1
train_b$OR_dif<-train_b$OR_dif*-1
train_b$DR_dif<-train_b$DR_dif*-1
train_b$Ast_dif<-train_b$Ast_dif*-1
train_b$TO_dif<-train_b$TO_dif*-1
train_b$Stl_dif<-train_b$Stl_dif*-1
train_b$Blk_dif<-train_b$Blk_dif*-1
train_b$PF_dif<-train_b$PF_dif*-1

train_b$SAG_dif<-train_b$SAG_dif*-1
train_b$MOR_dif<-train_b$MOR_dif*-1
train_b$DOK_dif<-train_b$DOK_dif*-1
train_b$POM_dif<-train_b$POM_dif*-1
train_b$WLK_dif<-train_b$WLK_dif*-1

train<-rbind(train_a,train_b)
fwrite(test,'./project/volume/data/interim/test.csv')
fwrite(train,'./project/volume/data/interim/train.csv')

