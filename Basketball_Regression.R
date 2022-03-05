library(tidyverse)

#Read data from 2019-2020 tournament
Games <- read.csv("~/Data/Games.txt", header=FALSE)
names(Games)=c('Name','Outcome')
Assists <- read.csv("~/Data/Assists.csv")
Turnovers <- read.csv("~/Data/Turnovers.csv")
FT.percent <- read.csv("~/Data/FT-percent.csv")
FG.percent.D <- read.csv("~/Data/FG-percent-D.csv")
FG.percent <- read.csv("~/Data/FG-percent.csv")
WL.percent <- read.csv("~/Data/WL-percent.csv")
#stats compiled from separate source
Bball <- read.table("~/Data/Bstat.txt",header=FALSE, sep=',',stringsAsFactors = FALSE)
Bball <- subset(Bball, select = c(V2,V6,V12,V13,V16))
names(Bball) = c("Name","W_Percentage","Own_PPG","Opp_PPG","Sched_Difficultly")
Bball$Name[which((Bball$Name=='Saint Marys (CA)')%>%replace(is.na(.),TRUE))] = 'Saint Mary\'s (CA)'

#clean and assemble data
WL.percent = WL.percent%>%select(Name,Pct)%>%drop_na()
Turnover = Turnovers%>%select(Name,Avg)%>%drop_na()
Assists = Assists%>%mutate(APG=AST/GM)%>%select(Name,APG)%>%drop_na()
FG.percent.D = FG.percent.D%>%select(Name,OPP.FG.)
FG.percent = FG.percent%>%select(Name,FG.)%>%drop_na()
FT.percent = FT.percent%>%select(Name,FT.)%>%drop_na()
data = Games%>%left_join(Assists,by='Name')%>%left_join(FG.percent.D,by='Name')%>%
  left_join(FT.percent,by='Name')%>%left_join(Bball,by="Name")%>%left_join(Turnover,by='Name')

final_stats = tibble(APG=numeric(),OPP.FG.=numeric(),FT.=numeric(),W_Percentage=numeric(),
                     Own_PPG=numeric(),Opp_PPG=numeric(),Sched_Difficultly=numeric(),
                     Avg=numeric(),Outcome=numeric())
for (i in seq(1,64,by=2)){
  final_stats = add_row(final_stats,cbind(data[i+1,3:10]-data[i,3:10],Outcome=data[i+1,2]))
}
names(final_stats)=c('APG','OPP.FG','FTP','WL','OWN.PPG','OPP.PPG','SCHD','Turnovers','Outcome')

#fit logistic regression after feature engineering
#Minimizing AIC
mod = glm(Outcome~SCHD+OPP.PPG+APG,data=final_stats,family = 'binomial')
summary(mod)

aov(Outcome~SCHD+APG+OPP.PPG,data=final_stats)
cor(final_stats)
view(data)


#validate

Games_2021 <- read.csv("~/Data/Games_2021.txt", header=FALSE)
names(Games_2021)=c('Name','Outcome')

B2021 <- read.table("~/Data/Bstats2021.txt",header=FALSE, sep=',',stringsAsFactors = FALSE)
B2021 <- subset(B2021, select = c(V2,V6,V12,V13,V16))
names(B2021) = c("Name","W_Percentage","Own_PPG","Opp_PPG","Sched_Difficultly")
B2021$Name[which((B2021$Name=='Saint Marys (CA)')%>%replace(is.na(.),TRUE))] = 'Saint Mary\'s (CA)'

FG.percent.D_2021 <- read.csv("~/Data/FG-percent-D_2021.csv")
FG.percent.D_2021 = FG.percent.D_2021%>%select(Team,OPP.FG.)
FG.percent.D_2021$Team = gsub("\\s*\\([^\\)]+\\)","",as.character(FG.percent.D_2021$Team))
names(FG.percent.D_2021) = c("Name","OPP.FG.")

Assists_2021 <- read.csv("~/Data/Assists_2021.csv")
Assists_2021 = Assists_2021%>%select(Team,APG)%>%drop_na()
Assists_2021$Team = gsub("\\s*\\([^\\)]+\\)","",as.character(Assists_2021$Team))
names(Assists_2021) = c("Name","APG")


data = Games_2021%>%left_join(Assists_2021,by='Name')%>%left_join(FG.percent.D_2021,by='Name')%>%left_join(B2021,by="Name")

final_stats_2021 = tibble(APG=numeric(),OPP.FG.=numeric(),W_Percentage=numeric(),
                     Own_PPG=numeric(),Opp_PPG=numeric(),Sched_Difficultly=numeric(),Outcome=numeric())
for (i in seq(1,length(data[,1]),by=2)){
  final_stats_2021 = add_row(final_stats_2021,cbind(data[i+1,3:8]-data[i,3:8],Outcome=data[i+1,2]))
}
Outcome_2021 = final_stats_2021$Outcome
final_stats_2021 <- subset(final_stats_2021, select = c(APG,Opp_PPG,Sched_Difficultly))

names(final_stats_2021)=c('APG','OPP.PPG','SCHD')

#make prediction
predictions = as.integer(predict.glm(mod,final_stats_2021,type='response')>.5)
sum(predictions==Outcome_2021)/length(predictions)
