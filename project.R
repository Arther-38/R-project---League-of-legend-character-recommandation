#題目改成 Student Performance analysis
#資料來源 https://www.kaggle.com/spscientist/students-performance-in-exams
#共1000筆資料
rm(list=ls())
if (!require(ggplot2)) {      #show graph
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(stats)) {
  install.packages("stats")
  library(stats)
}
if (!require(dplyr)) {       #filter,%>%
  install.packages("dplyr")
  library(dplyr)
}
if (!require(ggpubr)) {    #ggarrange
install.packages("ggpubr")
library(ggpubr)
}

#讀檔
setwd("C:/Users/user/OneDrive/桌面")
Data<-read.csv("StudentsPerformance.csv", header=T, sep=",")
#View(Data)
#head(Data)

#資料清洗和重整
colnames(Data)[2]<-"group" 
#colnames(Data)
Data$group<-as.character(Data$group)
group<-strsplit(Data$group," ")
group<-unlist(group)
group<-subset(group,group!="group")
Data$group<-group
Data$gender<-as.character(Data$gender)
Data$parental.level.of.education<-as.character(Data$parental.level.of.education)
Data$lunch<-as.character(Data$lunch)
Data$test.preparation.course<-as.character(Data$test.preparation.course)
Data$math.score<-as.numeric(as.character(Data$math.score))
Data$reading.score<-as.numeric(as.character(Data$reading.score))
Data$writing.score<-as.numeric(as.character(Data$writing.score))

#增加一列觀察成績總和
sum<-c(Data$math.score+Data$reading.score+Data$writing.score)
#將資料合併
Data<-cbind(Data,sum)
#將資料做排序
Data<-Data[order(Data$sum,decreasing = TRUE),]

View(Data)
reverse(unique(Data$gender))

table(Data$parental.level.of.education)

#男女資料分析
# 1.統計人數
fcount<-filter(Data,Data$gender=="female")
mcount<-filter(Data,Data$gender=="male")
gen<-c("male","female")
gencount<-c(nrow(mcount),nrow(fcount))
barplot(gencount,names.arg=gen,col=c("darkblue","red"))

# 2.性別與各科成績分析  
data<-Data
data$gender[which(data$gender=="female")]<-0
data$gender[which(data$gender=="male")]<-1
data$gender<-as.numeric(as.character(data$gender))
f1<-sum(fcount$writing.score)
m1<-sum(mcount$writing.score)
cor(data$gender,data$sum)
cor(data$gender,data$math.score)

p1<-mcount %>% ggplot( aes(x=rownames(mcount), y=mcount$math.score)) +
  geom_line() +
  geom_point()

p2<-fcount  %>% ggplot( aes(x=rownames(fcount), y=fcount$math.score)) +
  geom_line() +
  geom_point()

ggarrange(p1,p2,nrow=1,labels = c("male","female"))

cor(data$gender,data$reading.score)
p3<-mcount %>% ggplot( aes(x=rownames(mcount), y=mcount$reading.score)) +
  geom_line() +
  geom_point()

p4<-fcount  %>% ggplot( aes(x=rownames(fcount), y=fcount$reading.score)) +
  geom_line() +
  geom_point()
ggarrange(p3,p4,nrow=1,labels = c("male","female"))

cor(data$gender,data$writing.score)
p5<-mcount %>% ggplot( aes(x=rownames(mcount), y=mcount$writing.score)) +
  geom_line() +
  geom_point()

p6<-fcount  %>% ggplot( aes(x=rownames(fcount), y=fcount$writing.score)) +
  geom_line() +
  geom_point()
ggarrange(p3,p4,nrow=1,labels = c("male","female"))

cor(data$gender,data$sum)

p7<-mcount %>% ggplot( aes(x=rownames(mcount), y=mcount$sum)) +
  geom_line() +
  geom_point()

p8<-fcount  %>% ggplot( aes(x=rownames(fcount), y=fcount$sum)) +
  geom_line() +
  geom_point()
ggarrange(p7,p8,nrow=1,labels = c("male","female"))

#小隊資料分析

#1.分析小隊學歷
groupA<-filter(data,data$group=="A")
tableA<-table(groupA$parental.level.of.education)
tableA1<-table(groupA$sum)
tableA<-as.data.frame(tableA)
tableA1<-as.data.frame(tableA1)
groupB<-filter(data,data$group=="B")
tableB<-table(groupB$parental.level.of.education)
tableB1<-table(groupB$sum)
tableB<-as.data.frame(tableB)
tableB1<-as.data.frame(tableB1)
groupC<-filter(data,data$group=="C")
tableC<-table(groupC$parental.level.of.education)
tableC1<-table(groupC$sum)
tableC<-as.data.frame(tableC)
tableC1<-as.data.frame(tableC1)
groupD<-filter(data,data$group=="D")
tableD<-table(groupD$parental.level.of.education)
tableD1<-table(groupD$sum)
tableD<-as.data.frame(tableD)
tableD1<-as.data.frame(tableD1)
groupE<-filter(data,data$group=="E")
tableE<-table(groupE$parental.level.of.education)
tableE1<-table(groupE$sum)
tableE<-as.data.frame(tableE)
tableE1<-as.data.frame(tableE1)

#分析小隊成績
p1<-ggplot(tableA, aes(x=unique(data$parental.level.of.education), y=Freq)) + 
  geom_bar(stat = "identity", fill=c("red","blue","yellow","green","purple","orange") )
p2<-ggplot(tableB, aes(x=unique(data$parental.level.of.education), y=Freq)) + 
  geom_bar(stat = "identity", fill=c("red","blue","yellow","green","purple","orange") )
p3<-ggplot(tableC, aes(x=unique(data$parental.level.of.education), y=Freq)) + 
  geom_bar(stat = "identity", fill=c("red","blue","yellow","green","purple","orange") )
p4<-ggplot(tableD, aes(x=unique(data$parental.level.of.education), y=Freq)) + 
  geom_bar(stat = "identity", fill=c("red","blue","yellow","green","purple","orange") )
p5<-ggplot(tableE, aes(x=unique(data$parental.level.of.education), y=Freq)) + 
  geom_bar(stat = "identity", fill=c("red","blue","yellow","green","purple","orange") )
p6<-tableA1 %>% ggplot( aes(x=rownames(tableA1), y=tableA1$Var1)) +
  geom_line() +
  geom_point()
p7<-tableB1 %>% ggplot( aes(x=rownames(tableB1), y=tableB1$Var1)) +
  geom_line() +
  geom_point()
p8<-tableC1 %>% ggplot( aes(x=rownames(tableC1), y=tableC1$Var1)) +
  geom_line() +
  geom_point()
p9<-tableD1 %>% ggplot( aes(x=rownames(tableD1), y=tableD1$Var1)) +
  geom_line() +
  geom_point()
p10<-tableE1 %>% ggplot( aes(x=rownames(tableE1), y=tableE1$Var1)) +
  geom_line() +
  geom_point()

ggarrange(p6,p7,p8,p9,p10,nrow=1,labels=c("A","B","C","D","E"))

#學歷與成績的關係


#考試準備程度與成績結果分析


#午餐與成績結果相關分析



