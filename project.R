#題目改成 Student Performance analysis
#資料來源 https://www.kaggle.com/spscientist/students-performance-in-exams
#共1000筆資料
rm(list=ls())
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(stats)) {
  install.packages("stats")
  library(stats)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

#讀檔
data<-read.csv("C:/Users/user/OneDrive/桌面/StudentsPerformance.csv", header=T, sep=",")
#View(data)
#head(data)

#資料清洗和重整
colnames(data)[2]<-"group" 
#colnames(data)
data$group<-as.character(data$group)
group<-strsplit(data$group," ")
group<-unlist(group)
group<-subset(group,group!="group")
data$group<-group
data$gender<-as.character(data$gender)
data$parental.level.of.education<-as.character(data$parental.level.of.education)
data$lunch<-as.character(data$lunch)
data$test.preparation.course<-as.character(data$test.preparation.course)
data$math.score<-as.numeric(as.character(data$math.score))
data$reading.score<-as.numeric(as.character(data$reading.score))
data$writing.score<-as.numeric(as.character(data$writing.score))

#增加一列觀察成績總和
sum<-c(data$math.score+data$reading.score+data$writing.score)
#將資料合併
data<-cbind(data,sum)
#將資料做排序
data<-data[order(data$sum,decreasing = TRUE),]

View(data)

#男女資料分析
fcount<-filter(data,data$gender=="female")
mcount<-filter(data,data$gender=="male")
gen<-c("male","female")
gencount<-c(nrow(mcount),nrow(fcount))
barplot(gencount,names.arg=gen,col=c("darkblue","red"))

#condition <- rep(c("female","male") , 4)
#specie<-

#男女強科與弱科分析  
scatter_plot<- ggplot(data,aes(x=gender,y=sum)) +
  geom_point()
scatter_plot

#小隊資料分析

#學歷與成績的關係

#考試準備程度與成績結果分析

#午餐與成績結果相關分析
