rm(list=ls())
library(ggplot2)
library(stats)
library(dplyr)
if(!require(gapminder))
{
  install.packages(gapminder)
  library(gapminder)
}
library(gapminder)
data<-read.csv("C:/Users/user/OneDrive/桌面/StudentsPerformance.csv", header=T, sep=",")
#View(data)
#head(data)

colnames(data)[2]<-"group" 
#colnames(data)

#SP<-data.frame(){
#  as.character(data$gender,data$group)
#}
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
class(data$gender)
class(data$math.score)
data$math.score
sum<-c(data$math.score+data$reading.score+data$writing.score)
data<-cbind(data,sum)
data<-data[order(data$sum,decreasing = TRUE),]
data
View(data)
scatter_plot<- ggplot(data,aes(x=gender,y=sum)) +
  geom_point()
scatter_plot
