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

#處理GROUP列的數據,變成(A,B,C,D,E)
Data$group<-as.character(Data$group)
group<-strsplit(Data$group," ")
group<-unlist(group)
group<-subset(group,group!="group")
Data$group<-group

#轉換變數屬性
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

#男女資料分析

# 1.統計人數
fcount<-filter(Data,Data$gender=="female")
mcount<-filter(Data,Data$gender=="male")
gen<-c("male","female")
gencount<-c(nrow(mcount),nrow(fcount))
genderNum<-barplot(gencount,names.arg=gen,col=c("darkblue","red"))

# 2.性別與各科成績分析(可看每隊)  
data<-Data

gender1<-ggplot(data, aes(x=gender, y=math.score)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

gender2<-ggplot(data, aes(x=gender, y=reading.score)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

gender3<-ggplot(data, aes(x=gender, y=writing.score)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

gender4<-ggplot(data, aes(x=gender, y=sum)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

gender_all<-ggarrange(gender1,gender2,gender3,gender4,nrow=2,ncol=2,labels = c("math.score","reading.score","writing.score","sum.score"))

#小隊資料分析

#1.分析小隊 (1.每隊學歷 2.每隊吃午餐紀錄 3.每隊準備考試紀錄)

#groupA
groupA<-filter(data,data$group=="A")
tableA<-table(groupA$parental.level.of.education)
tableA<-as.data.frame(tableA)

# 1.(bar chart)
groupA1<-ggplot(tableA,aes(x=Var1,y=Freq))+
  geom_bar(stat="identity",aes(fill=Var1))

# 2.(pie chart)
groupA2<-ggplot(groupA,aes(fill=lunch,y=lunch,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# 3.(pie chart)
groupA3<-ggplot(groupA,aes(fill=test.preparation.course,y=test.preparation.course,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
  
#groupB
groupB<-filter(data,data$group=="B")
tableB<-table(groupB$parental.level.of.education)
tableB<-as.data.frame(tableB)

groupB1<-ggplot(tableB,aes(x=Var1,y=Freq))+
  geom_bar(stat="identity",aes(fill=Var1))

groupB2<-ggplot(groupB,aes(fill=lunch,y=lunch,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

groupB3<-ggplot(groupB,aes(fill=test.preparation.course,y=test.preparation.course,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#groupC
groupC<-filter(data,data$group=="C")
tableC<-table(groupC$parental.level.of.education)
tableC<-as.data.frame(tableC)
groupC1<-ggplot(tableC,aes(x=Var1,y=Freq))+
  geom_bar(stat="identity",aes(fill=Var1))

groupC2<-ggplot(groupC,aes(fill=lunch,y=lunch,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

groupC3<-ggplot(groupC,aes(fill=test.preparation.course,y=test.preparation.course,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#groupD
groupD<-filter(data,data$group=="D")
tableD<-table(groupD$parental.level.of.education)
tableD<-as.data.frame(tableD)
groupD1<-ggplot(tableD,aes(x=Var1,y=Freq))+
  geom_bar(stat="identity",aes(fill=Var1))

groupD2<-ggplot(groupD,aes(fill=lunch,y=lunch,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

groupD3<-ggplot(groupD,aes(fill=test.preparation.course,y=test.preparation.course,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

##groupE
groupE<-filter(data,data$group=="E")
tableE<-table(groupE$parental.level.of.education)
tableE<-as.data.frame(tableE)

groupE1<-ggplot(tableE,aes(x=Var1,y=Freq))+
  geom_bar(stat="identity",aes(fill=Var1))

groupE2<-ggplot(groupE,aes(fill=lunch,y=lunch,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

groupE3<-ggplot(groupE,aes(fill=test.preparation.course,y=test.preparation.course,x=""))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#以上各小隊資料結合圖
GOURP_ALL<-ggarrange(groupA1,groupA2,groupA3,groupB1,groupB2,groupB3,groupC1,groupC2,groupC3,groupD1,groupD2,groupD3,groupE1,groupE2,groupE3,ncol=3,nrow=5)

# 2.分析小隊成績(使用violin chart)

# math.score performance of each team
group_math<-ggplot(data, aes(x=group, y=math.score)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

# writing.score performance of each team
grouop_writing<-ggplot(data, aes(x=group, y=writing.score)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

#reading.score performance of each team
group_reading<-ggplot(data, aes(x=group, y=reading.score)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

## score sum performance of each team
group_sum<-ggplot(data, aes(x=group, y=sum)) + 
  geom_violin(aes(fill = group),draw_quantiles = c(0.25, 0.5, 0.75))

#小隊成績結合圖
# Please zoom the picture p5 !!
group_all<-ggarrange(group_math,grouop_writing,group_reading,group_sum,nrow=2,ncol=2,labels=c("math","writing","reading","sum"))

#學歷與成績的關係 (使用box chart)
# -> 篩選最佳狀態下的學生: prepare=completed, lunch=standard

level<-filter(Data,Data$lunch=="standard",Data$test.preparation.course=="completed")

level_score<-ggplot(level, aes(x=parental.level.of.education, y=sum)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) +    # set color
  xlab("level")


#考試準備程度與成績結果分析(使用box chart)
# -> 篩選最佳狀態下的學生: lunch=standard
prepare<-filter(Data,Data$lunch=="standard")

prepare_score<-ggplot(prepare, aes(x=test.preparation.course, y=sum)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) +    # set color
  xlab("prepared")

#午餐與成績結果相關分析 (使用box chart)
# -> 篩選最佳狀態下的學生: prepare=completed
Lunch<-filter(Data,Data$test.preparation.course=="completed")
lunch_score<-ggplot(Lunch, aes(x=lunch, y=sum)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) +    #set color
  xlab("lunch")

#以上三個屬性結合圖
attributes_all<-ggarrange(level_score,prepare_score,lunch_score,nrow=2,ncol=2,labels=c("math","writing","reading","sum"))

#在此可查看全部圖表
genderNum
gender_all
GOURP_ALL
group_all
attributes_all
