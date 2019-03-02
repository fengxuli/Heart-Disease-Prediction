
setwd("~/Desktop/JHU/Spring_1/Data Analytics/Group Project")

install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
install.packages("dplyr")
install.packages("mice")
install.packages("randomForest")
library(ggplot2) 
library(ggthemes) 
library(scales) 
library(dplyr) 
library(mice) 



file.choose()


HeartData <- read.csv("processed.cleveland.csv",sep=",",na.strings = '?')

names(HeartData) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
View(HeartData)
dim(HeartData)

str(HeartData)

HeartData = hd_origin

HeartData$Disease1 <- 1
HeartData$Disease1[HeartData$num == 0] <- 0


# classsify "age", "trestbps","chol""thalach""oldpeak"
HeartData$age <- cut(HeartData$age, breaks=10)
HeartData$trestbps <- cut(HeartData$trestbps, breaks=15)
HeartData$chol <- cut(HeartData$chol, breaks=5)
HeartData$thalach <- cut(HeartData$thalach, breaks=5)
HeartData$oldpeak <- cut(HeartData$oldpeak, breaks=5)

#name our factors
HeartData$sex <- factor(c("female","male"))

# obtain the level table for each character feature
sapply(HeartData, table)

# converted all character features into factor
HeartData[, sapply(HeartData, is.numeric)] <- lapply( HeartData[, sapply(HeartData, is.numeric )], factor) 
str(HeartData)

# age
g<-ggplot(data=HeartData,aes(x=ca,fill=factor(Disease1)))+
  geom_bar(position='stack')

# Trestbps
ggplot(data=HeartData,aes(x=fbs,fill=factor(Disease1)))+
    scale_fill_manual(values=c('#6285a3','#dcc48c'), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(position='stack') +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

# age & Sex & trestbps

ggplot(data=HeartData,aes(x=sex,fill=factor(Disease1)))+
  geom_bar(position='stack')+facet_grid(. ~ cp)+guides(fill = guide_legend(reverse = TRUE))

#age&trestbps
g+facet_wrap(~HeartData$trestbps)
g + facet_wrap(~HeartData$sex)

#age&slope
g+facet_wrap(~HeartData$slope)

#age&ca
g+facet_wrap(~HeartData$ca)


