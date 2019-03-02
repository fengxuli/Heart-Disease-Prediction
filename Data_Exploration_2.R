

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


HeartData <- read.csv("/Users/yingxue_long/Desktop/processed.cleveland.csv",sep=",",na.strings = '?')

names(HeartData) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
View(HeartData)
dim(HeartData)

str(HeartData)
HeartData$Disease1 <- 1
HeartData$Disease1[HeartData$num == 0] <- 0

# classsify "age", "trestbps","chol""thalach""oldpeak"
HeartData$age <- cut(HeartData$age, breaks=5)
HeartData$trestbps <- cut(HeartData$trestbps, breaks=5)
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
g<-ggplot(data=HeartData,aes(x=age,fill=factor(Disease1)))+
  geom_bar(position='stack')

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


