# Data exploration of Group Project
# 
# -*- Coding: utf-8 -*-

### I. Data Preprocessing

# Load the data into hd_origin
setwd("~/Desktop/JHU/Spring_1/Data Analytics/Group Project")
hd_origin = read.csv("processed.cleveland.csv", na='?')

# Load in requiring packages
# install.packages('corrplot')
# install.packages('ggplot2')

library('corrplot')
library('ggplot2')

# Name the columns 
colnames(hd_origin) = c('age', 
                        'sex', 
                        'cp', 
                        'trestbps', 
                        'chol', 
                        'fbs', 
                        'Restecg', 
                        'thalach', 
                        'exang', 
                        'oldpeak', 
                        'slope', 
                        'ca', 
                        'thal', 
                        'num')

# Processing the missing values, since there are only 6 missing values, we simply delete those rows
sum(is.na(hd_origin))
hd_origin = na.omit(hd_origin)
hd = hd_origin

# The num column should have two binomial values: 0, 1. Thus we should adjust the values more than 0 as 1
hd$num[hd$num > 0] <- 1

# Display the dataframe, and we find all columns are numerical catogeries. 
str(hd) 

# Transfer the catogery columns to factor class
hd$sex = factor(hd$sex, labels = c('Female', 'Male'))
hd$cp = factor(hd$cp, labels = c('Typical', 'Atypical', 'Non-anginal', 'Asymptomatic'))
hd$fbs = factor(hd$fbs, labels = c('False','True'))
hd$Restecg = factor(hd$Restecg, labels = c('Normal', 'ST-T', 'Left'))
hd$exang = factor(hd$exang, labels = c('No', 'Yes'))
hd$slope = factor(hd$slop, labels = c('Upsloping', 'Flat', 'Downsloping'))
hd$thal = factor(hd$thal, labels = c('Normal', 'Fixed', 'Reversable'))
hd$num = factor(hd$num)

attach(hd)

col_0 = '#6285a3'  # blue
col_1 = '#dcc48c'  # yellow





################## Yingxue

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




##################







### II. Data Exploration
## 1. Scatterplot Matrices of Variables
pairs(~., data = hd)


#  Indentify the multi-correlationship between variables
M<-cor(hd[,c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')])
head(round(M,2))
corrplot(M, method='number', tl.col="black", tl.srt=45)


## 2. Patients vs Healthy
par(mfrow=c(1,1))
barplot(table(hd$num), col=c(col_0, col_1), xlab='Heart Disease', ylab='Frequency')

# Distribution of Heart Disease
ggplot(as.data.frame(table(hd$num)),aes(Var1, Freq, fill=Var1)) + 
    geom_bar(stat="identity") + 
    labs(x="num",y="Volume of People", title='Distribution of Heart Disease') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_text(aes(label=Freq), position = position_stack(vjust = 0.5), color='white') 

## 3. Explore the distribution of disease with different ages
# Frequency table and barchart
age_range <- max(hd$age)-min(hd$age)
labels <- c("< 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">= 70")
breaks <- c(1,30,40,50,60,70,100)
mytable1 <- cut(hd$age[num == 0], breaks = breaks, labels = labels, right = TRUE )
mytable2 <- cut(hd$age[num == 1], breaks = breaks, labels = labels, right = TRUE )

df1 <- as.data.frame(table(age=mytable1))
df1['num'] = 0
df2 <- as.data.frame(table(age=mytable2))
df2['num'] = 1
df = rbind(df1, df2)
df$num = as.factor(df$num)

ggplot(df,aes(age, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="age",y="Volume of People",title='Distribution of Heart Disease(age)') + 
    theme(plot.title = element_text(hjust = 0.5))



# Yingxue
ggplot(data=HeartData,aes(x=age,fill=factor(Disease1)))+
    geom_bar(position='dodge') +
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    labs(x="age",y="Volume of People",title='Distribution of Heart Disease(age groups)') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))





## 4. Explore the relationships between sex and heart disease
df_sex = as.data.frame(table(hd$num, hd$sex))
names(df_sex) = c('num', 'sex', 'Freq')
ggplot(df_sex, aes(sex, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="sex",y="Volume of People",title='Distribution of Heart Disease(sex)') + 
    theme(plot.title = element_text(hjust = 0.5))


# Show number of female and male
ggplot(as.data.frame(table(hd$sex)),aes(Var1, Freq, fill=Var1)) + 
    geom_bar(stat="identity") + 
    labs(x="sex",y="Volume of People", title='Distribution of Sex') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c(col_0,col_1), name=NULL) +
    geom_text(aes(label=Freq), position = position_stack(vjust = 0.5), color='white') 



## 5. Explore the relationships between trestbps and heart disease
ggplot(hd, aes(x=trestbps, y=num)) + geom_point(size=2,shape=21)
summary(hd$trestbps[hd$num == 0])
summary(hd$trestbps[hd$num == 1])

# Draw boxplot to display the statistics of trestbps of two groups
ggplot(hd, aes(x=num, y=trestbps))+geom_boxplot(aes(fill=num)) +
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    labs(x="num",y="trestbps", title='Trestbps') +
    theme(plot.title = element_text(hjust = 0.5))


# Trestbps
ggplot(data=HeartData,aes(x=trestbps,fill=factor(Disease1)))+
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(position='stack') +
    labs(x="trestbps",y="Volume of People", title='Distribution of Heart Disease(trestbps group)') +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
          plot.title = element_text(hjust = 0.5))

    

## 6. Explore the relationships between chol and heart disease
ggplot(hd, aes(x=chol, y=num)) + geom_point(size=2,shape=21)
summary(hd$chol[hd$num == 0])
summary(hd$chol[hd$num == 1])

# Draw boxplot to display the statistics of chol of two groups
ggplot(hd, aes(x=num, y=chol))+geom_boxplot(aes(fill=num)) +
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    labs(x="num",y="chol", title='Chol') +
    theme(plot.title = element_text(hjust = 0.5))

# Yingxue
ggplot(data=HeartData,aes(x=chol,fill=factor(Disease1)))+
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    labs(x="chol",y="Volume of People", title='Distribution of Heart Disease(chol)') +
    geom_bar(position='dodge') +
    theme(plot.title = element_text(hjust = 0.5))


## 7. Explore the relationships between fbs and heart disease
df_fbs = as.data.frame(table(hd$num, hd$fbs))
names(df_fbs) = c('num', 'fbs', 'Freq')
ggplot(df_fbs, aes(fbs, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="fbs",y="Volume of People",title='Distribution of Heart Disease(fbs)') + 
    theme(plot.title = element_text(hjust = 0.5))


## 8. Explore the relationships between Restecg and heart disease
df_Restecg = as.data.frame(table(hd$num, hd$Restecg))
names(df_Restecg) = c('num', 'Restecg', 'Freq')
ggplot(df_Restecg, aes(Restecg, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="Restecg",y="Volume of People",title='Distribution of Heart Disease(restecg)') + 
    theme(plot.title = element_text(hjust = 0.5))

# which showes that it might be more significant if we catogerize ST-T and Left to a catogery

## 9. Explore the relationships between thalach and heart disease
ggplot(hd, aes(x=thalach, y=num)) + geom_point(size=2,shape=21)
summary(hd$thalach[hd$num == 0])
summary(hd$thalach[hd$num == 1])

# Draw boxplot to display the statistics of thalach of two groups
ggplot(hd, aes(x=num, y=thalach))+geom_boxplot(aes(fill=num)) +
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    labs(x="num",y="thalach", title='thalach') +
    theme(plot.title = element_text(hjust = 0.5))

# Have significant relationships, but not significant in regression?

## 10. Explore the relationships between exang and heart disease
df_exang = as.data.frame(table(hd$num, hd$exang))
names(df_exang) = c('num', 'exang', 'Freq')
ggplot(df_exang, aes(exang, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="exang",y="Volume of People",title='Distribution of Heart Disease(exang)') +
    theme(plot.title = element_text(hjust = 0.5))

# Have significant relationships, but not significant in regression?


## 11. Explore the relationships between oldpeak and heart disease
ggplot(hd, aes(x=oldpeak, y=num)) + geom_point(size=2,shape=21)
summary(hd$oldpeak[hd$num == 0])
summary(hd$oldpeak[hd$num == 1])

# Draw boxplot to display the statistics of oldpeak of two groups
ggplot(hd, aes(x=num, y=oldpeak))+geom_boxplot(aes(fill=num)) +
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    labs(x="num",y="oldpeak",title='oldpeak') +
    theme(plot.title = element_text(hjust = 0.5))

# Yingxue
ggplot(data=HeartData,aes(x=oldpeak,fill=factor(Disease1)))+
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(position='stack') +
    labs(x="oldpeak",y="Volume of People",title='Distribution of Heart Disease(oldpeak)') +
    theme(plot.title = element_text(hjust = 0.5))



## 12. Explore the relationships between slope and heart disease
df_slope = as.data.frame(table(hd$num, hd$slope))
names(df_slope) = c('num', 'slope', 'Freq')
ggplot(df_slope, aes(slope, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="slope",y="Volume of People",title='Distribution of Heart Disease(slope)') +
    theme(plot.title = element_text(hjust = 0.5))

## 13. Explore the relationships between ca and heart disease
ggplot(hd, aes(x=ca, y=num)) + geom_point(size=2,shape=21)
summary(hd$ca[hd$num == 0])
summary(hd$ca[hd$num == 1])

# Draw boxplot to display the statistics of ca of two groups
ggplot(hd, aes(x=num, y=ca))+geom_boxplot(aes(fill=num)) +
    scale_fill_manual(values=c(col_0,col_1)) +
    theme(legend.position="none")

# Yingxue
ggplot(data=HeartData,aes(x=ca,fill=factor(Disease1)))+
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(position='stack') +
    labs(x="ca",y="Volume of People",title='Distribution of Heart Disease(ca)') +
    theme(plot.title = element_text(hjust = 0.5))





## 14. Explore the relationships between thal and heart disease
df_thal = as.data.frame(table(hd$num, hd$thal))
names(df_thal) = c('num', 'thal', 'Freq')
ggplot(df_thal, aes(thal, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="thal",y="Volume of People",title='Distribution of Heart Disease(thal)') +
    theme(plot.title = element_text(hjust = 0.5))

## 15. Explore the relationships between cp and heart disease
df_exang = as.data.frame(table(hd$num, hd$cp))
names(df_exang) = c('num', 'cp', 'Freq')
ggplot(df_exang, aes(cp, Freq, fill=num)) + 
    scale_fill_manual(values=c(col_0,col_1), 
                      name=NULL,
                      breaks=c(0, 1),
                      labels=c('Healthy', 'Heart Disease')) +
    geom_bar(stat="identity",position="dodge") + 
    labs(x="cp",y="Volume of People",title='Distribution of Heart Disease(cp)') + 
    theme(plot.title = element_text(hjust = 0.5))






logreg.fit1 <- glm(num~oldpeak,data=hd,family=binomial)
summary(logreg.fit1)









