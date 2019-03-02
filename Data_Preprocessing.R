# Group Project: Heart Disease Prediction
# Group 1 (DeepMind): Yingxue Long, Chengyue Wang, Zheng Yu, Fengxu Li
# -*- coding: utf-8 -*-


## Part I. Data Preprocessing
setwd("~/github/Heart-Disease-Prediction")

# Importing the dataset
hd = read.csv('processed.cleveland.csv', na='?')
sum(is.na(hd))
hd = na.omit(hd)  # Deleting missing data

# Rename the column names
colnames(hd) = c('age', 
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

# The num column should have two binomial values: 0, 1. Thus we should adjust the values more than 0 as 1
hd$num[hd$num > 0] <- 1

# Transfer the catogery columns to factor class
hd$sex = factor(hd$sex, labels = c('Female', 'Male'))
hd$cp = factor(hd$cp, labels = c('Typical', 'Atypical', 'Non.anginal', 'Asymptomatic'))
hd$fbs = factor(hd$fbs, labels = c('False','True'))
hd$Restecg = factor(hd$Restecg, labels = c('Normal', 'ST.T', 'Left'))
hd$exang = factor(hd$exang, labels = c('No', 'Yes'))
hd$slope = factor(hd$slop, labels = c('Upsloping', 'Flat', 'Downsloping'))
hd$thal = factor(hd$thal, labels = c('Normal', 'Fixed', 'Reversable'))
hd$num = factor(hd$num)


# Splitting the hd into the Training set and Test set
# dataset1

# install.packages('caTools')
library(caTools)
set.seed(1)
split = sample.split(hd$num, SplitRatio = 0.7)
train_1 = subset(hd, split == TRUE)
test_1 = subset(hd, split == FALSE)


# dataset2(Scaled)
train_2 = train_1
train_2[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')] = scale(train_1[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')])

test_2 = test_1
test_2[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')] = scale(test_1[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')])


# dataset3: all-factor dataset

hd1 = hd
hd1$age = ifelse((hd1$age >= 53) & (hd1$age < 68), '[53, 68)', 'other') # group age
hd1$age = factor(hd1$age)
hd1$age<- relevel(hd1$age, ref = "other")

levels(hd1$cp)[c(1, 2, 3)] <- 'Non.Asymptomatic' # group cp
hd1$cp<- relevel(hd1$cp, ref = "Non.Asymptomatic")

hd1$trestbps = ifelse(hd1$trestbps > 142, '>142', 'other') # group trestbps
hd1$trestbps = factor(hd1$trestbps)
hd1$trestbps<- relevel(hd1$trestbps, ref = "other")

hd1$chol <- cut(hd1$chol, breaks=3, right=F) # group trestbps

levels(hd1$Restecg)[c(2, 3)] <- 'Abnormal' # aggregate restecg
hd1$Restecg<- relevel(hd1$Restecg, ref = "Abnormal")

hd1$thalach = ifelse(hd1$thalach>(220-hd$age), 'Abnormal', 'Normal') # compare thalach with 220-age
hd1$thalach = factor(hd1$thalach)

levels(hd1$slope)[c(2, 3)] <- 'Non.Up' # group slope

levels(hd1$thal)[c(2, 3)] <- 'Abnormal' # group thal


train_3 = subset(hd1, split == TRUE)
test_3 = subset(hd1, split == FALSE)

# scale oldpeak and ca
train_3[, c('oldpeak', 'ca')] = scale(train_3[, c('oldpeak', 'ca')])
test_3[, c('oldpeak', 'ca')] = scale(test_3[, c('oldpeak', 'ca')])




# dataset4: dataset1 - column chol and fbs
train_4 = subset(train_1, select=-c(chol,fbs))
test_4 = subset(test_1, select=-c(chol,fbs))

# dataset5: dataset2 - column chol and fbs
train_5 = subset(train_2, select=-c(chol,fbs))
test_5 = subset(test_2, select=-c(chol,fbs))

# dataset6: dataset3 - column chol and fbs
train_6 = subset(train_3, select=-c(chol,fbs))
test_6 = subset(test_3, select=-c(chol,fbs))





