# Group Project: Heart Disease Prediction
# Group 1 (DeepMind): Yingxue Long, Chengyue Wang, Zheng Yu, Fengxu Li
# -*- coding: utf-8 -*-


## Part I. Data Preprocessing

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
hd$cp = factor(hd$cp, labels = c('Typical', 'Atypical', 'Non-anginal', 'Asymptomatic'))
hd$fbs = factor(hd$fbs, labels = c('False','True'))
hd$Restecg = factor(hd$Restecg, labels = c('Normal', 'ST-T', 'Left'))
hd$exang = factor(hd$exang, labels = c('No', 'Yes'))
hd$slope = factor(hd$slop, labels = c('Upsloping', 'Flat', 'Downsloping'))
hd$thal = factor(hd$thal, labels = c('Normal', 'Fixed', 'Reversable'))
hd$num = factor(hd$num)

# Splitting the hd into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(10)
split = sample.split(hd$num, SplitRatio = 0.7)
train = subset(hd, split == TRUE)
test = subset(hd, split == FALSE)

# Feature Scaling
train_scale = train
train_scale[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')] = scale(train[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')])

test_scale = test
test_scale[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')] = scale(test[, c('age', 'trestbps', 'chol', 'thalach', 'oldpeak', 'ca')])

## Part II. Building Models

# Tree
library(tree)

tree.hd = tree(num~., train)
summary(tree.hd)

plot(tree.hd)
text(tree.hd, pretty=0)

tree.pred = predict(tree.hd,test,type="class")
table(tree.pred, test_scale$num)
mean(tree.pred == test_scale$num)

set.seed(3)
cv.hd = cv.tree(tree.hd,FUN=prune.misclass)
names(cv.hd)
cv.hd

prune.hd = prune.misclass(tree.hd,best=11)

plot(prune.hd)
text(prune.hd, pretty = 0)

tree.pred = predict(prune.hd,test,type="class")
table(tree.pred, test$num)
mean(tree.pred == test$num)


#  Logistic
logreg.fit <- glm(num~sex+cp+trestbps+slope+ca+thal,data=train,family=binomial)
summary(logreg.fit)

logreg.fit.prob=predict(logreg.fit,test,type="response")

logreg.fit.pred=rep(0,nrow(test))

logreg.fit.pred[logreg.fit.prob>.6]=1

# compute the percentage of time the prediction was correct
mean(logreg.fit.pred==test$num)

# knn


