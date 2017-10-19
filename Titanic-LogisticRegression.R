#Predicting Titanic survival rate using logistic regression
library(caret)
library(mice)

#Load the training data
setwd('D:\\Venkat\\Villanova\\Data Science Projects\\Titanic-LogisticRegression')
dataset_full = read.csv('train.csv')

#Reset the order of features in the dataset
dataset_full = dataset_full[c(1,3,4,5,6,7,8,9,10,11,12,2)]

#Pick the features of interest
dataset = dataset_full[,-c(1,3,8,10,11)]

#Redefine the modeling types of the features
str(dataset)
dataset$Pclass = factor(x = dataset$Pclass)
dataset$SibSp = factor(x = dataset$SibSp)
dataset$Parch = factor(x = dataset$Parch)
dataset$Survived = factor(dataset$Survived)

#Check for missing data
md.pattern(dataset)
impute_dataset = mice(data = dataset,
                      m = 1,
                      maxit = 3)
imputed_dataset = complete(impute_dataset,1)

#Scale continuous features
str(imputed_dataset)
imputed_dataset$Age = scale(imputed_dataset$Age)
imputed_dataset$Fare = scale(imputed_dataset$Fare)
