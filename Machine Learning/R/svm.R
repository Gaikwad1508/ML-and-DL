#########  Support Vector Machine   ##########

#Import data from csv file. Note a forward slash "/" is used in file location.
movie <- read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/4. ST Academy - SVM resource files/Movie_classification.csv", header = TRUE)

# Data Preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken, na.rm = TRUE)

# Test-Train Split
library('caTools')
set.seed(0)
split = sample.split(movie, SplitRatio = 0.8)
trainc = subset(movie, split == TRUE)
testc = subset(movie, split == FALSE)

#### For Classification 

# We use same function to train the model for svm in regression and classification
# it internally checks type of response variable and performs the regression or classification.
# Here we have response variable 0 and 1 values that is numeric, but we want to perform classification so we convert it as factor

trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)
testc$Start_Tech_Oscar <- as.factor(testc$Start_Tech_Oscar)

# Import relevant library e1071
install.packages('e1071')
library(e1071)

svmfit <- svm(Start_Tech_Oscar~., data = trainc, kernel = 'linear', cost = 1, scale = TRUE)
# kernel = 'linear' is same as support vector classifier also linear support vector machine
# cost =1 , it is a hyper parameter we use it for control the width of margin and we allow some of the points to be misclassified.
# scale  Ture, it means we are scaling all the variables present in the dataset. scaling means make the mean 0 and std deviation 1, because svm is scale sensitive

summary(svmfit)

# Predicting on test set
ypred <- predict(svmfit, testc)
table(predict = ypred, truth = testc$Start_Tech_Oscar)
(30+36)/(30+21+20+36)

# To check the support vectors
svmfit$index

# Finding best value of C / Tuning the hyperparameter
set.seed(0)
tune.out = tune(svm, Start_Tech_Oscar~., data = trainc, kernel = "linear", range = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))

bestmod = tune.out$best.model
summary(bestmod)

ypredL = predict(bestmod, testc)
table(predict = ypredL, truth = testc$Start_Tech_Oscar)

# Polynomial Kernel
svmfitP = svm(Start_Tech_Oscar~., data = trainc, kernel = "polynomial", cost = 1, degree = 2)

#Hyperparameter Tuning
tune.outP = tune(svm, Start_Tech_Oscar~., data = trainc, cross = 4, kernel = "polynomial", ranges = list(cost = c(0.001, 0.1, 1, 5, 10), degrees = c(0.5, 1, 2, 3, 5)))
bestmodP = tune.outP$best.model
summary(bestmodP)
ypredP = predict(bestmodP, testc)
table(prdict = ypredP, truth = testc$Start_Tech_Oscar)

# Radial kernel
svmfitR = svm(Start_Tech_Oscar~., data = trainc, kernel = "radial", gamma = 1, cost = 1)

tune.outR = tune(svm, Start_Tech_Oscar~., data = trainc, kernel = "radial", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.01, 0.1, 0.5, 1, 3, 10, 50)), cross = 4) 
summary(tune.outR)
bestmodR = tune.outR$best.model
summary(bestmodR)

ypredR = predict(bestmodR, testc)
table(predict = ypredR, truth = testc$Start_Tech_Oscar)
58/(58+31+18)

######  Regression #########

# Import data from csv file. Note forward slash '/' is used in file location.
df <- read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/4. ST Academy - SVM resource files/Movie_regression.csv", header = TRUE)

#Data preprocessing
summary(df)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm = TRUE)

# Test-train split
library(caTools)
set.seed(0)
split = sample.split(movie, SplitRatio = 0.8)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

# Importing relevant library e1071
library(e1071)
svmfitR = svm(Collection~., data = train, kernel = "linear", cost = 0.01, scale = TRUE)
summary(svmfitR)

# Predicting on test set
ypredR = predict(svmfitR, test)
ypredR

mse <- mean((ypredR-test$Collection)^2)
