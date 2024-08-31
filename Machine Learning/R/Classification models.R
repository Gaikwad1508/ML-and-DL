df=read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/2. ST Academy - Classification models resource files/Classification preprocessed data Python.csv", header=TRUE)

# Logistic regression with single predictor
glm.fit=glm(Sold~price, data=df, family=binomial)      #for logistic regression we have used glm function which stands for generalised linear models and also there should be a parameter family=binomial
summary(glm.fit)

# Logistic regression with multiple predictor
glm.fit=glm(Sold~ ., data=df, family=binomial)
summary(glm.fit)

#calculating probability to check whether house will sold or not.
glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]     #look first 10 probability values for sold variable
# Provide boundary condition if probability is greater than 0.5 then house will be sold else not
glm.pred=rep("No", 506)     #create array with size 506 with values "No"
glm.pred[glm.probs>0.5]="YES"

# Using predicted response value and actual response value create a confusion matrix
table(glm.pred, df$Sold)


#######         Linear Discriminant Analysis        #########
library('MASS')
lda.fit=lda(Sold~., data=df)      #create model for lda
lda.fit

lda.pred=predict(lda.fit, df)
lda.pred$posterior

lda.class=lda.pred$class          # defauld boundary condition for classes is 0.5

# create confusion matrix
table(lda.class, df$Sold)

# creating boundary condition as 0.8
sum(lda.pred$posterior[ ,1]>0.8) #gives the count of wherever the predicted probability belonging to class 1 more than 0.8.

##########       Quadratic Discriminant Analysis        #########
qda.fit=qda(Sold~., data=df)
qda.fit
qda.pred=predict(qda.fit, df)
qda.pred$posterior
qda.class=qda.pred$class
table(qda.class, df$Sold)
sum(qda.pred$posterior[ , 1]>0.8)

####### Split the data into train and test sets ########

library("caTools")
set.seed(0)
split=sample.split(df, SplitRatio=0.8)
train_set=subset(df, split==TRUE)
test_set=subset(df, split==FALSE)

####### Train the model using Logistic regression  ######

#train the model using train set
train.fit=glm(Sold~., data=train_set, family=binomial)
test.probs=predict(train.fit, test_set, type='response')     #predict probability on test set
test.pred=rep('NO', 120)             # create an array with size of test set with values of NO
test.pred[test.probs>0.5]='YES'
table(test.pred, test_set$Sold)

######  Train the model using linear discriminant analysis   ######
lda_train.fit=lda(Sold~., data=train_set, family=binomial)
lda_test.probs=predict(lda_train.fit, test_set, type='response')
lda_test.pred=rep('NO', 120)
lda_test.pred <- ifelse(lda_test.probs$posterior[,2] > 0.5, 'YES', 'NO')

table(lda_test.pred, test_set$Sold)

#########    K-Nearest Neighbors      ##############
library("class")

trainX=train_set[, -16]
testX=test_set[, -16]
trainy=train_set$Sold
testy=test_set$Sold

k=5

trainX_s=scale(trainX)
testX_s=scale(testX)

set.seed(0)
knn.pred=knn(trainX_s, testX_s, trainy, k=k)
table(knn.pred, testy)
