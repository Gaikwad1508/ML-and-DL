#######      SIMPLE DECISION TREES      ##########

#Import dataset
movie=read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/3. ST Academy - Decision Trees resource files/Movie_regression.csv")
View(movie)

# Data preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken, na.rm=TRUE)               #Some na values are present in Time_taken feature so replacing na with avg

# Test-Train split
library("caTools")
set.seed(0)
split=sample.split(movie, SplitRatio=0.8)
train=subset(movie, split==TRUE)
test=subset(movie, split==FALSE)

# Train the model
install.packages('rpart')
install.packages('rpart.plot')
library('rpart')
library('rpart.plot')

# Run regression tree model on train set
regtree=rpart(formula=Collection~., data=train, control=rpart.control(maxdepth=3))


#Plot the decision tree
rpart.plot(regtree, box.palette="RdBu", digits=-3)          # here if digits is 0 then the data in the scintific format like power of e and if want to increse the power of 10 then you can increase the digits

#Predict value at any point
test$pred<-predict(regtree, test, type="vector")         # Here our response variable is continuous so we used type="vector", if response variable is class type then we will use type="class"
MSE2<-mean((test$pred-test$Collection)^2)       #The accuracy is check by MSE(mean squared error) term , as it low our model have accuracy

#####  Tree Pruning    #####
fulltree<-rpart(formula=Collection~., data=train, control=rpart.control(cp=0))                 # Here cp is control parameter which means tuning parameter here cp is set to 0 means no pruning(created full tree without any pruning)
rpart.plot(fulltree, box.palette="RdBu", digits=-3)

# check the cp value for which the crossvalidation error(xerror) is minimum and then prune the tree
printcp(fulltree)      #checks the possible cp values for given tree
plotcp(regtree)

mincp<-regtree$cptable[which.min(regtree$cptable[,"xerror"]), "CP"]      #find cp value for which xerror is minimum

prunedtree <- prune(fulltree, cp=mincp)
rpart.plot(prunedtree, box.palette="RdBu", digits=-3)

test$fulltree <- predict(fulltree, test, type="vector")
MSE2full <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type="vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)


##################         Classification Tree             ###############

# Import dataset
df<- read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/3. ST Academy - Decision Trees resource files/Movie_classification.csv")
View(df)

#Data Preprocessing
summary(df)
df$Time_taken[is.na(df$Time_taken)]<-mean(df$Time_taken, na.rm=TRUE)

# Test-Train Split
library("caTools")
set.seed(0)
split=sample.split(movie, SplitRatio=0.8)
trainc=subset(df, split==TRUE)

testc=subset(df, split==FALSE)


# install required packages
library(rpart)
library("rpart.plot")

#Run classification tree model on train set
classtree<-rpart(formula=Start_Tech_Oscar~., data=trainc, method='class', control=rpart.control(maxdepth=3))         #for classification tree keep method="class"

# Press F1 on rpart fot help on this function

#Plot the decision Tree
rpart.plot(classtree, box.palette="RdBu", digits=-3)

#Predict value at any point
testc$pred<-predict(classtree, testc, type="class")

table(testc$Start_Tech_Oscar, testc$pred)

(41+24)/(41+10+38+24)        # Accuracy score


################  Ensemble Method   ###############

# Bagging
# create the model using regression(it is by default). 
install.packages("randomForest")
library("randomForest")
set.seed(0)

bagging <- randomForest(formula=Collection~., data=train, mtry=17)         #mtry is no. of predictor to make different bags with.
test$bagging <- predict(bagging, test)
MSE2bagging <- mean((test$bagging - test$Collection)^2)

# Random Forest
randomfor <- randomForest(Collection~., data=train, ntree=500)       #here ntree is maximum no. of trees can be created to train the model. we can also specify mtry(no. of variables want in subset) here as this is a regression model so it takes P/3 by default value(i.e. 17/3 no. of variables)
#Predict Output
test$random <- predict(randomfor, test)
MSE2random <- mean((test$random-test$Collection)^2)

#Task(create the model using classification model. just add one more parameter to specify classification in randomForest function)

######   Gradient boosting     #########

#########################################  COMPLETE THE CODE LATER NOW IT HAS MANT ERRORS   ###############################################

'''install.packages("gbm")       #Installing gradient boosting model
library("gbm")
set.seed(0)
boosting=gbm(Collection~., data=train, distribution = "gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose = F)
summary(df$X3D_available)

# For regression purpose use distribution="gaussian".
# For classification purpose use distribution="bernoulli"
# Interaction depth is the max depth of trees created using residual of previous tree
# Shrinkage is lambda which is the rate of learning and how much is less the model will learn more well
# verbose = F is used to give just final output, not gives output at all the stages or at each iterations. 

describe(df) '''
############################################################################################################################################
install.packages("adabag")
library(adabag)

trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)    #For classification model we need response variable as categorical so convert it as categorical first
                                                                 #For regression model we require response variable as numeric type
adaboost <- boosting(Start_Tech_Oscar~., data=trainc, boos=TRUE)     #boos is used to get bootsraped sample to train the next tree it will have more weightage the misclassified cases.

predada <- predict(adaboost, testc)
table(predada$class, testc$Start_Tech_Oscar)
74/113

t1 <- adaboost$trees[[1]]
plot(t1)
text(t1, pretty = 100)

########         XG Boosting         ##########
# For performing XG boost in r we have to convert our data in the format dematrix
install.packages("xgboost")
library(xgboost)

# Convert the all values from train dataset as dependent and independent in the form of boolean
trainY = trainc$Start_Tech_Oscar == "1"
trainX = model.matrix(Start_Tech_Oscar~.-1, data=trainc)