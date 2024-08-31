df<-read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/1. ST Academy - Crash course and Regression files/House_Price.csv", header = TRUE)

str(df)

summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall, data=df)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))


# Observations
# n_hot_rooms and rainfall has outliers
# n_hos_beds has missing values
# bus_term is a useless variable
# Crime_rate has some other functional relationship with price


quantile(df$n_hot_rooms, 0.99)
uv<-quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv

summary(df$n_hot_rooms)

lv<- 0.3*quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall<lv]<-lv

summary(df$rainfall)

mean(df$n_hos_beds)
mean(df$n_hos_beds, na.rm=TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds, na.rm=TRUE)

summary(df$n_hos_beds)


pairs(~price+crime_rate, data=df)
plot(df$crime_rate, df$price)

df$crime_rate=log(1+df$crime_rate)
df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4

plot(df$crime_rate, df$price)

df2 <- df[, -7:-10]
df <- df2
rm(df2)

df <- df[, -14]

install.packages("fastDummies")
library(fastDummies)

# As dummies package is removed from new version. I have installed it using browser

df<-dummy_cols(df)
df<-df[, -9]                #removing single column using index
df<-df[, -c(11, 15, 19)]    #removing multiple columns using indeces

cor(df)
round(cor(df), 2)

df<-df[, -12]


###        Linear Regression        #### 
##        Simple Linear Regression    ##

simple_model<-lm(price~room_num, data=df)
summary(simple_model)

plot(df$room_num, df$price)
abline(simple_model)

#Multiple regression model
multiple_model<-lm(price~., data=df)         #price is dependent variable and . indicates all remaining variable in dataset
summary(multiple_model)

## Split test and train data
install.packages("caTools")
library(caTools)

set.seed(0)    #if we don't set seed to any constant number then it will split randomly each time; while set a constant no. in seed that same no. set on onother machine then also splitting will same but if constant changed the splitting will be changed but not random whenever run the code.
split=sample.split(df, SplitRatio=0.8)   #this function splits data in true(training_set i.e.80%) and false format
training_set=subset(df, split==TRUE)
test_set=subset(df, split==FALSE)
#Train the linear model
lm_a=lm(price~., data=training_set)

#predict the y values based on train and test data
train_a=predict(lm_a, training_set)
test_a=predict(lm_a, test_set)

#find the mean squared error
mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)
#As we discussed in theory lectures training set mean squared error is less than test set mean squared error


##### Subset selection (Best subset selection, forward step-wise selection, backward step-wise selection)
#the best model choosing on the basis of adjusted r square value. as it more our model is good
install.packages("leaps")
library(leaps)
lm_best=regsubsets(price~., data=df, nvmax=15)       #nvmax is increases the limit of predictor variable. If we not specify this argument it will run till max 8 predictors
summary(lm_best)
summary(lm_best)$adjr2         #find adjusted r squared value
which.max(summary(lm_best)$adjr2)        #find maximum adjusted r square value
coef(lm_best, 8)                         # find beta values

#forward step-wise selection
lm_forward=regsubsets(price~., data=df, nvmax=15, method='forward')          #add one more attribute method='forward'
summary(lm_forward)
which.max(summary(lm_forward)$adjr2)
coef(lm_forward, 8)

#backward step-wise selection
lm_backward=regsubsets(price~., data=df, nvmax=15, method='backward')
summary(lm_backward)
which.max(summary(lm_backward)$adjr2)
coef(lm_backward, 8)


#Train the model using shrinkage methods(ridge and lasso these 2 are shrinkage methods)
###         Ridge regression        ###
# for ridge regression we need glmnet package
# also we need to segregate our data into independent variable and dependent variable
install.packages("glmnet")
library(glmnet)
x=model.matrix(price~ ., data=df)[, -1]      #all independent variables
y=df$price

# create a grid of lambdas and find one which has minimum error
grid = 10^seq(10, -2, length=100)      #100 lambda values are created with range of 10^10 to 10^-2
grid
# create ridge model
# alpha =0 for Ridge regression
# alpha =1 for Lasso regression
lm_ridge=glmnet(x, y, alpha=0, lambda=grid)
summary(lm_ridge)
# glmnet has in built cross validation functionality for lambda value
cv_fit=cv.glmnet(x, y, alpha=0, lambda=grid)
plot(cv_fit)
# Optimum lambda for which error is minimum
opt_lambda=cv_fit$lambda.min
#check correspondin r squared value for optimum value of lambda
tss=sum((y-mean(y))^2)
y_a=predict(lm_ridge, s=opt_lambda, newx=x)
rss=sum((y-y_a)^2)
# r squared value
rsq=1-rss/tss
rsq       # So this value we can compare with other model for checking the best model

####           Lasso Regression           ####
lm_lasso=glmnet(x, y, alpha=1, lambda=grid)
cv_fit=cv.glmnet(x, y, alpha=1, lambda=grid)
plot(cv_fit)
opt_lambda_lasso=cv_fit$lambda.min
tss_l=sum((y-mean(y))^2)
y_a_l=predict(lm_lasso, s=opt_lambda_lasso, newx=x)
rss_l=sum((y-y_a_l)^2)
rsq_l=1-rss/tss
rsq_l
