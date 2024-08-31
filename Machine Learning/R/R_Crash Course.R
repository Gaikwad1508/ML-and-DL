2+5
# Demo Comment
print("Hello World")

x<-2
x<-3
x
y<-c(1, 2, 3, 4, 5)
y<-1:10
x<-y<-1:10
x+y
z<-x+y
z2<-x*y
x<-10

ls()

rm(x)
remove(z2)
rm(list=ls())


# Installing packages

# http://cran.r-project.org/web/views/
#http://crantastic.org

browseURL("http://cran.r-project.org/web/views/")

install.packages("LiblineaR")

library()
search()
require("graphics")
detach("package:LiblineaR", unload=TRUE)
remove.packages("LiblineaR")

? ggplot2


# Inputting data
data()
library(help="datasets")
? iris
str(iris)        #Structure of dataset

iris
data("iris")

x1<-1:10
x2<-c(2, 5, 7, 4)
x3<-seq(5, 50, by=50)
x4<-scan()
x4

# Importing data from files

Product<-read.table("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/2. ST Academy - Classification models resource files/Product.txt", header=TRUE, sep="\t")


str(Product)

Customer<-read.csv("C:/Users/Abhishek/OneDrive/Desktop/ML and DL/Machine-Learning-Deep-Learning-in-Python-R/Data Files/2. ST Academy - Classification models resource files/Customer.csv", header=TRUE)

Customer
View(Customer)


# Barpolts

y<-table(Customer$Region)
View(y)
barplot(y)
barplot(y[order(y)], horiz=TRUE)
barplot(y[order(y)], horiz=TRUE, col="red")
barplot(y[order(y)], horiz=TRUE, col=c("red", "green", "blue", "beige"))
barplot(y[order(y)], horiz=TRUE, col=c("red", "green", "blue", "beige"), border=NA)
barplot(y[order(y)], horiz=TRUE, col=c('red', 'green', 'blue', 'beige'), border=NA, main="Freq of \n Regions")
barplot(y[order(y)], horiz=TRUE, col=c('red', 'green', 'blue', 'beige'), border=NA, main="Freq of \n Regions", xlab = "Number of Customers")

# Histogram

hist(Customer$Age)
hist(Customer$Age, breaks=5)
hist(Customer$Age, breaks=c(0, 40, 60, 100))
hist(Customer$Age, breaks=c(0, 40, 60, 100), freq=TRUE)
hist(Customer$Age, breaks=c(0, 40, 60, 100), freq=TRUE, col="blue", main = "Histogram of Age")
