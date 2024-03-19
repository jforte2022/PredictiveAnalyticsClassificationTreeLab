#####################################################
## Chapter 9 - Classification and Regression Trees ## 
#####################################################

#NOTE: Prepared with R version 3.6.0

#set the working directory to appropriate folder on your machine, so as to 
#access the data files.

#load the required libraries/packages for this chapter
#Install the package(s) below once on your machine. To do so, uncomment the 
#install.packages line(s) below.

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("xgboost")
#install.packages("Matrix")
#install.packages("caret")
#install.packages("gains")

library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)
library(caret)
library(gains)

## Problem 9.1 Competitive Auctions on eBay.com. 
##The file eBayAuctions.csv contains information on 1972 auctions that 
##transacted on eBay.com during May-June 2004. The goal is to use these data to
##build a model that will classify auctions as competitive or noncompetitive.
##A competitive auction is defined as an auction with at least two bids placed
##on the item auctioned. The data include variables that describe the item 
##(auction category), the seller (his/her eBay rating), and the auction terms 
##that the seller selected (auction duration, opening price, currency, 
##day-of-week of auction close). In addition, we have the price at which the 
##auction closed. The task is to predict whether or not the auction will be 
##competitive.
##Data Preprocessing. Convert variable Duration into a categorical variable. 
##Split the data into training (60%) and validation (40%) datasets.

#load the data
ebay.df <- read.csv("eBayAuctions.csv")
lapply(ebay.df, class)

#Q1: convert variables "Duration" and "Competitive" to categorical type
ebay.df$Duration <- as.factor(ebay.df$Duration)
ebay.df$Competitive. <- as.factor(ebay.df$Competitive.)

#partition the data into train (60%) and validation (40%) sets
#set the seed for the random number generator for reproducing the partition.
set.seed(1)  
train.index <- sample(c(1:dim(ebay.df)[1]), dim(ebay.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(ebay.df)[1]), train.index)  
#Q2: complete the following codes to create training and validation datasets.
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

##9.1.a Fit a classification tree using all predictors, using the best-pruned 
##tree. To avoid overfitting, set the minimum number of records in a terminal 
##node to 50 (in R: minbucket = 50). Also, set the maximum number of levels to 
##be displayed at seven (in R: maxdepth = 7). Write down the results in terms 
##of rules. 

#Q3: fit a classification tree model (Competitive. is the outcome variable, and all other variables are predictors). 
#Complete the code below.Keep the attribute setting sor minbucket and maxdepth as they are.
tr <- rpart(Competitive. ~ ., data = train.df, cp = 0.00001, minbucket = 50, maxdepth = 7)

#print cp of the newly fitted model.  
options(scipen = 10)
printcp(tr)

# CP nsplit rel error  xerror     xstd
# 1 0.299618      0   1.00000 1.00000 0.032605
# 2 0.083969      1   0.70038 0.71565 0.030542
# 3 0.080153      2   0.61641 0.68702 0.030201
# 4 0.057252      3   0.53626 0.56107 0.028366
# 5 0.032443      4   0.47901 0.51145 0.027476
# 6 0.000010      5   0.44656 0.45802 0.026396


#Q4: annotate the following two lines of codes. 
#The following line prunes the tr tree model and stores it in pfit.
pfit<- prune(tr, cp = tr$cptable[which.min(tr$cptable[,"xerror"]),"CP"])

#The following line of code returns the min cp.
tr$cptable[which.min(tr$cptable[,"xerror"]),"CP"]

#Q6: Based on the above calculation, which number of splits indicates the minimum complexity parameters?
# The optimal number of splits is 5.

prp(tr)
t(t(names(ebay.df)))

#which are the most important variables
t(t(tr$variable.importance))

#set of rules
tr
#Q7: based on the above rules, which of the following rules are Incorrect?  


##Q8: Is this model practical for predicting the outcome of a new auction? Critically think about each of 
#the predictor variables in comparison to the future dataset. If you find any issues, describe possible actions to address the issue
# The tr model will be overfit to the training data and will not perform well predicting new data. The pruned model will be better.
 