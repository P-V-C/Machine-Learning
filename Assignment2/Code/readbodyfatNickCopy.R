#
#  Reads the body fat data set from the bodyfat.mat file.
#
require ('MASS')
load( '/Users/Nick/Documents/Bioinformatics/Machine Learning/Machine-Learning/Assignment2/Data/bodyfat.RData' )

ridx <- sample( 1:dim( data )[1], dim( data )[1] )
colnames(data) <- c("density", "pctbodyfat", "age", "weight", "height", "neck",
                    "chest", "abdomen", "hip", "thigh", "knee", "ankle","biceps",
                    "forearm", "wrist")

test <- data[ridx[201:length( ridx )],]
train <- data[ridx[1:200],]

# Column 2 is the target variable.
# Relevant section is 3.1 in Bishop

cols1test <- test[c("weight","chest","abdomen","hip")]
cols1test <- as.matrix(cols1test)
cols1train <- train[c("weight","chest","abdomen","hip")]
cols1train <- as.matrix(cols1train)
cols2test <- test[c("abdomen")]
cols2test <- as.matrix(cols2test)
cols2train <- train[c("abdomen")]
cols2train <- as.matix(cols2train)

targettrain <- train[c("pctbodyfat")]
targettrain <- as.matrix(targettrain)

targettest <- test[c("pctbodyfat")]
targettest <- as.matrix(targettest)

targettrain2 <- train[c("pctbodyfat")]
targettrain2 <- as.matrix(targettrain2)

targettest2 <- test[c("pctbodyfat")]
targettest2 <- as.matrix(targettest2)

cols1train <- as.matrix(cols1train)
cols1trainwithone <- cbind(seq(1,1,length.out=length(cols1train)), cols1train)

cols2train <- as.matrix(cols2train)
cols2trainwithone <- cbind(seq(1,1,length.out=length(cols2train)), cols2train)

## calculate the weights with identity basis function using function (3.15)
y <-ginv(t(cols1trainwithone)%*%cols1trainwithone)%*%t(cols1trainwithone)%*%targettrain

## add column of ones to test data for first model
cols1testwithone <- cbind(seq(1,1,length.out=length(cols1test)), cols1test)

## calculate prediction values
pred<- cols1testwithone%*%y

##calculate RMS for first specification - 3.05
RMS <- sqrt((1/length(pred)*(sum(pred-targettest))^2))

## Deternime RMS for second procedure
y2 <-ginv(t(cols2trainwithone)%*%cols2trainwithone)%*%t(cols2trainwithone)%*%targettrain2

## add column of ones to test data for first model
cols2testwithone <- cbind(seq(1,1,length.out=length(cols2test)), cols2test)

## calculate prediction values
pred2<- cols2testwithone%*%y2

##calculate RMS for first specification - 2.59
RMS2 <- sqrt((1/length(pred2)*(sum(pred2-targettest2))^2))
