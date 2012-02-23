#
#  Reads the body fat data set from the bodyfat.mat file.
#

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

targettest <- test[c("pctbodyfat")]
targettest <- as.matrix(targettest)
  
cols1test <- as.matrix(cols1test)
cols1testwithone <- cbind(seq(1,1,length.out=length(cols1test)), cols1test)

y <-solve(t(cols1testwithone)%*%cols1testwithone)%*%t(cols1testwithone)%*%targettest
a <- solve(t(cols1testwithone)%*%cols1testwithone)
b <- t(cols1testwithone)%*%target 