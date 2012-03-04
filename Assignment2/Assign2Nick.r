#  Reads the body fat data set from the bodyfat.mat file.
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

train1 <- train[c("weight","chest","abdomen","hip")]
train1 <- as.matrix(train1)
train1 <- cbind(1, train1)
train2 <- train[c("abdomen")]
train2 <- as.matrix(train2)
train2 <- cbind(1,train2)

test1 <- test[c("weight","chest","abdomen","hip")]
test1 <- as.matrix(test1)
test1 <- cbind(1, test1)
test2 <- test[c("abdomen")]
test2 <- as.matrix(test2)
test2 <- cbind(1, test2)

t_train1 <- train[c("pctbodyfat")]
t_train1 <- as.matrix(t_train1)
t_train2 <- train[c("pctbodyfat")]
t_train2 <- as.matrix(t_train2)
t_test <- test[c("pctbodyfat")]
t_test <- as.matrix(t_test)

remove(train, test, data, ridx)

## calculate the weights with identity basis function using function (3.15)
#w_ML1 <-ginv(t(train1)%*%train1)%*%t(train1)%*%t_train1
w_ML1 <- ginv(train1)%*%t_train1
## calculate prediction values
pred1 <- test1%*%w_ML1

##calculate RMS for first specification
RMS1_ML <- sqrt(1/length(pred1)*sum((t_test-pred1)^2))

## Deternime RMS for second procedure
#w_ML2 <-ginv(t(train2)%*%train2)%*%t(train2)%*%t_train2
w_ML2 <- ginv(train2)%*%t_train2

## calculate prediction values
pred2<- test2%*%w_ML2

##calculate RMS for first specification
RMS2_ML <- sqrt(1/length(pred2)*sum((t_test-pred2)^2))

##################
## Question 1.2 ##
##################
# Calculate covariance matrix for test dataset from 1st model using eq. (3.54) for given alpha value
beta <- 1
alpha <- seq(0.001,1.5,0.001)

RMS <- function(j, phitrain = train1, phitest=test1, targettrain=t_train1, targettest=t_test) {
  I <- diag(length(phitrain[1,]))
  S_N_inv <- j*I+beta*t(phitrain)%*%phitrain
  m_N <- beta*ginv(S_N_inv)%*%t(phitrain)%*%targettrain  #Calculate mean, equation (3.53)
  pred3 <- t(m_N)%*%t(phitest) #Use (3.31) to calculate predictions, note that the lowercase fi is transposed
  return (sqrt(1/length(pred3)*sum((t(targettest)-pred3)^2)))     #calculate RMS alpha=0.5, RMS 4.51
}
list1 <- sapply(alpha, RMS)
plot(alpha,list1,type="line", ylab="RMS", main="Model 1: 5 dimensional pfi")  #plot alpha vs. RMS
df<-data.frame(cbind(list1,alpha))
df1<-rev(df[order(df$list1),]) #find lowest alpha value
alpha1 <- (df1[min(df1$list1),]$alpha)
RMS1_MAP <- (df1[min(df1$list1),]$list)

list2 <- sapply(alpha, RMS, phitrain=train2, phitest=test2, targettrain=t_train2, targettest=t_test)
plot(alpha,list2,type="line", ylab="RMS", main = "Model 2: 2 dimensional pfi")  #plot alpha vs. RMS
df<-data.frame(cbind(list2,alpha))
df2<-rev(df[order(df$list2),]) #find lowest alpha value
alpha2 <- (df2[min(df2$list2),]$alpha)
RMS2_MAP <- (df2[min(df2$list2),]$list)
