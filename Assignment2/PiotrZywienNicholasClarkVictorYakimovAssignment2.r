#First we load the dataset, give it reasonable column names, and split it up into a training and a testing subset.

rm(list=ls())
setwd('~/github/StatML/Assignment2')
load('./Data/bodyfat.RData')
require(MASS)
colnames(data)<-c('density','bodyFatPercentage','Age','Weight',
                  'Height','Neck','Chest','Abdomen','Hip','Thigh',
                  'Knee','Ankle','Biceps','Forearm','Wrist')
ridx <- sample( 1:dim( data )[1], dim( data )[1] )
trainIdx<-ridx[1:200]
testIdx<-ridx[201:length( ridx )]
train <- data[trainIdx,]
test <- data[testIdx,]

#We attempt to predict body fat using two models. The first one is based on weight, and chest, abdomen, and hip circumferences, and the second is based solely on abdominal circumference.

selection1Train<-train[,c(4,7,8,9)]
selection2Train<-train[,8]
selection1Test<-test[,c(4,7,8,9)]
selection2Test<-test[,8]
targetTrain<-train[,2]
targetTest<-test[,2]

#We make a design matrix for every selection

design1Train<-as.matrix(cbind(1,selection1Train))
design2Train<-as.matrix(cbind(1,selection2Train))
wML1<-ginv(design1Train)%*%targetTrain
wML2<-ginv(design2Train)%*%targetTrain

#Once e have found wML we can apply it to the test set
design1Test<-as.matrix(cbind(1,selection1Test))
design2Test<-as.matrix(cbind(1,selection2Test))
y1<-t(wML1)%*%t(design1Test)
y2<-t(wML2)%*%t(design2Test)
RMS1<-sqrt(sum((targetTest-y1)^2)/length(targetTest))
RMS2<-sqrt(sum((targetTest-y2)^2)/length(targetTest))
print(RMS1)
print(RMS2)

#Thus we get an RMS of 4.07 using the first model, and an RMS of 4.4 for the second model, showing the first model providing more accurate predictions. We also show the error on the train dataset
y1Train<-t(wML1)%*%t(design1Train)
y2Train<-t(wML2)%*%t(design2Train)
RMS1Train<-sqrt(sum((targetTrain-y1Train)^2)/length(targetTrain))
RMS2Train<-sqrt(sum((targetTrain-y2Train)^2)/length(targetTrain))
print(RMS1Train)
print(RMS2Train)


#Doing leave-one-out cross validation
selection1Full<-data[,c(4,7,8,9)]
selection2Full<-data[,8]
targetFull<-data[,2]
design1Full<-as.matrix(cbind(1,selection1Full))
design2Full<-as.matrix(cbind(1,selection2Full))


leave.one.out<-function(design1,design2,target){
    design1Train<-
    wML1<-ginv(design1Train)%*%target
    wML2<-ginv(design2Train)%*%target
    y1<-t(wML1)%*%t(design1Test)
    y2<-t(wML2)%*%t(design2Test)
    RMS1<-sqrt(sum((target-y1)^2)/length(target))
    RMS2<-sqrt(sum((target-y2)^2)/length(target))
    return(list(rms1=RMS1,rms2=RMS2))
}
RMSD.full<-list()
for (i in 1:nrow(data)){
    append(RMSD.full,leave.one.out(design1Full[1:nrow(data)!=i,],design2Full[1:nrow(data)!=i,],targetFull[i]))
}


#Doing MAP below:
beta <- 1
alpha <- seq(0.001,1.5,0.001)

RMS <- function(j, phitrain = design1Train, phitest=design1Test, targettrain=targetTrain, targettest=targetTest) {
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

list2 <- sapply(alpha, RMS, phitrain=design2Train, phitest=design2Test, targettrain=targetTrain, targettest=targetTest)
plot(alpha,list2,type="line", ylab="RMS", main = "Model 2: 2 dimensional pfi")  #plot alpha vs. RMS
df<-data.frame(cbind(list2,alpha))
df2<-rev(df[order(df$list2),]) #find lowest alpha value
alpha2 <- (df2[min(df2$list2),]$alpha)
RMS2_MAP <- (df2[min(df2$list2),]$list)



#Linear Discriminant Analysis
#We load the data KnollA, KnollB, and KnollC, and create 2-dimensional plots, while coloring by index. KnollA and KnollC

KA.test<-read.table('./Data/KnollA-test.dt')
KA.train<-read.table('./Data/KnollA-train.dt')
KB.test<-read.table('./Data/KnollB-test.dt')
KB.train<-read.table('./Data/KnollB-train.dt')
KC.test<-read.table('./Data/KnollC-test.dt')
KC.train<-read.table('./Data/KnollC-train.dt')
require(ggplot2)

#We plot the data, and color by factor
qplot(KA.train$V1,KA.train$V2,color=as.factor(KA.train$V3))
qplot(KB.train$V1,KB.train$V2,color=as.factor(KB.train$V3))
qplot(KC.train$V1,KC.train$V2,color=as.factor(KC.train$V3))
#KA.train and KC.train are very similar;
# KA.train$V1-KC.train$V1*100 = 0
# KA.train$V2-KC.train$V2 = 0

#Using the lda function from the MASS library:
require(MASS)

# covarA<-cov(cbind(KA.train$V1,KA.train$V2)) #covar for A we don't actually use. 
ldaa<-lda(as.factor(V3) ~ ., data = KA.train) #priors=0.5
predA<-which(predict(ldaa,KA.test)$posterior[,2]>0.5) #indices where class==1
sum(KA.test$V3[predA]==1)/length(which(KA.test$V3==1)) #accuracy=98%


ldab<-lda(as.factor(V3) ~ ., data = KB.train)
predB<-which(predict(ldab,KB.test)$posterior[,2]>0.5) #indices where class==1
sum(KB.test$V3[predB]==1)/length(which(KB.test$V3==1)) #accuracy=58% Hey, it's better than half.


ldac<-lda(as.factor(V3) ~ ., data = KC.train)
predC<-which(predict(ldac,KC.test)$posterior[,2]>0.5) #indices where class==1
sum(KC.test$V3[predC]==1)/length(which(KC.test$V3==1)) #accuracy=98%

#K-nearest neighbor
knn<-function(test,metric,train,k){
  
  if (metric=='euclidian'){
    distance<-function(x,y){
      return( sqrt((x[1]-y[1])^2+(x[2]-y[2])^2) )
    }
  }
  
  if (metric=='alternative'){
    distance<-function(x,y){
      M=matrix(c(100,0,0,1),2,2)
      return(norm(M%*%x-M%*%y)) #f'real?
    }
  }
  trainMat<-as.matrix(cbind(train$V1,train$V2))
  d<-vector()
  class<-vector()
  i=1
  while(i<=nrow(trainMat)) {
    d<-append(d,(distance(x=trainMat[i,],y=test[c(1,2)])))
    class<-append(class,train[i,]$V3)
    i=i+1
  }
  map<-data.frame(cbind(d,class))
  map<-map[order(map$d),]
  
  if(sum(map$class[1:k])>0){
    return(1)
  }
  else{
    return(-1)
  }
  
}

knnAeuclidian<-apply(X=KA.test,MARGIN=1,FUN=knn,metric='euclidian',train=KA.train,k=3)
length(which(knnAeuclidian==KA.test[,3]))/length(KA.test[,3])
knnAalternative<-apply(X=KA.test,MARGIN=1,FUN=knn,metric='alternative',train=KA.train,k=3)
length(which(knnAalternative==KA.test[,3]))/length(KA.test[,3])

knnBeuclidian<-apply(X=KB.test,MARGIN=1,FUN=knn,metric='euclidian',train=KB.train,k=3)
length(which(knnBeuclidian==KB.test[,3]))/length(KB.test[,3])
knnBalternative<-apply(X=KB.test,MARGIN=1,FUN=knn,metric='alternative',train=KB.train,k=3)
length(which(knnBalternative==KB.test[,3]))/length(KB.test[,3])

knnCeuclidian<-apply(X=KC.test,MARGIN=1,FUN=knn,metric='euclidian',train=KC.train,k=3)
length(which(knnCeuclidian==KC.test[,3]))/length(KC.test[,3])
knnCalternative<-apply(X=KC.test,MARGIN=1,FUN=knn,metric='alternative',train=KC.train,k=3)
length(which(knnCalternative==KC.test[,3]))/length(KC.test[,3])

kAaccE <- vector()
kBaccE <- vector()
kCaccE <- vector()

kAaccA <- vector()
kBaccA <- vector()
kCaccA <- vector()

KAY <- 3

while( KAY < 10 )
{

knnAeuclidian<-apply(X=KA.test,MARGIN=1,FUN=knn,metric='euclidian',train=KA.train,k=KAY)
kAaccE<-append( kAaccE, length(which(knnAeuclidian==KA.test[,3]))/length(KA.test[,3]) )
knnAalternative<-apply(X=KA.test,MARGIN=1,FUN=knn,metric='alternative',train=KA.train,k=KAY)
kAaccA<-append( kAaccA, length(which(knnAalternative==KA.test[,3]))/length(KA.test[,3]) )

# k=3
# e:98%
# a:98%
# k=5
# e:97%
# a:98%
# k=7
# e:96%
# a:98%
# k=9
# e:97%
# a:98%

knnBeuclidian<-apply(X=KB.test,MARGIN=1,FUN=knn,metric='euclidian',train=KB.train,k=KAY)
kBaccE<-append( kBaccE, length(which(knnBeuclidian==KB.test[,3]))/length(KB.test[,3]) )
knnBalternative<-apply(X=KB.test,MARGIN=1,FUN=knn,metric='alternative',train=KB.train,k=KAY)
kBaccA<-append( kBaccA, length(which(knnBalternative==KB.test[,3]))/length(KB.test[,3]) )

# k=3
# e:80%
# a:81%
# k=5
# e:81%
# a:84%
# k=7
# e:81%
# a:84%
# k=9
# e:77%
# a:85%

knnCeuclidian<-apply(X=KC.test,MARGIN=1,FUN=knn,metric='euclidian',train=KC.train,k=KAY)
kCaccE<-append( kCaccE, length(which(knnCeuclidian==KC.test[,3]))/length(KC.test[,3]) )
knnCalternative<-apply(X=KC.test,MARGIN=1,FUN=knn,metric='alternative',train=KC.train,k=KAY)
kCaccA<-append( kCaccA, length(which(knnCalternative==KC.test[,3]))/length(KC.test[,3]) )

# k=3
# e:68%
# a:98%
# k=5
# e:66%
# a:98%
# k=7
# e:58%
# a:97%
# k=9
# e:54%
# a:97%

KAY = KAY + 2

}

plot(seq(3,9,2),kAaccE,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: Set A, Euclidian metric")
plot(seq(3,9,2),kAaccA,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: Set A, Alternative metric")

plot(seq(3,9,2),kBaccE,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: Set B, Euclidian metric")
plot(seq(3,9,2),kBaccA,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: Set B, Alternative metric")

plot(seq(3,9,2),kCaccE,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: Set C, Euclidian metric")
plot(seq(3,9,2),kCaccA,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: Set C, Alternative metric")


pdf( "kA.pdf" )
plot(seq(3,9,2),kAaccE,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: KNOLL-A, Euclidian")
grid()
dev.off()
pdf( "kB.pdf" )
plot(seq(3,9,2),kBaccE,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: KNOLL-B, Euclidian")
grid()
dev.off()
pdf( "kC.pdf" )
plot(seq(3,9,2),kCaccE,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: KNOLL-C, Euclidian")
grid()
dev.off()

pdf( "kAa.pdf" )
plot(seq(3,9,2),kBaccA,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: KNOLL-B, L2-norm")
grid()
dev.off()

pdf( "kBa.pdf" )
plot(seq(3,9,2),kAaccA,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: KNOLL-A, L2-norm")
grid()
dev.off()

pdf( "kCa.pdf" )
plot(seq(3,9,2),kCaccA,type="line", xlab="k", ylab="Accuracy", main = "Relationship between k and accuracy: KNOLL-C, L2-norm")
grid()
dev.off()
