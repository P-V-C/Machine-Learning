require(ggplot2)
rm(list=ls())
setwd('~/github/StatML/Assignment3')
nDataTrain<-read.table('./Data/sincTrain50.dt')
qplot(nDataTrain[,1],nDataTrain[,2])+geom_line()

#dim(matrix(runif(100),50,2)%*%matrix(1,2,100))

sigma<-function(u){u/(1+abs(u))}

#initialize
x_init<-rbind(1,nDataTrain[,1]) #one col per training exmaple
w1<-matrix(runif(2),1,2) #w^(1)
w2<-matrix(runif(2),1,2) #w^(2)

#forward propagation
z2<-w1%*%x_init
a2<-rbind(1,sapply(z2,sigma))
z3<-w2%*%a2
y<-sapply(z3,sigma)

#back propagation


#2.1 Model selection using grid search:
rm(list=ls())
require(ggplot2)
require(kernlab)
setwd('~/github/StatML/Assignment3')
kc100<-read.table('./Data/knollC-train100.dt')
kc100$V3<-as.factor(kc100$V3)
kc200<-read.table('./Data/knollC-train200.dt')
kc200$V3<-as.factor(kc200$V3)
kc400<-read.table('./Data/knollC-train400.dt')
kc400$V3<-as.factor(kc400$V3)
kcTest<-read.table('../Assignment2/Data/knollC-test.dt')
C<-c(0.1,1,10,100,1000,10000)
#gamma<-seq(0.1,10,length.out=7)
gamma<-c(0.00001,0.01,0.05,1,3,7,10000)
sigma<-sapply(gamma,function(x){sqrt(1/(2*x))})
cv<-vector()
test<-vector()
cvMat<-matrix(0,length(C),length(sigma))
testMat<-matrix(0,length(C),length(sigma))
errMat<-matrix(0,length(C),length(sigma))
dimnames(cvMat)<-list(C=C,gamma=gamma)
dimnames(testMat)<-list(C=C,gamma=gamma)
dimnames(errMat)<-list(C=C,gamma=gamma)



for (i in 1:length(sigma)){
  for (j in 1:length(C)){
    svm<-ksvm(V3~.,data=kc200,kernel='rbfdot',cross=5,C=C[j],kpar=list(sigma=sigma[i]))
    cvMat[j,i]<-cross(svm)
    errMat[j,i]<-error(svm)
    prediction<-predict(svm,kcTest[,c(1,2)])
    predictionTest<-as.numeric(levels(prediction))[prediction]==kcTest[,3]
    errorRate<-1-sum(predictionTest)/length(predictionTest)
    testMat[j,i]<-errorRate
  }
}
print(testMat)
print(cvMat)
print(errMat)

#kc400 gamma=1, C=0.01, error: 0.025
#kc200 sigma=2.236 (gamma=0.1),c=1 error: 0.015
#kc100 gamma=1, c=1, error: 0.01

#2.2.1
finalModel<-ksvm(V3~.,data=kc100,kernel='rbfdot',C=1,kpar=list(sigma[4]))
plot(finalModel,data=kc100)
#points(-0.23,0,col="red")
#points(-0.5,0,col="red")
#points(0,0,col="red")
#points(0.5,0,col="red")
#points(1,0,col="red")


###2.2.2
largeC<-ksvm(V3~.,data=kc200,kernel='rbfdot',C=100,kpar=list(sigma[4]))
smallC<-ksvm(V3~.,data=kc200,kernel='rbfdot',C=0.01,kpar=list(sigma[4]))
plot(largeC,data=kc200)  #mostly unbound
plot(smallC,data=kc200)  #all bound #plot supportvectors

prediction<-predict(largeC,kcTest[,c(1,2)])
predictionTest<-prediction==kcTest[,3]
errorRate<-1-sum(predictionTest)/length(predictionTest)
print(errorRate)

prediction<-predict(smallC,kcTest[,c(1,2)])
predictionTest<-prediction==kcTest[,3]
errorRate<-1-sum(predictionTest)/length(predictionTest)
print(errorRate)


###2.2.3
require(ggplot2)
require(reshape)
testSize<-function(data,C){
  svm<-ksvm(V3~.,data=data,kernel='rbfdot',C=C,kpar=list(sigma[4]))
  vectors<-alpha(svm)[[1]]
#  print(sum(vectors!=C) / length(vectors) )
    list(totalVectors=length(alpha(svm)[[1]]),boundVectors=sum(alpha(svm)[[1]]==C),unboundVectors=sum(alpha(svm)[[1]]!=C))
}
observations<-vector()
cee<-vector()
vectors<-vector()
bound<-vector()
unbound<-vector()

for (i in list(kc100,kc200,kc400)){
  for (j in c(0.1,10,100)){
    observations<-append(observations,nrow(i))
    cee<-append(cee,j)
    vecs<-testSize(data=i,C=j)
    vectors<-append(vectors,vecs[[1]])
    bound<-append(bound,vecs[[2]])
    unbound<-append(unbound,vecs[[3]])
  }
}

df<-data.frame(observations=observations,C=cee,totalVectors=vectors,boundVectors=bound,unboundVectors=unbound)
#qplot(data=df,x=observations,y=totalVectors,color=as.factor(C),main="all vectors")
#qplot(data=df,x=observations,y=boundVectors,color=as.factor(C),main="bound vectors")
#qplot(data=df,x=observations,y=unboundVectors,color=as.factor(C),main="unbound vectors")
dfMelt<-melt(df,id.vars=c('observations','C'))
qplot(data=dfMelt,x=observations,y=value,color=as.factor(C),main="Number of Vectors")+facet_wrap(~variable)+scale_colour_discrete(name = "C")+ylab('count')+geom_line()
