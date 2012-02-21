require(ggplot2)
qplot(rnorm(10000,-1,1))

plotGauss<-function(x,mu,sig)
{
  factor<-1/(sig*2*pi)
  exponent<-((x-mu)^2)/(2*sig^2)
  r<-factor*exp(-exponent)
  return(r)
}
x<-seq(-10,10,0.1)
a<-sapply(x,plotGauss,-1,1)
b<-sapply(x,plotGauss,0,2)
c<-sapply(x,plotGauss,2,3)
plot(x,a)
plot(x,b)
plot(x,c)

N<-100
z<-matrix(c(rnorm(N*2)),2,N)
mu<-matrix(replicate(200,1),2,N)
sigma<-matrix(c(0.3,0.2,0.2,0.2),2,2)
L<-t(chol(sigma))
y<-mu+(L%*%z)
qplot(y[1,],y[2,])+geom_bin2d(aes(fill=log2(..count..)),binwidth=c(0.05,0.05))
muML<-matrix(c(1/N*sum(y[1,]),1/N*sum(y[2,])),2,1)
mu1=replicate(N,muML[1,])
mu2=replicate(N,muML[2,])

sigmafunc<-function(x,mu,N){
1/N*sum((x-mu)%*%t((x-mu)))
}

mlsigma<-function(x,mu,N){
  1/N*(sum(matrix((x-mu),1,100)%*%t(matrix(x-mu),1,100))
}
mlsigma(y[1,],mu1,N)