require(ggplot2)
setwd(".")
#q1: plot gaussian dist with mu,sigma=(-1,1),(0,2),(2,3)
#plotGauss<-function(x,mu,sig) #functionally equivalent to dnorm in the special case of 1 dimension. 
#{
#  factor<-1/(sig*2*pi)
#  exponent<-((x-mu)^2)/(2*sig^2)
#  r<-factor*exp(-exponent)
#  return(r)
#}
x<-seq(-10,10,0.1)
a<-sapply(x,dnorm,-1,1)
b<-sapply(x,dnorm,0,2)
c<-sapply(x,dnorm,2,3)
qplot(x,a)+geom_line()+ylab("y") #gaussian with (-1,1)
ggsave(filename="1dgauss11.pdf")
qplot(x,b)+geom_line()+ylab("y") #gaussian with (0,2)
ggsave(filename="1dgauss02.pdf")
qplot(x,c)+geom_line()+ylab("y") #gaussian with (2,3)
ggsave(filename="1dgauss23.pdf")

#q2: make a 2d-gaussian with N=100
N=100
twoDgauss<-function(N,mu,sig11,sig12,sig21,sig22){
  #assumes mean is (mu,mu)
  z<-matrix(c(rnorm(N*2)),2,N)
  mu<-matrix(replicate(N,mu),2,N)
  sigma<-matrix(c(sig11,sig12,sig21,sig22),2,2)
  L<-t(chol(sigma))
  y<-mu+(L%*%z)
  return(y)
}
y<-twoDgauss(N=N,mu=1,sig11=0.3,sig12=0.2,sig21=0.2,sig22=0.2)
y10k<-twoDgauss(N=10000,mu=1,sig11=0.3,sig12=0.2,sig21=0.2,sig22=0.2)
qplot(y[1,],y[2,])+xlab("x")+ylab("y")
ggsave(filename="2dgauss100.pdf")
qplot(y10k[1,],y10k[2,])+geom_bin2d(aes(fill=log2(..count..)),binwidth=c(0.05,0.05))+xlab("x")+ylab("y")
ggsave(filename="2dgauss10k.pdf")

#q3: estimate muML and sigmaML

#method one
muML<-matrix(c(mean(y[1,]),mean(y[2,])),2)
sigma1<-function(x,mu){
(x-mu)%*%t((x-mu))
}
sigmaAll<-apply(y,2,sigma1,muML) #estimate all covariance matrices
sigma<-matrix(apply(sigmaAll,1,sum)/N,2,2) #add covariance matrices and normalize them

#method two
mu1=replicate(N,muML[1,]) #ugly way to add mu piecewise to a matrix
mu2=replicate(N,muML[2,])
mlsigma<-function(x1,x2,mu,N){ #estimate single element of covar matrix
  (1/N)*(sum(matrix((x1-mu),1,100)%*%t(matrix((x2-mu),1,100))))
}
sigma<-matrix(c(mlsigma(y[1,],y[1,],mu1,N),mlsigma(y[1,],y[2,],mu1,N),mlsigma(y[2,],y[1,],mu1,N),mlsigma(y[2,],y[2,],mu1,N)),2,2)
qplot(y[1,],y[2,])+geom_point(aes(color="red",x=muML[1],y=muML[2]))+geom_point(aes(color="blue",x=1,y=1))+xlab("x")+ylab("y")+opts(legend.position="none")
ggsave(filename="2dmuml.pdf")

#q1.5: estimate p(x1) and p(x2)

#p(x1):
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/100))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/50))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/30))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/20))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/15))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/10))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/5))
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/1))

qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/100))+xlab("marginal distribution p(x1)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1hist100.pdf")
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/75))+xlab("marginal distribution p(x1)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1hist75.pdf")
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/50))+xlab("marginal distribution p(x1)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1hist50.pdf")
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/25))+xlab("marginal distribution p(x1)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1hist25.pdf")
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/10))+xlab("marginal distribution p(x1)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1hist10.pdf")
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/5))+xlab("marginal distribution p(x1)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1hist5.pdf")


#p(x2)
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/100))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/50))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/30))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/20))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/15))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/10))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/5))
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/2))

qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/100))+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px2hist100.pdf")
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/75))+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px2hist75.pdf")
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/50))+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px2hist50.pdf")
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/25))+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px2hist25.pdf")
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/10))+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px2hist10.pdf")
qplot(y[2,],binwidth=abs((range(y[2,])[2]-range(y[2,])[1])/5))+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px2hist5.pdf")

#q1.6
#marginal distr. p(x1)=N(mu1,sig11)
qplot(y[1,],binwidth=abs((range(y[1,])[2]-range(y[1,])[1])/18))+geom_line(aes(y=sapply(X=seq(-1,3,length.out=100),FUN=dnorm,mean=1,sd=0.3)*20,x=seq(-1,3,length.out=100)),color="red")+xlab("marginal distribution p(x2)")+ylab("probability density estimate")+opts(legend.position="none")
ggsave(filename="px1histanal.pdf")

#q1.7
require(gplots)
N=100
y<-twoDgauss(N=N,mu=1,sig11=0.3,sig12=0.2,sig21=0.2,sig22=0.2)
pdf('twodhist1.pdf')
hist2d(y[1,],y[2,],nbins=c(10,10))
dev.off()
pdf('twodhist2.pdf')
hist2d(y[1,],y[2,],nbins=c(15,15))
dev.off()
pdf('twodhist3.pdf')
hist2d(y[1,],y[2,],nbins=c(20,20))
dev.off()
qplot(y[1,],y[2,])+geom_bin2d(aes(fill=log2(..count..)),binwidth=c(0.05,0.05))

N=1000
y<-twoDgauss(N=N,mu=1,sig11=0.3,sig12=0.2,sig21=0.2,sig22=0.2)
pdf('twodhist4.pdf')
hist2d(y[1,],y[2,],nbins=c(10,10))
dev.off()
pdf('twodhist5.pdf')
hist2d(y[1,],y[2,],nbins=c(15,15))
dev.off()
pdf('twodhist6.pdf')
hist2d(y[1,],y[2,],nbins=c(20,20))
dev.off()
qplot(y[1,],y[2,])+geom_bin2d(aes(fill=log2(..count..)),binwidth=c(0.05,0.05))

N=10000
y<-twoDgauss(N=N,mu=1,sig11=0.3,sig12=0.2,sig21=0.2,sig22=0.2)
pdf('twodhist7.pdf')
hist2d(y[1,],y[2,],nbins=c(10,10))
dev.off()
pdf('twodhist8.pdf')
hist2d(y[1,],y[2,],nbins=c(15,15))
dev.off()
pdf('twodhist9.pdf')
hist2d(y[1,],y[2,],nbins=c(20,20))
dev.off()
qplot(y[1,],y[2,])+geom_bin2d(aes(fill=log2(..count..)),binwidth=c(0.05,0.05))

#q1.8 sample from exp dist.
expont<-function(y,lambda){
  return(lambda*exp(-lambda*y))
}
hy<-function(z,lambda){
  return((-1/lambda)*log(1-z))
}

createLists<-function(N){ 
listExpont<-sapply(seq(0,2,length.out=N),expont,3)
listHy<-sapply(seq(0,1,length.out=N),hy,3)
return(cbind(listExpont,listHy))
}

qplot(x=createLists(1000)[,1],y=seq(0,1,length.out=1000),geom="line") #1000 samples. exp(y)
qplot(x=createLists(1000)[,2],y=seq(0,1,length.out=1000),geom="line") #1000 samples. h(y)
qplot(x=createLists(100)[,1],y=seq(0,1,length.out=100),geom="line") #100 samples. exp(y)
qplot(x=createLists(100)[,2],y=seq(0,1,length.out=100),geom="line") #100 samples. h(y)
qplot(x=createLists(10)[,1],y=seq(0,1,length.out=10),geom="line") #10 samples. exp(y)
qplot(x=createLists(10)[,2],y=seq(0,1,length.out=10),geom="line") #10 samples. h(y)

N=100 #100 samples
listExpont<-sapply(seq(0,2,length.out=N),expont,3)
listHy<-sapply(seq(0,1,length.out=N),hy,3)
qplot(listExpont,seq(0,1,length.out=N),geom="line")
qplot(listHy,seq(0,1,length.out=N),geom="line")

N=10 #10 samples
listExpont<-sapply(seq(0,2,length.out=N),expont,3)
listHy<-sapply(seq(0,0.1,length.out=N),hy,3)
qplot(listExpont,seq(0,2,length.out=N),geom="line")
qplot(listHy,seq(0,1,length.out=N),geom="line")

qplot(listExpont1000,seq(0,2,length.out=N),geom="line")
qplot(listHy1000,seq(0,0.999,length.out=N),geom="line")

getmeans<-function(N){
  mean(sapply(runif(N),hy,3))
}
means1<-sapply(seq(10,10,length.out=1000),getmeans)
means2<-sapply(seq(100,100,length.out=1000),getmeans)
means3<-sapply(seq(1000,1000,length.out=1000),getmeans)
pdf('qqplot1.pdf')
qqnorm(means1)
dev.off()

pdf('qqplot2.pdf')
qqnorm(means2)
dev.off()

pdf('qqplot3.pdf')
qqnorm(means3)
dev.off()

qplot(means1)
ggsave(filename='normeans1.pdf')
qplot(means2)
ggsave(filename='normeans2.pdf')
qplot(means3)+xlab('mu')
ggsave(filename='normeans3.pdf')

qplot(c(10,100,1000),c(sd(means1),sd(means2),sd(means3)))
qplot(c(log10(10),log10(100),log10(1000)),c(sd(means1),sd(means2),sd(means3)))

qplot(c(10,100,1000),c(mad(means1),mad(means2),mad(means3)))+xlab('L')+ylab('mean absolute deviation')
ggsave(filename='deviation.pdf')
qplot(c(log10(10),log10(100),log10(1000)),c(abs(1/3-mean(means1)),abs(1/3-mean(means2)),abs(1/3-mean(means3))))

# straight line transformation
qplot(c(10,100,1000),c(1/(mad(means1)^2),1/(mad(means2)^2),1/(mad(means3)^2)))+geom_line(color="red")+xlab('L')+ylab('1/(mean absolute deviation)^2')
ggsave(filename='madstraight.pdf')

require(pixmap)
kande1 = read.pnm("./images/kande1.pnm")
# pixel specifications:
tll = c(150, 328)  # training: lower left
tur = c(330, 264)  # training: upper right

## Show image and regions

#plot(kande1)
rect(xleft = tll[1], ybottom = 640 - tll[2], xright = tur[1], ytop= 640 - tur[2]) 
text(tll[1] + 25, 640 - tur[2] + 15, labels= "train")


# we subtract from 640 because the plot has origo at lower left corner, now. Don't worry about the subtractions and additions to the x and y parameters to the text-method. These are only there to position the text labels correctly.

## Extract regions
train.set = kande1[tur[2]:tll[2], tll[1]:tur[1]]
train = NULL
train$red = as.vector(getChannels(train.set,"red"))
train$green = as.vector(getChannels(train.set,"green"))
train$blue = as.vector(getChannels(train.set,"blue"))
train = as.data.frame(train)
means<-c(mean(train$red),mean(train$green),mean(train$blue))
rownames(means)<-c("red","green","blue")
N<-nrow(train)

sigML<- (1/N)*as.matrix(t(train)-means)%*%t((as.matrix(t(train)-means))) # calculate covar
norm3d <-function(x, mu, sigma){
  a<-(1/((2*pi)^(3/2)))*(1/sqrt(det(sigma)))  
  b<-exp(  -.5*(t(x-mu)) %*% (solve(sigma) %*% (x-mu) ) )
  return(a*b)
}
RGB = getChannels(kande1)
norm3d(c(0.95,0,0),means,sigML)

prob<-apply(RGB,c(1,2),norm3d,means,sigML) 
png('pitcher.png')
plot(pixmapGrey(prob)) 
dev.off()
probDnorm<-dnorm(RGB,mean=means,sd=sqrt(sigML))
plot(pixmapGrey(probDnorm)) #I'm pretty sure this is wrong. leaving it here for posterity
#1.10
#creating a data frame to contain the weights. rowIndex=(1,2,3...640,1,2,3,..640) 480 times in total, colIndex=(1,1,1...1,2,2,2...2...,480,480,480) 640 times in total
prob_vector <- as.vector(prob)
rowIndex<-as.vector(replicate(480,seq(1,640)))
colIndex<-as.vector(sapply(seq(1,480),replicate,n=640))
frame<-data.frame(rowIndex,colIndex,prob_vector) #length(frame)==640*480*3==length(rowIndex)*3 ; first third represents red pixels, second third green pixels, and third third represents blue pixels
Z=sum(prob_vector)
# q_row*[p(x_q_row|mu_rgb,sigma_rgb)]
q_hat_r<-1/Z * sum(  frame$prob_vector * frame$rowIndex  )
q_hat_c<-1/Z * sum(  frame$prob_vector * frame$colIndex  )
png('centerOfMass.png')
plot(pixmapGrey(prob))
points(q_hat_c,q_hat_r,col="red")
dev.off()
#not very accurate... :/

spatial_covariance<-function(mat,q_hat){
    q<-mat[1:2]
    prob<-mat[3]
    (q-q_hat) %*% t(q-q_hat) * as.numeric(prob)
}
q_hat<-c(q_hat_r,q_hat_c)
neo<-as.matrix(frame)
spatial_covariance(neo[1,],q_hat)
all_covariances<-apply(neo,1,spatial_covariance,q_hat) #returns a matrix for every row of neo
the_one_covariance<-matrix(apply(all_covariances,1,sum),2)/Z #sums all of them and normalzies 

plot_results <- function( prob_map, mu, Sigma, npoints = 100 ){
# plot_contour(prob_im,centroid,covariance): plot the centroid and the
# contours of the corresponding gaussian on top of the model
# probabability map
#
#
# centroid, and the contours of the probability map (prob_map)
# prob_map: a M x N data matrix
# centroid: 2 x 1 vector
# covariance: 2 x 2 matrix

dx <- seq( 1, dim( prob_map )[1], length.out = npoints )
dy <- seq( 1, dim( prob_map )[2], length.out = npoints )
gr <- list( x = outer( dy*0, dx, FUN = '+' ), y = outer( dy, dx*0, FUN = '+' ) )

C <- chol( Sigma )
iSigma <- chol2inv( C )
Z <- 1/( 2*pi*sqrt( prod( diag( C ) ) ) )

dens <- rep( 0, prod( dim( gr$x ) ) )
for( i in 1:prod( dim( gr$x ) ) )
	dens[i] <- exp( -0.5*t( c( gr$x[i], gr$y[i] ) - mu )%*%iSigma%*%( c( gr$x[i], gr$y[i] ) - mu ) )
dev.new()
png('spat.png')
image( 1:dim( prob_map )[1], 1:dim( prob_map )[2], prob_map, xlab = "x", ylab = "y" )
points( Sigma[1], Sigma[2], cex = 2 )
contour( dx, dy, matrix( dens, nrow = length( dx ), ncol = length( dy ), byrow = TRUE ), add = TRUE )
dev.off()
}
plot_results(prob,q_hat,the_one_covariance) 


#11
kande2 = read.pnm("./images/kande2.pnm")
RGB2=getChannels(kande2)
png('newMap.png')
plot(pixmapGrey(apply(RGB2,c(1,2),norm3d,means,sigML) ))
dev.off()
