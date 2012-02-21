## Question 1.1
## Compute Bell Curves

x <- seq(-4, 4, length=200)
y <- dnorm(x, mean=-1, sd=1)
plot(x,y,type="l",lwd=2, col="red")
y <- dnorm(x, mean=0, sd=2)
plot(x,y,type="l",lwd=2, col="red")
y <- dnorm(x, mean=2, sd=3)
plot(x,y,type="l",lwd=2, col="red")

## Question 1.2
## Generate 100 (2,1) observations with mean (1, 1) 
## and the variance-cavariance matrix

z <- matrix(c(rnorm(100),rnorm(100)),2,100)
#z <- matrix(c(rnorm(100),rnorm(100)),2,100)
###mu <- matrix(c(1,1),2,1)
mu <- matrix(replicate(200,1),2,100)
#mu <- matrix(replicate(200,1),2,100)

sigma <- matrix(c(0.3,0.2,0.2,0.2),2,2)
L <- t(chol(sigma))
L%*%t(L) # will give us the original var-covar by matrix multiplication
y <- mu+(L%*%z)
row1 <- y[1,]
row2 <- y[2,]
require(ggplot2)
qplot(row1,row2)

## Question 1.3
## Estimate sample mean and covariance
avg <- (1/length(row1)*matrix(c(sum(row1), sum(row2)))) ## = (1.0957, 1.0528)

avgmat <- matrix(replicate(100,avg),2,100) # convert avg to 2x100 matrix
covar <- (1/length(row1))*(y-avgmat)%*%t(y-avgmat) # calculate covar
resid <- covar - sigma # compute deviations

## Question 1.8
pdf <- function(y, lambda){
  return(lambda*exp(-lambda*y))
}

cdf <- function(z, lambda){
  return((-1/lambda)*log(1-z))
}

samplemean <- function(sample){
  return((1/length(sample))*sum(sample))
}
createLists <- function(N){
  listpdf<-sapply(seq(0,2,N*2),pdf,3)
  qplot(listHy,seq(0,1,N))
  listHy <- sapply(seq(0,1,N),cdf, 3)
  qplot(listHy, seq(0,1,N))
  return(cbind(listpdf, listHy))
  }

N=0.001
qplot(x=createLists(N)[,1],y=seq(0,1,N), geom="line") #1000 samples
qplot(x=createLists(N)[,2],y=seq(0,1,N), geom="line") #1000 samples
N=0.01
qplot(x=createLists(N)[,1],y=seq(0,1,N), geom="line") #1000 samples
qplot(x=createLists(N)[,2],y=seq(0,1,N), geom="line") #1000 samples
N=0.1
qplot(x=createLists(N)[,1],y=seq(0,1,N), geom="line") #1000 samples
qplot(x=createLists(N)[,2],y=seq(0,1,N), geom="line") #1000 samples

## find distribution of means
getmeans <- function(N){
  mean(sapply(runif(N), cdf, 3))
}

qplot(sapply(seq(10,10, length.out=1000), getmeans))
qplot(sapply(seq(100,100, length.out=1000), getmeans))
qplot(sapply(seq(1000,1000, length.out=1000), getmeans))

qqnorm(sapply(seq(1000,1000, length.out=1000), getmeans))

# mean10 = mean(sapply(seq(10,10, length.out=1000), getmeans))
# mean100 = mean(sapply(seq(100,100, length.out=1000), getmeans))
# mean1000 = mean(sapply(seq(1000,1000, length.out=1000), getmeans))

median10 = median(sapply(seq(10,10, length.out=1000), getmeans))
median100 = median(sapply(seq(100,100, length.out=1000), getmeans))
median1000 = median(sapply(seq(1000,1000, length.out=1000), getmeans))

sd10 = sd(sapply(seq(10, length.out=1000), getmeans))
sd100 = sd(sapply(seq(100, length.out=1000), getmeans))
sd1000 = sd(sapply(seq(1000, length.out=1000), getmeans))

qplot(c(log10(10),log10(100),log10(1000)),c(abs(1/3-mean10),abs(1/3-mean100),abs(1/3-mean1000)))
qplot(c(log10(10),log10(100),log10(1000)),c(abs(1/3-mean10)/sd10,abs(1/3-mean100)/sd100,abs(1/3-mean1000)/sd1000))
qplot(c(log10(10),log10(100),log10(1000)),c(log10(abs(1/3-mean10)/sd10),
                                            log10(abs(1/3-mean100)/sd100),log10(abs(1/3-mean1000)/sd1000)))
qplot(c(log10(10),log10(100),log10(1000)),c(log10(abs(1/3-median10)/sd10),
                                            log10(abs(1/3-median100)/sd100),log10(abs(1/3-median1000)/sd1000)))


qplot(sapply(seq(1000,1000, length.out=1000), getmeans))


###############
###############
require("pixmap")
setwd( "~/Documents/KU/statml/" )
# To run any script (like this one) inside R, just type source("case19.R"). You can also just copy-paste the code into the R console.

# We need an R-package called pixmap to handle and manipulate images like in this exercise. If it's not installed on your computer, simply type install.packages("pixmap") in the R-console. 

# pixmap only reads pictures in the PNM-format, not the JPG-format. Therefore, images of that format should be provided. If not, you can easily convert from JPG to PNM using this online converter: http://www.pictureresize.org/online-images-converter.html

library(pixmap) # load the package
setwd("~/Dropbox/StatsML/Assignment1/")
kande1 = read.pnm("./images/kande1.pnm")

# pixel specifications:
tll = c(150, 328)  # training: lower left
tur = c(330, 264)  # training: upper right

## Show image and regions

plot(kande1)
rect(xleft = tll[1], ybottom = 640 - tll[2], xright = tur[1], ytop= 640 - tur[2]) 
text(tll[1] + 25, 640 - tur[2] + 15, labels= "train")


# we subtract from 640 because the plot has origo at lower left corner, now. Don't worry about the subtractions and additions to the x and y parameters to the text-method. These are only there to position the text labels correctly.

## Extract regions
train.set = kande1[tur[2]:tll[2], tll[1]:tur[1]]

# Training data:
train = NULL
train$red = as.vector(getChannels(train.set,"red"))
train$green = as.vector(getChannels(train.set,"green"))
train$blue = as.vector(getChannels(train.set,"blue"))

train = as.data.frame(train)

# Ok, so now you have the data you need. They are arranged in a data frame, because they are easily handled by most commands such as mean and cov etc. At some point however, you probably would like to loop over all the pixels in kande1. The pixmap package works like this: to get the size of the image, just do kande1@size[1] for the x-size. kande1@size[2] for the y-size. As you can see above, we access the RGB-values of a picture using getChannels(). Here's an example on how you handle the object:
# RGB = getChannels(kande1)
# pix = RGB[14,32,] # Get the RGB-value at pixel (14,32) in kande1.

require("pixmap")
setwd( "~/Documents/KU/statml/" )
# To run any script (like this one) inside R, just type source("case19.R"). You can also just copy-paste the code into the R console.

# We need an R-package called pixmap to handle and manipulate images like in this exercise. If it's not installed on your computer, simply type install.packages("pixmap") in the R-console. 

# pixmap only reads pictures in the PNM-format, not the JPG-format. Therefore, images of that format should be provided. If not, you can easily convert from JPG to PNM using this online converter: http://www.pictureresize.org/online-images-converter.html

library(pixmap) # load the package
setwd("~/Dropbox/StatsML/Assignment1/")
kande1 = read.pnm("./images/kande1.pnm")

# pixel specifications:
tll = c(150, 328)  # training: lower left
tur = c(330, 264)  # training: upper right

## Show image and regions

plot(kande1)
rect(xleft = tll[1], ybottom = 640 - tll[2], xright = tur[1], ytop= 640 - tur[2]) 
text(tll[1] + 25, 640 - tur[2] + 15, labels= "train")


# we subtract from 640 because the plot has origo at lower left corner, now. Don't worry about the subtractions and additions to the x and y parameters to the text-method. These are only there to position the text labels correctly.

## Extract regions
train.set = kande1[tur[2]:tll[2], tll[1]:tur[1]]

# Training data:
train = NULL
train$red = as.vector(getChannels(train.set,"red"))
train$green = as.vector(getChannels(train.set,"green"))
train$blue = as.vector(getChannels(train.set,"blue"))

train = as.data.frame(train)

# Ok, so now you have the data you need. They are arranged in a data frame, because they are easily handled by most commands such as mean and cov etc. At some point however, you probably would like to loop over all the pixels in kande1. The pixmap package works like this: to get the size of the image, just do kande1@size[1] for the x-size. kande1@size[2] for the y-size. As you can see above, we access the RGB-values of a picture using getChannels(). Here's an example on how you handle the object:
# RGB = getChannels(kande1)
# pix = RGB[14,32,] # Get the RGB-value at pixel (14,32) in kande1.

means  <- matrix(c(mean(train$red), mean(train$green), mean(train$blue)),1,3)
colnames(means) <- c("red","green","blue") 
                   
## Estimate sample mean
empmean <- (1/length(train$red)*matrix(c(sum(train$red), sum(train$green), sum(train$blue)))) ## = (1.0957, 1.0528)
## Estimate sample covariance
avgmat2 <- t(replicate(length(train$red),empmean,3)) # convert avg to 3x11765 matrix
covar <- (1/length(train$red))*t(as.matrix(train-avgmat2))%*%(as.matrix(train-avgmat2)) # calculate covar


norm <-function(sigma, mu, x){
  (1/2*pi^(3/2))*(1/sqrt(abs(sigma)))*exp(-.5*(t(x-mu))*(solve(sigma))*(x-mu))  
}

