install.packages('UsingR')
library(UsingR)
library(manipulate)
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
x2=x-mean(x)
x3=(x-mean(x))^2

weighted.mean(x2,w)

myHist=function(mu){
    hist(x,w,col='blue',breaks=100)
    lines(c(mu,mu),c(0,150),col='red',lwd=5)
    mse=mean(x$w -mu)^2
    text(63,150,paste('mu= ',mu))
    text(63,140,paste('MSE= ',round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))