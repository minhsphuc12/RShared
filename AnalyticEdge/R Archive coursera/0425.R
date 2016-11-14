a= matrix(rnorm())



#quiz 3 questions 1,2
library(datasets)
data(iris)
?iris
virginica=subset(iris,Species=='virginica')
virginica_petal=virginica[,1]
mean(virginica_petal)
apply(iris[, 1:4], 2, mean)

#quiz 3 questions 3,4
library(datasets)
data(mtcars)
?mtcars
split(mtcars$mpg, mtcars$cyl)               #vector of mpg by each cyl
sapply(split(mtcars$mpg, mtcars$cyl), mean) #average mpg by cyl
tapply(mtcars$mpg, mtcars$cyl, mean)        #same above
lapply(mtcars, mean)                        #mean of each column
apply(mtcars,2,mean)                        #same above

abs(mean(subset(mtcars,cyl==4)[,4])-mean(subset(mtcars,cyl==8)[,4]))
#absolute difference between the average horsepower of 4-cylinder 
#cars and the average horsepower of 8-cylinder cars

#split multiple levels
f1=gl(2,5)
f2=gl(5,2)
k=interaction(f1,f2)
s=rnorm(10)
str(split(s,k))

#mapply
r=list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))
r2=mapply(rep,1:4,4:1)
noise=function(n,s,d){
    rnorm(n,s,d)
}
mapply(noise,1:5,1:5,2)
#same as mapply
list(noise(1,1,2),noise(2,2,2),noise(3,3,2),noise(4,4,2),noise(5,5,2))

#simulation from linear function
set.seed(20)
a=rnorm(100)
b=rnorm(100,0,2)
y=1+2*a+b
log.mu=0.5+0.3*a
y2=rpois(20,exp(log.mu))
