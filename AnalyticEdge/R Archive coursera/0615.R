library(UsingR)
library(ggplot2)
library(reshape)

data(diamond)
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(lm(price ~ carat, data = diamond), lwd = 2)

fit <- lm(price~ carat, data = diamond)
coef(fit)
#expected 3721.02 (SIN) dollar increase in price for 
#every carat increase in mass of diamon

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
#Thus $500.1 is the expected price for the average sized 
#diamond of the data (0.2042 carats).

fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)
#expect a 372.102 (SIN) dollar change in price for 
#every 1/10th of a carat increase in mass of diamond

newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
predict(fit, newdata = data.frame(carat = newx))
#prediction
diamond$predictedvalue=predict(fit)
diamond$estimationerror=diamond$price-diamond$predictedvalue
#estimation error

#plotting stuff
ggplot(data=diamond,
       aes(x=date, y=value, colour=variable)) +geom_dot()
plot(price~carat,data=diamond)
lines(diamond$predictedvalue~diamond$carat,lwd=2)
plot(price~carat,data=diamond)

#residual of linear data
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit4 <- lm(y ~ x)
plot(price~carat,data=diamond)
abline(fit4$coef[1],fit4$coef[2])
e <- resid(fit4)
yhat <- predict(fit4)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit4)[2] * x)))

#residual of non linear data
x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2); 
plot(x, y); abline(lm(y ~ x))
plot(x, resid(lm(y ~ x))); 
abline(h = 0)

#residual variantion
summary(fit4)$sigma
sqrt(sum(resid(fit4)^2)/(n-2))

#heteroskedascity
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
plot(x, y); abline(lm(y ~ x))
plot(x, resid(lm(y ~ x))); 
abline(h = 0)

#last example
data(anscombe)
example(anscombe)

#quiz 2 question 1,2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
a1=lm(y~x)
summary(a1)
plot(y~x);abline(a1)
summary(a1)$sigma

#quiz 2 question 3
mt=read.csv('mtcars.csv',header=TRUE)
m1=mt$mpg
m2=mt$wt
a3=lm(m1~m2)
summary(a3)
q3a= a3$coef[1]+a3$coef[2]*mean(m2)
q3b= qt(0.975,df=length(m1)-1) * sd(m1) / sqrt(length(m1))
q3final=q3a-q3b
q3final
q3c= a3$coef[1]+a3$coef[2]*3
q4final=q3c+q3b
q4final

summary(a3)$sigma

a4=lm(m1~m1)

summary(a4)$sigma

#Practical machine learning
install.packages('AppliedPredictiveModeling')
install.packages('caret')
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
plot(CompressiveStrength~row.names,data=testing)