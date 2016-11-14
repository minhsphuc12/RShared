a=matrix(c(1,2,3,5,7,8))
b=matrix(c(4,6,3,67,8,7))
a=c(1,2,3,5,7,8)
b=c(4,6,3,67,8,7)

dist(a,b)
a
b
rbind(a,b)
dist(rbind(a,b),method='manhattan')
dist(a,method='manhattan')
help(dist)
require(graphics)
sum(rdist(a,b))
x <- matrix(rnorm(100), nrow = 5)
y = matrix(rnorm(100),nrow=5)
dist(rbind(x,y))

dist(x)
x
dist(x, diag = TRUE)
dist(x, upper = TRUE)

install.packages('fields')
library(fields)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
euc.dist(a,b)
