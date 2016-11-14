s1=rnorm(10, mean = 3, sd = 0.7746)
s2=rnorm(10, mean = 5, sd = 0.8246)
t.test(s1,s2,paired=FALSE, var.equal=TRUE)$conf

s1=rnorm(100, mean = 4, sd = 0.5)
s2=rnorm(100, mean = 6, sd = 2)
t.test(s2,s1,paired=FALSE, var.equal=TRUE)$conf

s1=rnorm(9, mean = -3, sd = 1.5)
s2=rnorm(9, mean = 1, sd = 1.8)
t.test(s1,s2,paired=FALSE, var.equal=TRUE)$conf

a1=c(0,1,2)
a2=c(3,4,5)
t.test(a2,a1,paired=FALSE, var.equal=TRUE)$conf
