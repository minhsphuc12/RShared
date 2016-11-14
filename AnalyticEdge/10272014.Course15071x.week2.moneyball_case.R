baseball=read.csv('baseball.csv')
str(baseball)
View(baseball)

moneyball=subset(baseball,Year<2002)
moneyball$RD=moneyball$RS-moneyball$RA

wins=lm(W~RD,data=moneyball)
summary(wins)

wins2=lm(RS~OBP+SLG+BA,data=moneyball)
summary(wins2)
#BA has negative coeff, 
#maybe there is some multicollineray 

wins3=lm(RS~OBP+SLG,data=moneyball)
summary(wins3)
#this works better

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)
