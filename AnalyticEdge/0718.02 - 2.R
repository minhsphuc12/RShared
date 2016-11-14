library(lattice)
a=file.path(getwd(),'contribution.csv')
don <- read.csv(a,header=TRUE)
don[1:5,]
x= table (don$Class.Year)
barchart(x,horizontal=FALSE,xlab='class year',col='violet')
don$TGiving=don$FY00Giving+don$FY01Giving+don$FY02Giving+
          don$FY03Giving+don$FY04Giving
mean(don$TGiving)
sd(don$TGiving)
quantile(don$TGiving,probs=seq(0,1,0.05))
hist(don$TGiving)
hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000])
boxplot(don$TGiving,horizontal=TRUE,xlab="Total Contribution")
boxplot(don$TGiving,outline=FALSE,horizontal=TRUE,
         xlab="Total Contribution")

ddd=don[don$TGiving>=30000,]
ddd
ddd1=ddd[,c(1:5,12)]
ddd1
ddd1[order(ddd1$TGiving,decreasing=TRUE),]

#find average giving amount by major
t4=tapply(don$TGiving,don$Major,mean,na.rm=TRUE)
t4
#check number of student on major
t5=table(don$Major)
t5
#bind major, number of student and average giving
t6=cbind(t4,t5)
#select only majors with more than 10 students counted
t7=t6[t6[,2]>10,]
#sort
t7[order(t7[,1],decreasing=TRUE),]
#make chart
barchart(t7[,1],col="black")

#same w/ above, but now with next_degree type
t4=tapply(don$TGiving,don$Next.Degree,mean,na.rm=TRUE)
t4
t5=table(don$Next.Degree)
t5
t6=cbind(t4,t5)
t7=t6[t6[,2]>10,]
t7[order(t7[,1],decreasing=TRUE),]
barchart(t7[,1],col="black")

#density plot of giving by each year, use line, color black
densityplot(~TGiving|factor(Class.Year),
             data=don[don$TGiving<=1000,][don[don$TGiving<=1000,]
           $TGiving>0,],plot.points=FALSE,col="black")

#get total giving by class year
t11=tapply(don$TGiving,don$Class.Year,FUN=sum,na.rm=TRUE)
t11
barplot(t11,ylab="Average Donation")

barchart(tapply(don$FY04Giving,don$Class.Year,FUN=sum,
                 na.rm=TRUE),horizontal=FALSE,ylim=c(0,225000),col="black")
#separate charts of total giving comparison of 5 generations, vertical chart
barchart(tapply(don$FY04Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal=FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY03Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY02Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY01Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY00Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")

#create column Giving IND, value 0 means 0$, 1 means more than 0$
don$TGivingIND=cut(don$TGiving,c(-1,0.5,10000000),labels=FALSE)-1
#percentage of people giving
mean(don$TGivingIND)
#classify giving identification with class; 1957 class got 31 not giving, 96 giving
t5=table(don$TGivingIND,don$Class.Year)
t5 
#plot with both giving and not side by side if beside =TRUE, stacked if FALSE
barplot(t5,beside=TRUE)

#another type of plot: mosaic
mosaicplot(factor(don$Class.Year)~factor(don$TGivingIND))

#compare percentage of giving for each year, 1957 is the best
t50=tapply(don$TGivingIND,don$Class.Year,FUN=mean,na.rm=TRUE)
t50
barchart(t50,horizontal=FALSE,col="black")

#same w/ above but only for year 2004, later generation just getting worse
don$FY04GivingIND=cut(don$FY04Giving,c(-1,0.5,10000000),labels=FALSE)-1
t51=tapply(don$FY04GivingIND,don$Class.Year,FUN=mean,na.rm=TRUE)
t51
barchart(t51,horizontal=FALSE,col="black")

#data frame of all year giving
Data=data.frame(don$FY04Giving,don$FY03Giving,don$FY02Giving,don$FY01Giving,don$FY00Giving)
correlation=cor(Data)
#pair correlation 
correlation
plot(Data)
library(ellipse)  
plotcorr(correlation)

#male and female have same percentage giving
mosaicplot(factor(don$Gender)~factor(don$TGivingIND))
#single and divorce give least
mosaicplot(factor(don$Marital.Status)~factor(don$TGivingIND))

#same with above
t2=table(factor(don$Marital.Status),factor(don$TGivingIND))
mosaicplot(t2)

#people attend event have higher chance to give than who don't go
mosaicplot(factor(don$AttendenceEvent)~factor(don$TGivingIND))

t2=table(factor(don$Marital.Status),factor(don$TGivingIND),factor(don$AttendenceEvent))
t2
#compare people don't attend event with who do BY each martial status: divorce ~ same; 
#other type~ big time
par(mfcol=c(1,2))
mosaicplot(t2[,,1])
mosaicplot(t2[,,2])

a = c(2,3,,56,3,6,4,6)
b = factor(a)
str(b)
# variable gender with 20 "male" entries and 
# 30 "female" entries 
gender <- c(rep("male",20), rep("female", 30)) 
gender <- factor(gender) 
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
# R now treats gender as a nominal variable 
summary(gender)


