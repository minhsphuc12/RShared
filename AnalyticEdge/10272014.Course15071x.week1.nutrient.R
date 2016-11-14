data=read.csv('USDA.csv')
str(data)
summary(data
        )
which.max(data$Sodium)

HighSodium = subset(data, Sodium>10000)
nrow(HighSodium)
HighSodium$Description

Caviar_Index= match('CAVIAR',data$Description)
#this will look up for caviar in food data
caviar_sodium=data$Sodium[match('CAVIAR',data$Description)]
#check sodium amount of caviar

summary(data$Sodium)
sd(data$Sodium,na.rm=TRUE)
#there were some data not avaialble
z_value_caviar=(caviar_sodium-mean(data$Sodium,na.rm=TRUE))/sd(data$Sodium,na.rm=TRUE)
#this get 1.1267, which is more than 1 standard deviation from the mean
#there for caviar is pretty rich in sodium compared to others in dataset

View(data)
plot(data$Protein,data$TotalFat)

hist(data$VitaminC,xlim=c(0,100),breaks=4000)
boxplot(data$VitaminC)

high_sod_check=data$Sodium>mean(data$Sodium,na.rm=TRUE)
high_sod_check_numeric=as.numeric(high_sod_check)
high_sod_check_numeric
data$high_sodium=high_sod_check_numeric

high_fat_check=data$TotalFat>mean(data$TotalFat,na.rm=TRUE)
high_fat_check_numeric=as.numeric(high_fat_check)
high_fat_check_numeric
data$high_fat=high_fat_check_numeric

table(high_sod_check_numeric)


table(data$high_sod,data$high_fat)


#tapply(arg1,arg2,arg3) group argument 1 
#by argument 2 and apply argument 3

tapply(data$Iron,data$high_sod,mean,na.rm=TRUE)
'''result
       0        1 
2.332241 3.914451 
mean: mean of iron for food with low sod is 2.33
      mean of iron for food with high sod is 3.9144
observation: higher sod <--> higher iron??
'''
library(plyr)
tapply(data$high_sod,data$high_sod,length)
#this is the way to get categorical counting by tapply


