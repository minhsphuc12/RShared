who=read.csv('WHO.csv')
str(who)
plot(who$GNI,who$FertilityRate)
install.packages('ggplot2')
library(ggplot2)

scatterplot=ggplot(who,aes(x=GNI,y=FertilityRate))
#create original graph object, so add geometrics component laters

scatterplot + geom_point(color='blue',size=2, shape=15) +ggtitle('Fertility Rate vs. GNI')
#geom_point: parameters color, size, shape
#geom_title: parameter title text
#geom_line

fertilityGNIplot= scatterplot + geom_point(color='blue',size=2, shape='f') +ggtitle('Fertility Rate vs. GNI')
#make an object our of the graph
fertilityGNIplot
pdf('myplot.pdf')
print(fertilityGNIplot)
dev.off()
 
ggplot(who,aes(x=GNI,y=FertilityRate,color=Region))+geom_point()
#GNI vs. fertility rate, with region group

ggplot(who,aes(x=GNI,y=FertilityRate,color=LifeExpectancy))+geom_point()
#GNI vs. fertility rate, with life expectacy group

ggplot(who,aes(x=log(FertilityRate),y=Under15))+geom_point()
#Under15 vs Fertility Rate relationship, getting diminishing rate of increase
#so the direction will be LOG transformation
#it works!, get linear now

model=lm(Under15~FertilityRate,data = who)
summary(model)
model2=lm(Under15~log(FertilityRate),data = who)
summary(model2)
#so the log transformation of fertility rate will increase
#R^2 from 87.6% to 93.9% --> much better model

ggplot(who,aes(x=log(FertilityRate),y=Under15))+geom_point() + stat_smooth(method='lm')
#add regression line with stat_smooth(method='lm')
#this shaded 95% confident level by default
#change this to 99% by add parameter level=0.99
ggplot(who,aes(x=log(FertilityRate),y=Under15))+geom_point() + stat_smooth(method='lm',level=0.99)
#get only the LINE by remove 'level', add se=FALSE
ggplot(who,aes(x=log(FertilityRate),y=Under15))+geom_point() + stat_smooth(method='lm',se=FALSE,color='orange')
#add some color for fun

ggplot(who, aes(x = FertilityRate, y = Under15,color=Region)) + geom_point()+scale_color_brewer(palette="Dark2")
# get more contrast by scale_color_brewer(palette="Dark2") 

