library(ggplot2)
intl=read.csv('intl.csv')
str(intl)
ggplot(intl,aes(Region,order(PercentOfIntl))) + geom_bar(stat='identity')+geom_text(aes(label=PercentOfIntl))
ggplot(intl,aes(Region,PercentOfIntl)) + geom_bar(stat='identity')+geom_text(aes(label=PercentOfIntl))

intl=transform(intl,Region=reorder(Region,-PercentOfIntl))
#use transformation on Region variable
#another option
intl$PercentOfIntl=intl$PercentOfIntl*100

ggplot(intl,aes(Region,PercentOfIntl)) +
    geom_bar(stat='identity',fill='dark blue') +
    geom_text(aes(label=PercentOfIntl),vjust=-0.5)+
    ylab('Percentage Intl Student')+
    theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
#SO MANY NEW ELEMENTS
# _bar parameter "stat='identity'" will set value presented to y value
# _text parameter vjust will justify text down with positive value
# theme: just look and differentiate next time, but axis.text.x=element_text(angle=45,hjust=1) comes in handy

intlall=read.csv('intlall.csv')
str(intlall)
intlall[is.na(intlall)]=0
#replace NA values w/ 0

world_map=map_data('world')
#REMEMBER MAP_DATA() FUNCTIOn
str(world_map)

merge=merge(world_map,intlall,by.x='region',by.y='Citizenship')
#merge two dataset with region and Citizenship variable

ggplot(merge,aes(long,lat,group=group))+
  geom_polygon(fill='white',color='black')+
  coord_map('mercator')
#short for coord_map(projection='mercator') 
## equally spaced straight meridians,   ##
## conformal, straight compass courses  ##

merge=merge[order(merge$group,merge$order),]
table(world_map$region)
#sort the merged data based on group and order

intlall$Citizenship[intlall$Citizenship == NA]= "China"
intlall$Citizenship=factor(intlall$Citizenship)
#convert China (Republic of People) to China, so to consistent with world_map
#this doesn't work out, ain't know why?
#let's just say it worked

merge=merge(world_map,intlall,by.x='region',by.y='Citizenship')
merge=merge[order(merge$group,merge$order),]

ggplot(merge,aes(long,lat,group=group))+
  geom_polygon(aes(fill=Total),color='black')+
  coord_map('mercator')
#with color based on total number of student
#aes(fill=Total)

ggplot(merge,aes(long,lat,group=group))+
  geom_polygon(aes(fill=Total),color='black')+
  coord_map('ortho',orientation=c(20,30,0))
#3D world map
#coord_map('ortho')
#orientation=c(20,30,0) is centered view point

households=read.csv('households.csv')
library(reshape2)

str(households)

households[,1:2]

housemelt=melt(households,id='Year')
#this melt() function of reshape2 package
#rearrange all other variables based on 1 variable
#this is pretty similar with pivot table in Excel

ggplot(housemelt,aes(Year,value,color=variable))+
  geom_line(size=2)+
  geom_point(size=5)
# REMEMBER aesthetic parameter color=variable

617*150+16*238
