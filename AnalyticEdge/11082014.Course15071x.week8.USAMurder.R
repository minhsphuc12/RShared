murders=read.csv('murders.csv')
str(murders)
statemap=map_data('state')
ggplot(statemap, aes(long,lat,group=group))+geom_polygon(fill='white',color='black')+coord_map('mercator')
#this only to MAP the UAS map

murders$region=tolower(murders$State)
mudermap=merge(statemap,murders,by='region')
#merge 2 dataset

ggplot(mudermap, aes(long,lat,group=group,fill=Murders))+geom_polygon(color='black')+coord_map('mercator')+scale_fill_gradient(low='white',high='red',guide='legend')
#fill based on muders quantity
#remove fill in _polygon parameter
#add _fill_gradient
ggplot(mudermap, aes(long,lat,group=group,fill=Murders/Population*100000))+geom_polygon(color='black')+coord_map('mercator')+scale_fill_gradient(low='white',high='red',guide='legend')
#use Muders/Population rate
#or just create new variable
mudermap$MurderRate=mudermap$Murders/mudermap$Population*100000
ggplot(mudermap, aes(long,lat,group=group,fill=MurderRate))+geom_polygon(color='black')+coord_map('mercator')+scale_fill_gradient(low='white',high='red',guide='legend')
#SAME
#there is an outlier (Washington DC :v), so it make other looks insignificant
#remove an outlier by place an limits parameter on _fill_gradient
ggplot(mudermap, aes(long,lat,group=group,fill=MurderRate))+geom_polygon(color='black')+coord_map('mercator')+scale_fill_gradient(low='white',high='red',guide='legend',limits=c(0.9,10))
#now we got something --> LOUSIANA 
ggplot(mudermap, aes(long,lat,group=group,fill=GunOwnership))+geom_polygon(color='black')+coord_map('mercator')+scale_fill_gradient(low='white',high='red',guide='legend')
