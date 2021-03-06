options(stringsasfactors=F)
setwd('/Users/christopherlee/Documents/CAL/Real_Life/Geography/')
G<-read.csv("Geography of Cal.csv", TRUE, sep=",",dec=".", na.strings="NA", stringsAsFactors=FALSE)
G$Date.Begin<-as.Date(strptime(G$Date.Begin, format='%m/%d/%Y'))
G$End.Date<-as.Date(strptime(G$End.Date, format='%m/%d/%Y'))
G$Date.Begin<-as.Date(format(G$Date.Begin, '20%y-%m-%d')) #this line may be needed if .csv file truncates dates to /yy instead of /yyyy
G$End.Date<-as.Date(format(G$End.Date, '20%y-%m-%d'))
(G$End.Date - G$Date.Begin)+1->N #N = number of nights of stay. Because Date Begin is the date of the first night spent and End Date is date of the last night, the +1 is required

N<-as.integer(N) #Safety precaution in case of date input error
G$Location[which(G$Location=='Aberdeen')]<-'Hong Kong'
G$Location[which(G$Location=='Kowloon')]<-'Hong Kong'
Cities<-unique(G$Location)
TN<-rep(0,length(Cities))
for (i in 1:length(Cities)) {
  TN[i]<-sum(N[which(G$Location==Cities[i])], na.rm=TRUE)
} #TN represents total night spent in a given city
cbind(Cities,TN)->Total
SIZE<-6 #Flexible, determines minimum number of nights spent in a city to make our graph
Stops<-which(TN>=SIZE)
Total[Stops]

library('RColorBrewer')
PA<-c(brewer.pal(9,'Set3'), 'red')
#Start with a blank plot with the dates on the x-axis and a y-axis big enough to host all the data

plot(G$Date.Begin, rep(-1, length(G[,1])), ylim=c(0,length(Stops)+0.5), ylab='Cities', xlab='Date', main='Travels of Cal', yaxt='n')
#Iterate through cities in stops
for (i in 1:length(Stops)){
  current.db<-G$Date.Begin[which(G$Location==Total[Stops][i])]
  current.ed<-G$End.Date[which(G$Location==Total[Stops][i])]
  for (j in 1:length(current.db)) {
    #lines(c(current.db[j], current.ed[j]),c(G[which(G$Location==Total[Stops][3]),7][1],G[which(G$Location==Total[Stops][3]),7][1]), col=PA[i], lwd=5) #Optional addition to grab based on latitude
    lines(c(current.db[j], current.ed[j]),c(i,i), col=PA[i], lwd=3)
    #Create a line from (x1,y1) to (x2, y1) where x1 and x2 are start and end dates of a stay, and y1 is simply an incremental integer
  }
}
text(x=rep(as.Date('2012-01-01'),length(Stops)), y=c(0.8:(length(Stops)-0.2)), labels=Total[Stops], cex=0.6, font =2)
#Add city name labels to graph


#NOT SHOWN - merge data with existing cities and dataset, and manually add missing long and lat coordinates
#Assume G2 is the same dataset as G but now with columns "long" and "lat" which represent geographic coordinates
merge(G2[which(!duplicated(G2$Location)),], Total, by.x='Location', by.y='Cities')->G3
G3<-G3[which(!is.na(G3$Lon)),]
G3$TN<-as.numeric(as.character(G3$TN))
mapWorld<-borders("world", colour="gray50", fill="gray50")
#mapWorld<-borders("world", colour="brown",  lwd=0.1)

map<- ggplot() + mapWorld 
mapPoints <- map + geom_point(aes(x = Lon, y = Lat, size =(TN^0.5), colour=factor(strptime(G3$Date.Begin, format='%m/%d/%y')$year+1900)), data = G3, alpha=0.8)+ theme(legend.position='right') +  scale_colour_manual(values=PA, name='Year')
mapPoints + scale_size(range=c(0,4), guide=FALSE) + ggtitle('Cal Travel Map') + labs(x='', y='')
mapPoints + scale_size_continuous(range=c(0,5), name='Number of Days', breaks=sqrt(c(1,10,100,1000)), labels=c(1,10,100,1000)) + ggtitle('Cal Asia Travel Map') + labs(x='', y='') 


mapPoints <- map + geom_point(aes(x = Lon, y = Lat, size =(TN^0.5)/12, alpha = .5), data = G3, color=PA[strptime(G3$Date.Begin, format='%Y-%m-%d')$year-100],)+ theme(legend.position='right') + scale_color_continuous(name='Year')


