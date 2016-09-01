options(stringsasfactors=F)
setwd('/Users/christopherlee/Documents/CAL/Real_Life/Geography/')
G<-read.csv("Geography of Cal.csv", TRUE, sep=",",dec=".", na.strings="NA", stringsAsFactors=FALSE)
G2<-read.csv("Geography of Cal2.csv", TRUE, sep=",",dec=".", na.strings="NA", stringsAsFactors=FALSE)

G$Date.Begin<-as.Date(strptime(G$Date.Begin, format='%m/%d/%Y'))
G$End.Date<-as.Date(strptime(G$End.Date, format='%m/%d/%Y'))
G2$Date.Begin<-as.Date(strptime(G2$Date.Begin, format='%m/%d/%Y'))
G2$End.Date<-as.Date(strptime(G2$End.Date, format='%m/%d/%Y'))
(G$End.Date - G$Date.Begin)+1->N

N<-as.integer(N)
G$Location[which(G$Location=='Aberdeen')]<-'Hong Kong'
G$Location[which(G$Location=='Kowloon')]<-'Hong Kong'
Cities<-unique(G$Location)
TN<-rep(0,length(Cities))
for (i in 1:length(Cities)) {
  TN[i]<-sum(N[which(G$Location==Cities[i])], na.rm=TRUE)
}
cbind(Cities,TN)->Total
Total$TN<-as.numeric(Total$TN)
SIZE<-7
Stops<-which(TN>=SIZE)
Total[Stops]

library('RColorBrewer')
library('ggmap')
PA<-c(brewer.pal(9,'Set1'), brewer.pal(8,'Set2'),brewer.pal(9,'Set3'))
#Start with a blank plot with the dates on the x-asis and a y-axis big enough to host all the data
qplot(G2$Date.Begin, rep(-10, length(G2[,1])), ylim=c(10,60), ylab='Cities', xlab='Date', main='Travels of Cal')
for (i in 1:length(Stops)){
  current.db<-G2$Date.Begin[which(G2$Location==Total[Stops][i])]
  current.ed<-G2$End.Date[which(G2$Location==Total[Stops][i])]
  for (j in 1:length(current.db)) {
    lines(c(current.db[j], current.ed[j]),c(G2[which(G2$Location==Total[Stops][i]),12][1],G2[which(G2$Location==Total[Stops][i]),12][1]), col=PA[i], lwd=5)
    text(x=as.Date('2012-01-01'),y=G2[which(G2$Location==Total[Stops][i]),12], labels=G2[which(G2$Location==Total[Stops][i]),2][1], cex=0.8, font =2)
  }
}
text(x=rep(as.Date('2012-01-01'),length(Stops)), y=G2[which(G2$Location==Total[Stops])[c(1:(length(Stops)))],12], labels=Total[Stops], cex=0.8, font =2)


merge(G2[which(!duplicated(G2$Location)),], Total, by.x='Location', by.y='Cities')->G3
G3<-G3[which(!is.na(G3$long)),]
G3$TN<-as.numeric(as.character(G3$TN))
mapWorld<-borders("world", colour="gray50", fill="gray50")
map<- ggplot() + mapWorld
mapPoints <- map + geom_point(aes(x = long, y = lat, size =(TN^0.5)/12, colour=factor(strptime(G3$Date.Begin, format='%Y-%m-%d')$year+1900)), data = G3, alpha=0.6)+ theme(legend.position='right') +  scale_color_brewer(palette='Set1', name='Year')
mapPoints + scale_size(range=c(0,4), guide=FALSE) + ggtitle('Cal Travel Map') + labs(x='', y='')


mapPoints <- map + geom_point(aes(x = long, y = lat, size =(TN^0.5)/12, alpha = .5), data = G3, color=PA[strptime(G3$Date.Begin, format='%Y-%m-%d')$year-100],)+ theme(legend.position='right') + scale_color_continuous(name='Year')