options(stringsasfactors=F)
setwd('/Users/christopherlee/Documents/CAL/Real_Life/Geography/')
library('RColorBrewer')
library('ggmap')
TS<-read.csv('Backpacker_Sites_Summary.csv')
TS$Name<-as.character(TS$Name)
boundScale<-2
minLng<-min(TS$long)-boundScale
maxLng<-max(TS$long)+boundScale
minLat<-min(TS$lat)-boundScale
maxLat<-max(TS$lat)+boundScale
qmap(location=c(mean(c(minLng, maxLng)), mean(c(minLat,maxLat))), zoom = 4, source = 'stamen', maptype='watercolor')->stbase
qmap(location=c(mean(c(minLng, maxLng)), mean(c(minLat,maxLat))), zoom = 4, source = 'google', maptype='satellite')->gbase
qmap(location=c(mean(c(minLng, maxLng)), mean(c(minLat,maxLat))), zoom = 4, source = 'stamen', maptype='toner-lines')->tonerbase
qmap(location=c(mean(c(minLng, maxLng)), mean(c(minLat,maxLat))), zoom = 14, source = 'stamen', maptype='terrain') ->qbase

colorTypes<-brewer.pal(3,'Set1')
siteType<-TS$Type
world <-borders("world", colour="gray30", fill="gray30", xlim=c(minLng, maxLng), ylim=c(minLat, maxLat))
siteMap <- ggplot() + world
sitePoints <- gbase + geom_point(aes(x=long, y=lat, size=Overall/10, shape=Type, color=Country), data=TS, alpha=0.8)
sitePoints + scale_size_continuous(range=c(0,3), guide=FALSE) + ggtitle('Cal Tourist Site Map') + scale_color_brewer(palette='Dark2')
library(ggrepel)

sitePoints + scale_size_continuous(range=c(0,3), guide=FALSE) + ggtitle('Cal Tourist Site Map') + scale_color_brewer(palette='Dark2') + geom_text_repel(data=TS, aes(x=long, y=lat, label = TS$Name, size=0.33), color='white', segment.color='white', box.padding = unit(0.5, "lines"))