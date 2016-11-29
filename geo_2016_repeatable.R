setwd('/Users/christopherlee/Documents/CAL/Real_Life/Geography/')
library(ggplot2)
library(geosphere)
library(ggmap)
library(RColorBrewer)
library(sp)
library(plyr)

G_merged<-read.csv('G_merged.csv')
G_merged$Date.Begin <- as.Date(G_merged$Date.Begin)
G_merged$End.Date <- as.Date(G_merged$End.Date)
mapWorld<-borders("world", colour="darkseagreen1", fill="darkseagreen1")
map<- ggplot() + mapWorld + theme(panel.background = element_rect(fill = 'dodgerblue4', colour = 'dodgerblue4'))
map + geom_point(data=G_merged, aes(x=Lon, y=Lat, size=sqrt(Nights), color=Date.Begin), alpha=0.5)  
scale_color_gradient(low='pink', high='blue')

pal<-brewer.pal(9, 'Set1')
arcmap <- map + geom_point(data=G_merged, aes(x=Lon, y=Lat, size=sqrt(Nights), color=Country), alpha=0.5)  
for(i in 1:(nrow(G_coords)-1)){
	arcmap<-arcmap + geom_path(data=lines[[i]], aes(x=lon, y=lat), size=.4, color='light blue', alpha=0.6)
}
center<-180
#base<-map('world', resolution=0, fill=F, xlim=c(-150, 170), ylim=c(-15,65))
worldmap <- map_data ("world", resolution=0, xlim=c(-150, 170), ylim=c(-15,85))
#worldmap <- map_data(base)
worldmap$long.recenter <- ifelse(worldmap$long < center - 180 , worldmap$long + 360, worldmap$long)

### Function to regroup split lines and polygons
# Takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) { # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1 # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2 # parts that are moved
  }
  g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# Takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df)) # rassign the order variable
  df[,ordercol] <- o
  df
}

# now regroup
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")

# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order") # use the new grouping var
#############################################################################

# Plot worldmap using data from worldmap.cp
worldmap = ggplot(aes(x = long.recenter, y = lat), data = worldmap.cp) + 
  geom_polygon(aes(group = group.regroup), fill="darkseagreen1", colour = "darkseagreen1") + 
  scale_y_continuous(limits = c(-60, 85)) +  
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0)) +
  coord_equal() +  theme_bw() + 
  theme(legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    panel.border = element_rect(colour = "black"),
    panel.background = element_rect(fill = 'dodgerblue4', colour = 'dodgerblue4'))

G_merged$Lon.recenter <- ifelse(G_merged$Lon < center - 180, G_merged$Lon + 360, G_merged$Lon)
G_coords<-G_merged[!is.na(G_merged$Lon),]
lines<-vector('list', length=nrow(G_coords)-1)
for(i in 1:(nrow(G_coords)-1)){
	lines[[i]]<-as.data.frame(gcIntermediate(G_coords[i,c(8:9)], G_coords[i+1, c(8:9)]))
	for(j in 1:nrow(lines[[i]])){
		lines[[i]]$lon[j] <- ifelse(lines[[i]]$lon[j]< center - 180, lines[[i]]$lon[j] + 360, lines[[i]]$lon[j]) 
	}
}

sweetmap <- worldmap + geom_point(data=G_merged, aes(x=Lon.recenter, y=Lat, size=sqrt(Nights), color=Country), alpha=0.5)  
sweetermap<-qmap(c(180,0), zoom=2)+ geom_point(data=G_merged, aes(x=Lon.recenter, y=Lat, size=sqrt(Nights), color=Country), alpha=0.5)  
for(i in 1:(nrow(G_coords)-1)){
	sweetmap <-sweetmap + geom_path(data=lines[[i]], aes(x=lon, y=lat), size=.4, color='light blue', alpha=0.6)
	sweetermap <-sweetermap + geom_path(data=lines[[i]], aes(x=lon, y=lat), size=.4, color='dark red', alpha=0.6)
}
sweetmap + xlim(0, 320) + ylim(-15,85) + ggtitle('2016 Travels') +
pal<-c('grey', brewer.pal(9, 'Set1'))
sweetermap + scale_color_manual(values=pal, guide=F) + scale_size_continuous(range(0,2.5), guide=F)+ ylim(-25,65) + ggtitle('2016 Travels')
