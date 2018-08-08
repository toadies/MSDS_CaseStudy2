library(ggplot2)
library(dplyr)
library(sqldf)
library(geojsonio)

setwd("C:/Users/Anand/Documents/SMU/DDS/GITHUB/Case-Study-2/MSDS_CaseStudy2/data/")
filePath='checkins-final.csv'
checkIn_all <- read.csv(filePath)
summary(checkIn_all)
checkIn_Dallas <- unique(checkIn_all[checkIn_all$venueCity== "Dallas",])
nrow(checkIn_Dallas)
dallas_RSMean <- checkIn_Dallas %>% group_by(venueId) %>% mutate(meanRS = mean(ratingScore))%>% ungroup()

dallasVenues <- sqldf('SELECT distinct venueId,venueName,meanRS, venueLat,venueLng FROM dallas_RSMean where venueId != "NA"')
nrow(dallasVenues)
head(dallasVenues)
write.csv(dallasVenues,file="dvtest.csv")

data_json <- geojson_read("Dallas City Limits GIS Layer.geojson", what = "sp")
plot(data_json)

p <- ggplot()+
ggtitle("Dallas Venues Average Rating score")+ 
xlab("Venue Longitude")+
ylab("Venue Latitude")
p <- p +theme(
  plot.title = element_text(color="red", size=14, face="bold.italic",hjust=0,vjust=1),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)
p <- p + geom_polygon( data=data_json,aes(x=long, y=lat, group = group),colour="black",fill="grey" )
p


p<- p+ geom_point(aes(x = venueLng, y = venueLat,color=cut(meanRS, c(-Inf,0,1,2,3,4,5,Inf))), data = dallasVenues,shape=18,size=2)
p
p <- p +scale_color_manual(name = "Rating Score Scale",
                           values = c("(-Inf,0]" = "black",
                                      "(0,1]" = "coral1",
                                      "(1,2]" = "red",
                                      "(2,3]" = "green",
                                      "(3,4]" = "blue",
                                      "(4,5]" = "yellow",
                                      "(5, Inf]" = "violet"),
                           labels = c("<= 0", "0 to 1", "1 to 2","2 to 3","3 to 4","4 to 5"," >=5"))
p <- p+geom_jitter(width = .5, size=1)
p

