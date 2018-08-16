setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

# install.packages("sqldf")
# install.packages("geojsonio")
library(ggplot2)
library(dplyr)
library(sqldf)
library(geojsonio)

filePath='data/checkins.csv'
checkIn_all <- read.csv(filePath)
summary(checkIn_all)
checkIn_Dallas <- checkIn_all[checkIn_all$venueCity== "Dallas",]
nrow(checkIn_Dallas)
dallas_RSMean <- checkIn_Dallas %>% group_by(venueId) %>% mutate(meanRS = mean(ratingScore))%>% ungroup()

dallasVenues <- sqldf("SELECT distinct venueId,venueName,meanRS, venueLat,venueLng FROM dallas_RSMean where venueId != 'NA'")
nrow(dallasVenues)
head(dallasVenues)
# write.csv(dallasVenues,file="data/dvtest.csv")

# data_json <- geojson_read("data/Dallas City Limits GIS Layer.geojson", what = "sp")
# plot(data_json$shape_area)
checkIn_Texas <- checkIn_all[checkIn_all$venueState== "TX",]
texas_RSMean <- checkIn_Texas %>% group_by(venueId) %>% mutate(meanRS = mean(ratingScore))%>% ungroup()
texasVenues <- sqldf('SELECT distinct venueId,venueName,meanRS, venueLat,venueLng FROM texas_RSMean where venueId != "NA"')

peticolasVenues <- sqldf("SELECT distinct venueId, venueName, meanRS, venueLat, venueLng FROM texas_RSMean WHERE breweryId =13688")
communityVenues <- sqldf("SELECT distinct venueId, venueName, meanRS, venueLat, venueLng FROM texas_RSMean WHERE breweryId =48372")


data_json <- geojson_read("data/Dallas City Limits GIS Layer.geojson", what = "sp")
p <- ggplot()+
    ggtitle("Dallas, Tx Beer Rating Activities")+ 
    xlab(" ")+
    ylab(" ")
p <- p +theme(
    plot.title = element_text(color="red", size=14, face="bold.italic",hjust=0,vjust=1),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
)
p <- p + geom_polygon( data=data_json,aes(x=long, y=lat, group = group),colour="black",fill="grey" )
p


p<- p+ geom_point(aes(x = venueLng, y = venueLat,color=meanRS), data = dallasVenues,shape=18,size=2)
p
p <- p + scale_color_gradient(low="#C0FAAB",high="#388A19")
# p <- p +scale_color_manual(name = "Rating Score Scale",
#                            values = c("(-Inf,0]" = "black",
#                                       "(0,1]" = "coral1",
#                                       "(1,2]" = "red",
#                                       "(2,3]" = "green",
#                                       "(3,4]" = "blue",
#                                       "(4,5]" = "yellow",
#                                       "(5, Inf]" = "violet"),
#                            labels = c("<= 0", "0 to 1", "1 to 2","2 to 3","3 to 4","4 to 5"," >=5"))
p <- p+geom_jitter(width = .5, size=1)
p
ggsave("DallasRatings.png",p)

# Texas
data_json <- geojson_read("https://raw.githubusercontent.com/TNRIS/tx.geojson/master/tx.geojson",what="sp")

## Peticolas
p <- ggplot()+
ggtitle("Peticolas Brewery Venues Average Rating score")+
xlab(" ")+
ylab(" ")
p <- p +theme(
  plot.title = element_text(color="red", size=14, face="bold.italic",hjust=0,vjust=1),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
)
p <- p + geom_polygon(
    data=data_json,
    aes(
        x=long, 
        y=lat, 
        group = group
    ),
    colour="black",
    fill="grey"
)
p


p<- p+ geom_point(
    aes(
        x = venueLng, 
        y = venueLat,
        color=meanRS
        # color=cut(meanRS, c(-Inf,0,1,2,3,4,5,Inf))
    ), 
    data = peticolasVenues,
    shape=18,size=2)
p

p <- p + scale_color_gradient(low="#C6FAB2",high="#42A11E")
# p <- p +scale_color_manual(name = "Rating Score Scale",
#                            values = c("(-Inf,0]" = "#67E438",
#                                       "(0,1]" = "#61D835",
#                                       "(1,2]" = "#59C930",
#                                       "(2,3]" = "#52BD2B",
#                                       "(3,4]" = "#4BAD28",
#                                       "(4,5]" = "#449F23",
#                                       "(5, Inf]" = "3D8F1F"),
#                            labels = c("<= 0", "0 to 1", "1 to 2","2 to 3","3 to 4","4 to 5"," >=5"))
# p <- p+geom_jitter(width = .5, size=1)
p

ggsave("PeticolasPlot.png",p)

## Community
p <- ggplot()+
    ggtitle("Community Brewery Venues Average Rating score")+
    xlab(" ")+
    ylab(" ")
p <- p +theme(
    plot.title = element_text(color="red", size=14, face="bold.italic",hjust=0,vjust=1),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
)
p <- p + geom_polygon(
    data=data_json,
    aes(
        x=long, 
        y=lat, 
        group = group
    ),
    colour="black",
    fill="grey"
)
p


p<- p+ geom_point(
    aes(
        x = venueLng, 
        y = venueLat,
        color=meanRS
        # color=cut(meanRS, c(-Inf,0,1,2,3,4,5,Inf))
    ), 
    data = communityVenues,
    shape=18,size=2)
p

p <- p + scale_color_gradient(low="#C6FAB2",high="#42A11E")
# p <- p +scale_color_manual(name = "Rating Score Scale",
#                            values = c("(-Inf,0]" = "#67E438",
#                                       "(0,1]" = "#61D835",
#                                       "(1,2]" = "#59C930",
#                                       "(2,3]" = "#52BD2B",
#                                       "(3,4]" = "#4BAD28",
#                                       "(4,5]" = "#449F23",
#                                       "(5, Inf]" = "3D8F1F"),
#                            labels = c("<= 0", "0 to 1", "1 to 2","2 to 3","3 to 4","4 to 5"," >=5"))
# p <- p+geom_jitter(width = .5, size=1)
p

ggsave("CommunityPlot.png",p)
    