setwd("D:/traffic-contest")

library(dplyr)

#remove.packages("ggmap")
#remove.packages("tibble")

library('devtools')
#install_github('dkahle/ggmap', ref="tidyup")

library('ggmap')

## 구글 map api 등록
register_google(key = 'AIzaSyCPMeJm2vE2IhX9F2klU0HnvDVlczctQnU')



airport <- read.csv("airports.dat", sep=",", header=F, stringsAsFactor = F)

route <- read.csv("routes.dat", sep=",", header=F, stringsAsFactor = F)


airport <- airport[airport$V5!='', c('V3', 'V4', 'V5', 'V7', 'V8')]
airport %>% head

names(airport) <- c("City", "Country", "IATA", "lantitude", "longitude")

route <- route[c('V1', 'V3', 'V5')]

names(route) <- c("Airline", "Departure", "Arrival")

airport.krjp <- subset(airport, Country %in% c("South Korea", "Japan"))

route.krjp <- subset(route, (Departure %in% airport.krjp$IATA & Arrival %in% airport.krjp$IATA))

findposition <- function(IATA) {
  
  find <- airport.krjp$IATA==IATA
  
  x <- airport.krjp['find','longitude']
  
  y <- airport.krjp['find', 'lantitude']
  
  return(data.frame(x,y)) }



from <- lapply(route.krjp$Departure, findposition)
from <- do.call('rbind', from)
from$group <- c(1:nrow(from))
names(from) <- c("longitude",'lantitude','group')


to <- lapply(route.krjp$Arrival, findposition)
to <- do.call('rbind', to)
to$group <- c(1:nrow(to))
names(to) <- c('longitude','lantitude','group')
data.line <- rbind(from,to)



Arriving.freq <- data.frame(table(route.krjp$Arrival))
names(Arriving.freq) <- c('IATA', 'Freq')
airport.krjp <- merge(airport.krjp, Arriving.freq, by = c("IATA"), all=T)
airport.lkrjp <- airport.krjp[order(airport.krjp$Freq, decreasing=T),]



map <- ggmap(get_googlemap(center = c(lon=134, lat=36),
                           
                           zoom=5, maptype='roadmap', color='bw', scale='2'), extent='device')



map + geom_point(data=airport.krjp, aes(x=longitude, y=lantitude, size=Freq),
                 
                 colour = 'gray10', alpha=0.3) +
  
  geom_line(data=data.line, aes(x=longitude, y=lantitude, group=group),
            
            size=0.2, alpha=.1, color = '#816960') +
  
  scale_size(range=c(0,15)) + theme(legend.position="none")



