
##지역별 통화량

setwd("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/배달통화량/2018")


document = dir()

df = data.frame()
for(i in 1:12) {
  temp = fread(document[i], encoding = "UTF-8")
  df = rbind(df, temp)
  print(i)
}

df
df$시도 %>% unique()
df$시군구 %>% unique()
df$읍면동 %>% unique()

df %>% str
df$month = substr(as.character(df$일자), 1, 6)

df2018 = df %>%  group_by(시도, 시군구, 읍면동) %>% summarise(call_2018 = sum(통화건수))
df2018

df2 %>% group_by(month) %>% summarise(sum = sum(call))



setwd("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/배달통화량/2017")


document = dir()

df = data.frame()
for(i in 1:12) {
  temp = fread(document[i], encoding = "UTF-8")
  df = rbind(df, temp)
  print(i)
}

df
df$시도 %>% unique()
df$시군구 %>% unique()
df$읍면동 %>% unique()

df %>% str
df$month = substr(as.character(df$일자), 1, 6)

df2017 = df %>%  group_by(시도, 시군구, 읍면동) %>% summarise(call_2017 = sum(통화건수))
df2017 = df2017 %>%  filter(시도 == "서울특별시")




full_df = merge(df2017, df2018) 
full_df %>% filter(읍면동 == "봉천동")




full_df


library(maps)
library(knitr)
#install.packages("rgdal")
library(rgdal)
library(maptools)
#install.packages("rgeos")
library(rgeos)
library(raster)

library(gpclib)
library(rgdal)
library(maptools)

korea = shapefile("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/SIG_201804/TL_SCCO_SIG.shp")


korea<-spTransform(korea, CRS("+proj=longlat"))
map <- fortify(korea)
str(map)
map %>% tail()
map$id = as.numeric(map$id)

seoul_map = map[map$id <= 24,]



seoul_id = read.csv("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/seoul_id.csv")
names(seoul_id) = c("시군구", "code_id")
full_df = full_df %>% group_by(시군구) %>% summarise(call_2017 = sum(call_2017), call_2018 = sum(call_2018))

merge_df = merge(full_df, seoul_id)
merge_df = merge_df %>% arrange(code_id)
merge_df$id = 0:24

M = merge(seoul_map, merge_df, by ="id")

guname = M %>% group_by(시군구) %>% summarise(long = mean(long), lat = mean(lat), call_2017 = mean(call_2017), call_2018 = mean(call_2018))
guname$lat
guname$long

seoul_map %>% tail()

ggplot() + geom_polygon(data = M,
                        aes(x = long,
                            y = lat,
                            group = group,
                            fill = call_2018),
                        color = "white") +
  scale_fill_gradient(low = "#FBCF61",
                      high = "#00CC99",
                      space = "Lab",
                      guide = "colourbar") +
  labs(fill = "2018년 배달 통화량") +
  theme_void() +
  theme(legend.position = c(.15, .85)) +
  geom_text(data = guname,
            aes(x = long,
                y = lat,
                label = paste(시군구, call_2018, sep = "\n")))


options("scipen" = 100)

ggplot() + geom_polygon(data = M,
                        aes(x = long,
                            y = lat,
                            group = group,
                            fill = call_2017),
                        color = "white") +
  scale_fill_gradient(low = "#FBCF61",
                      high = "#00CC99",
                      space = "Lab",
                      guide = "colourbar") +
  labs(fill = "2017년 배달 통화량") +
  theme_void() +
  theme(legend.position = c(.15, .85)) +
  geom_text(data = guname,
            aes(x = long,
                y = lat,
                label = paste(시군구, call_2018, sep = "\n")))


