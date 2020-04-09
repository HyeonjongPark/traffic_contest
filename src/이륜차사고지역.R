
setwd("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/지역별이륜차사고")

acc = read.csv("twowheel-acc.csv")
acc %>% class

acc = acc[,c("year", "시군구", "합계", "이륜차")]

## 이륜차 사고 건수

acc2014 = acc %>% filter(year == 2014)
acc2014 = acc2014 %>% filter(시군구 != "합계")

acc2014

seoul_id = read.csv("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/seoul_id.csv")
names(seoul_id) = c("시군구", "code_id")
seoul_id
m_df = merge(acc2014, seoul_id)
m_df
m_df = m_df %>% arrange(code_id)
m_df$id = 0:24



korea = shapefile("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/SIG_201804/TL_SCCO_SIG.shp")

korea<-spTransform(korea, CRS("+proj=longlat"))
map <- fortify(korea)
str(map)
map %>% tail()
map$id = as.numeric(map$id)
map
seoul_map = map[map$id <= 24,]




M = merge(seoul_map, m_df, by ="id")
M %>% str
M$이륜차 = as.integer(as.character(M$이륜차))


guname = M %>% group_by(시군구) %>% summarise(long = mean(long), lat = mean(lat), two_acc = mean(이륜차))
guname$lat
guname$long




ggplot() + geom_polygon(data = M,
                        aes(x = long,
                            y = lat,
                            group = group,
                            fill = 이륜차),
                        color = "white") +
  scale_fill_gradient(low = "#FBCF61",
                      high = "#00CC99",
                      space = "Lab",
                      guide = "colourbar") +
  labs(fill = "2014년 이륜차 사고 지역") +
  theme_void() +
  theme(legend.position = c(.15, .85)) +
  geom_text(data = guname,
            aes(x = long,
                y = lat,
                label = paste(시군구, two_acc, sep = "\n")))






## 전체 사고 건수
acc %>% str
setwd("C:/Users/onycom/Desktop/competition-info/교통기획/시각화자료")
for(i in 2014:2018) {
  
  acc2014 = acc %>% filter(year == i)
  acc2014 = acc2014 %>% filter(시군구 != "합계")
  
  acc2014
  
  seoul_id = read.csv("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/seoul_id.csv")
  names(seoul_id) = c("시군구", "code_id")
  
  m_df = merge(acc2014, seoul_id)
  m_df = m_df %>% arrange(code_id)
  
  m_df$이륜차 = gsub(",","",m_df$이륜차)
  m_df$합계 = gsub(",","",m_df$합계)
  
  m_df$id = 0:24
  
  
  
  korea = shapefile("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/SIG_201804/TL_SCCO_SIG.shp")
  
  korea<-spTransform(korea, CRS("+proj=longlat"))
  map <- fortify(korea)
  
  map$id = as.numeric(map$id)
  
  seoul_map = map[map$id <= 24,]
  
  
  
  
  M = merge(seoul_map, m_df, by ="id")
  M %>% str
  M$이륜차 = as.integer(as.character(M$이륜차))
  M$합계 = as.integer(as.character(M$합계))
  
  guname = M %>% group_by(시군구) %>% summarise(long = mean(long), lat = mean(lat), two_acc = mean(합계))

  ggplot() + geom_polygon(data = M,
                          aes(x = long,
                              y = lat,
                              group = group,
                              fill = 합계),
                          color = "white") +
#    scale_fill_gradient(low = "#FBCF61",
#                        high = "#00CC99",
#                        space = "Lab",
#                        guide = "colourbar") +
    scale_fill_gradient(low = "#FFDFD3",
                        high = "#D291BC",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = paste0(i,"년 사고 지역")) +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    geom_text(data = guname,
              aes(x = long,
                  y = lat,
                  label = paste(시군구, two_acc, sep = "\n")))
  
  ggsave(paste0(i,"사고합계.png"))
  
  
}




## 전체 사고 대비 이륜차 사고율



## 전체 사고 건수

setwd("C:/Users/onycom/Desktop/competition-info/교통기획/시각화자료")
for(i in 2014:2018) {
  
  acc2014 = acc %>% filter(year == i)
  acc2014 = acc2014 %>% filter(시군구 != "합계")
  
  acc2014
  
  seoul_id = read.csv("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/seoul_id.csv")
  names(seoul_id) = c("시군구", "code_id")
  
  m_df = merge(acc2014, seoul_id)
  m_df = m_df %>% arrange(code_id)
  
  m_df$이륜차 = gsub(",","",m_df$이륜차)
  m_df$합계 = gsub(",","",m_df$합계)
  
  m_df$이륜차 = as.integer(m_df$이륜차)
  m_df$합계 = as.integer(m_df$합계)
  
  m_df$ratio = (m_df$이륜차 / m_df$합계) * 100
  
  m_df$id = 0:24
  
  
  
  korea = shapefile("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/SIG_201804/TL_SCCO_SIG.shp")
  
  korea<-spTransform(korea, CRS("+proj=longlat"))
  map <- fortify(korea)
  
  map$id = as.numeric(map$id)
  
  seoul_map = map[map$id <= 24,]
  
  
  
  
  M = merge(seoul_map, m_df, by ="id")
  M %>% str
  #M$이륜차 = as.integer(as.character(M$이륜차))
  #M$합계 = as.integer(as.character(M$합계))
  #M$ratio = as.integer(as.character(M$ratio))
  
  guname = M %>% group_by(시군구) %>% summarise(long = mean(long), lat = mean(lat), two_acc = mean(ratio))
  
  ggplot() + geom_polygon(data = M,
                          aes(x = long,
                              y = lat,
                              group = group,
                              fill = round(ratio,2)),
                          color = "white") +
    #    scale_fill_gradient(low = "#FBCF61",
    #                        high = "#00CC99",
    #                        space = "Lab",
    #                        guide = "colourbar") +
    scale_fill_gradient(low = "#FFDFD3",
                        high = "#D291BC",
                        space = "Lab",
                        guide = "colourbar") +
    labs(fill = paste0(i,"년 전체 사고 대비 이륜차 사고율")) +
    theme_void() +
    theme(legend.position = c(.15, .85)) +
    geom_text(data = guname,
              aes(x = long,
                  y = lat,
                  label = paste(시군구, paste0(round(two_acc,2),"%"), sep = "\n")))
  
  ggsave(paste0(i,"년_전체대비_이륜차_사고율.png"))
  
  
}

