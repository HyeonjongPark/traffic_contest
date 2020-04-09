library(data.table)
library(dplyr)

setwd("C:/Users/onycom/Desktop/competition-info/교통기획/데이터/가해운전자특성별/")

data2014 = fread("./도로교통공단_가해운전자특성별_교통사고(2014)/도로교통공단_가해운전자_차량용도별_월별_교통사고(2014).csv")
moto_2014 = data2014 %>% filter(가해자차량용도 == "이륜차", 발생월 == "사고건수")
moto_2014 = as.data.frame(moto_2014[,4:15] %>% t())

moto_2014 = gsub(",","",moto_2014$V1) %>% as.integer()
a = sum(moto_2014[is.na(moto_2014) == F]) # 2014 이륜차 사고건수


old2014 = fread("./도로교통공단_가해운전자특성별_교통사고(2014)/도로교통공단_가해운전자_연령층별_월별_교통사고(2014).csv")
old2014 = old2014 %>% filter(발생월 == "사고건수") 
old2014[,2] == NULL
old2014


data2015 = fread("./도로교통공단_가해운전자특성별_교통사고(2015)/도로교통공단_가해운전자_차량용도별_월별_교통사고(2015).csv")
moto_2015 = data2015 %>% filter(차량용도 == "이륜차")
moto_2015 = moto_2015$발생건수

b = sum(moto_2015[is.na(moto_2015) == F]) # 2014 이륜차 사고건수

old2015 = fread("./도로교통공단_가해운전자특성별_교통사고(2015)/도로교통공단_가해운전자_연령층별_월별_교통사고(2015).csv")
old2015 = old2015 %>% group_by(연령층) %>% summarise(year_acc = sum(발생건수))
old2015$year = 2015



data2016 = fread("./도로교통공단_가해운전자특성별_교통사고(2016)/도로교통공단_가해운전자_차량용도별_월별_교통사고(2016).csv")
moto_2016 = data2016 %>% filter(차량용도 == "이륜차")
moto_2016 = moto_2016[,5]

c = sum(moto_2016[is.na(moto_2016) == F]) # 2014 이륜차 사고건수

old2016 = fread("./도로교통공단_가해운전자특성별_교통사고(2016)/도로교통공단_가해운전자_연령층별_월별_교통사고(2016).csv")
old2016 = old2016 %>% group_by(연령층) %>% summarise(year_acc = sum(발생건수))
old2016$year = 2016



data2017 = fread("./도로교통공단_가해운전자특성별_교통사고(2017)/도로교통공단_가해운전자_차량용도별_월별_교통사고(2017).csv")
moto_2017 = data2017 %>% filter(차량용도 == "이륜차")
moto_2017 = moto_2017[,5]

d = sum(moto_2017[is.na(moto_2017) == F]) # 2014 이륜차 사고건수



old2017 = fread("./도로교통공단_가해운전자특성별_교통사고(2017)/도로교통공단_가해운전자_연령층별_월별_교통사고(2017).csv")
old2017 = old2017 %>% group_by(연령층) %>% summarise(year_acc = sum(발생건수))
old2017$year = 2017


data2018 = fread("./도로교통공단_가해운전자특성별_교통사고_통계(2018)/도로교통공단_가해운전자_차량용도별_월별_교통사고(2018).csv")
moto_2018 = data2018 %>% filter(차량용도 == "이륜차")
moto_2018 = moto_2018[,5]

e = sum(moto_2018[is.na(moto_2018) == F]) # 2014 이륜차 사고건수


old2018 = fread("./도로교통공단_가해운전자특성별_교통사고_통계(2018)/도로교통공단_가해운전자_연령층별_월별_교통사고(2018).csv")
old2018 = old2018 %>% group_by(연령층) %>% summarise(year_acc = sum(발생건수))
old2018$year = 2018



df_year = data.frame(년도 = c(2014:2018),
                  사고건수 = c(a,b,c,d,e))
df_year %>% ggplot(aes(x = 년도, y = 사고건수, fill = 사고건수)) + 
  geom_bar(stat = "identity", colour = "red") +
  geom_line(color = "red") +
  geom_point(color = "red", size = 5) +
  ggtitle("년도별 이륜차 사고건수") + 
  theme(plot.title = element_text(color = " red", size = 14, face ="bold.italic"))



df_month = rbind(moto_2014, moto_2015, moto_2016, moto_2017, moto_2018)
colnames(df_month) = 1:12
df_month %>% t()


df_old = rbind(old2015, old2016, old2017, old2018)

df_old %>% filter(연령층 == "21-30세") %>% group_by(year) %>% ggplot(aes(x = year, y = year_acc, fill = year_acc)) + geom_bar(stat = "identity")
df_old %>% filter(연령층 == "31-40세") %>% group_by(year) %>% ggplot(aes(x = year, y = year_acc, fill = year_acc)) + geom_bar(stat = "identity")
df_old %>% filter(연령층 == "41-50세") %>% group_by(year) %>% ggplot(aes(x = year, y = year_acc, fill = year_acc)) + geom_bar(stat = "identity")
df_old %>% filter(연령층 == "51-60세") %>% group_by(year) %>% ggplot(aes(x = year, y = year_acc, fill = year_acc)) + geom_bar(stat = "identity")
df_old %>% filter(연령층 == "61-64세") %>% group_by(year) %>% ggplot(aes(x = year, y = year_acc, fill = year_acc)) + geom_bar(stat = "identity")
df_old %>% filter(연령층 == "65세이상") %>% group_by(year) %>% ggplot(aes(x = year, y = year_acc, fill = year_acc)) + geom_bar(stat = "identity")



