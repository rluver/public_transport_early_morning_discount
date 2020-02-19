require("data.table")
require("dplyr")
require("tidyr")
require("stringr")
require("lubridate")
require("forecast")
require("tsoutliers")
require("ggplot2")
require("plotly")




bus2015 = fread("D:/대중교통/2015년_버스노선별_정류장별_시간대별_승하차_인원_정보.csv")
bus2016 = fread("D:/대중교통/2016년_버스노선별_정류장별_시간대별_승하차_인원_정보.csv")
bus2017 = fread("D:/대중교통/2017년_버스노선별_정류장별_시간대별_승하차_인원_정보.csv")
bus2018 = fread("D:/대중교통/2018년_버스노선별_정류장별_시간대별_승하차_인원_정보.csv")

subway2015 = fread("D:/대중교통/서울교통공사 2015년 일별 역별 시간대별 승하차인원(1_8호선).csv")
subway2016 = fread("D:/대중교통/서울교통공사 2016년 일별 역별 시간대별 승하차인원(1_8호선).csv")
subway2017 = fread("D:/대중교통/서울교통공사 2017년 일별 역별 시간대별 승하차인원(1_8호선).csv")
subway2018 = fread("D:/대중교통/서울교통공사 2018년 일별 역별 시간대별 승하차인원(1_8호선).csv")



bus = bind_rows(bus2015 %>% select(1, 19, 21, 23, 25) %>% 
                  group_by(use_mon) %>% 
                  summarise(`05~06` = sum(ride_05_pasgr_num),
                            `06~07` = sum(ride_06_pasgr_num),
                            `07~08` = sum(ride_07_pasgr_num),
                            `08~09` = sum(ride_08_pasgr_num)),
                bus2016 %>% select(1, 19, 21, 23, 25) %>% 
                  group_by(use_mon) %>% 
                  summarise(`05~06` = sum(ride_05_pasgr_num),
                            `06~07` = sum(ride_06_pasgr_num),
                            `07~08` = sum(ride_07_pasgr_num),
                            `08~09` = sum(ride_08_pasgr_num))) %>% 
  bind_rows(bus2017 %>% select(1, 19, 21, 23, 25) %>% 
              group_by(use_mon) %>% 
              summarise(`05~06` = sum(ride_05_pasgr_num),
                        `06~07` = sum(ride_06_pasgr_num),
                        `07~08` = sum(ride_07_pasgr_num),
                        `08~09` = sum(ride_08_pasgr_num))) %>% 
  bind_rows(bus2018 %>% select(1, 19, 21, 23, 25) %>% 
              group_by(use_mon) %>% 
              summarise(`05~06` = sum(five_ride_num), 
                        `06~07` = sum(six_ride_num), 
                        `07~08` = sum(seven_ride_num), 
                        `08~09` = sum(eight_ride_num))) %>% 
  rename(date = use_mon) %>% 
  mutate(date = str_c(str_sub(date, 1, 4), str_sub(date, 5, 6), sep = "-"))
  


subway = bind_rows(subway2015 %>% group_by(날짜) %>% filter(구분 == "승차") %>% 
                     select(1, 5:8) %>%
                     mutate(`05~06` = as.numeric(str_replace(`05~06`, ",", "")), 
                            `06~07` = as.numeric(str_replace(`06~07`, ",", "")), 
                            `07~08` = as.numeric(str_replace(`07~08`, ",", "")), 
                            `08~09` = as.numeric(str_replace(`08~09`, ",", ""))) %>% 
                     summarise(`05~06` = sum(`05~06`),
                               `06~07` = sum(`06~07`),
                               `07~08` = sum(`07~08`),
                               `08~09` = sum(`08~09`)),
                   subway2016 %>% group_by(날짜) %>% filter(구분 == "승차") %>% 
                     select(1, 5:8) %>%
                     mutate(`05~06` = as.numeric(str_replace(`05~06`, ",", "")), 
                            `06~07` = as.numeric(str_replace(`06~07`, ",", "")), 
                            `07~08` = as.numeric(str_replace(`07~08`, ",", "")), 
                            `08~09` = as.numeric(str_replace(`08~09`, ",", ""))) %>% 
                     summarise(`05~06` = sum(`05~06`),
                               `06~07` = sum(`06~07`),
                               `07~08` = sum(`07~08`),
                               `08~09` = sum(`08~09`))) %>% 
  bind_rows(subway2017 %>% group_by(날짜) %>% filter(구분 == "승차") %>% 
              select(1, 7:10) %>%
              mutate(`05~06` = as.numeric(str_replace(`05~06`, ",", "")), 
                     `06~07` = as.numeric(str_replace(`06~07`, ",", "")), 
                     `07~08` = as.numeric(str_replace(`07~08`, ",", "")), 
                     `08~09` = as.numeric(str_replace(`08~09`, ",", ""))) %>% 
              summarise(`05~06` = sum(`05~06`),
                        `06~07` = sum(`06~07`),
                        `07~08` = sum(`07~08`),
                        `08~09` = sum(`08~09`))) %>% 
  bind_rows(subway2018 %>% filter(구분 == "승차") %>% select(1, 6:9) %>%
              magrittr::set_names(c("날짜", str_replace_all(names(.)[2:5], " ", ""))) %>% 
              group_by(날짜) %>% 
              mutate(`05~06` = as.numeric(str_replace(`05~06`, ",", "")), 
                     `06~07` = as.numeric(str_replace(`06~07`, ",", "")), 
                     `07~08` = as.numeric(str_replace(`07~08`, ",", "")), 
                     `08~09` = as.numeric(str_replace(`08~09`, ",", ""))) %>% 
              summarise(`05~06` = sum(`05~06`),
                        `06~07` = sum(`06~07`),
                        `07~08` = sum(`07~08`),
                        `08~09` = sum(`08~09`))) %>% 
  rename(date = 날짜) %>% 
  mutate(date = ymd(date),
         year = str_sub(date, 1, 4))




# intervention analysis

# 2015 

# subway
# AO
# 05 ~ 06

((subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
  tso(types = "AO"))$outliers

(subway %>% group_by(date) %>% 
  filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(33, 83, 89, 121, 141, 185, 204), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
  tso(types = "AO") %>% 
  plot()


# 06 ~ 07

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "AO")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(33, 83, 156), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "AO") %>% 
  plot()


# 07 ~ 08

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "AO")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(1, 33, 83, 250), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "AO") %>% 
  plot()


# 08 ~ 09

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "AO")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(83, 250), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "AO") %>% 
  plot()



# TC
# 05 ~ 06

((subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
    tso(types = "TC"))$outliers

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(83, 185), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
  tso(types = "TC") %>% 
  plot()


# 06 ~ 07

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "TC")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[83, ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "TC") %>% 
  plot()


# 07 ~ 08

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "TC")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(2, 83), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "TC") %>% 
  plot()


# 08 ~ 09

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "TC")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(33, 83, 147, 185, 250), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "TC") %>% 
  plot()



# LS
# 05 ~ 06

((subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
    tso(types = "LS"))$outliers

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(84, 185, 188), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
  tso(types = "LS") %>% 
  plot()


# 06 ~ 07

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "LS")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[88, ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "LS") %>% 
  plot()


# 07 ~ 08

try({(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "LS")})

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(2, 83), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "LS") %>% 
  plot()


# 08 ~ 09

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "LS")

# (subway %>% group_by(date) %>% 
#     filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
#     select(date))[c(33, 83, 147, 185, 250), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  plot()



# IO
# 05 ~ 06

try({(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
    tso(types = "IO")$outliers})

# (subway %>% group_by(date) %>% 
#     filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
#     select(date))[, ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`05~06`) %>% as.ts())[,2] %>% 
  # tso(types = "IO") %>% 
  plot()


# 06 ~ 07

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "IO")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(2, 83, 88, 156), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`06~07`) %>% as.ts())[,2] %>% 
  tso(types = "IO") %>% 
  plot()


# 07 ~ 08

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "IO")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(2, 39, 83, 85, 216, 250), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`07~08`) %>% as.ts())[,2] %>% 
  tso(types = "IO") %>% 
  plot()


# 08 ~ 09

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "IO")

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000 && year == 2015) %>% 
    select(date))[c(83, 185, 250), ]

(subway %>% group_by(date) %>% 
    filter(sum(`05~06`, `06~07`, `07~08`, `08~09`) >= 600000) %>% 
    filter(year == 2015) %>% select(`08~09`) %>% as.ts())[,2] %>% 
  tso(types = "IO") %>% 
  plot()




# trend analysis

# bus
# 05 ~ 06

ggplotly(bus %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "05~06") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)


# 06 ~ 07

ggplotly(bus %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "06~07") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)


# 07 ~ 08

ggplotly(bus %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "07~08") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)


# 08 ~ 09

ggplotly(bus %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "08~09") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)




# subway
# 05 ~ 06

ggplotly(subway %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "05~06") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)


# 06 ~ 07

ggplotly(subway %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "06~07") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)


# 07 ~ 08

ggplotly(subway %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "07~08") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)


# 08 ~ 09

ggplotly(subway %>% group_by(str_sub(.$date, 1, 7)) %>% 
           summarise(`05~06` = sum(`05~06`),
                     `06~07` = sum(`06~07`),
                     `07~08` = sum(`07~08`),
                     `08~09` = sum(`08~09`)) %>% 
           gather(key = time, value = num_ride, names(.)[-1]) %>% 
           rename(date = `str_sub(.$date, 1, 7)`) %>% 
           filter(time == "08~09") %>% 
           select(num_ride) %>% 
           ts(start = 2015, frequency = 12) %>% 
           decompose %>% autoplot)