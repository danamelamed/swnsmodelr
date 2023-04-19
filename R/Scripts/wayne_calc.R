daily_df_in <- read.csv('f:\\WeatherData\\daily_20110101_20180218.csv')
annual_df <- read.csv('f:\\WeatherData\\annual_20110101_20180218.csv')

daily_df <- daily_df_in %>% filter(year(date_time) == 2012) %>%
  filter(between(month(date_time),4,11)) %>%
  mutate(temp_mean2 = (temp_min + temp_max)/2) %>%
  mutate(gdd10_daily = ifelse(temp_mean2>10,temp_mean2-10,0)) %>%
  group_by(stationid) %>%
  mutate(gdd10an = sum(round(gdd10_daily,0))) %>%
  filter(date_time == max(date_time))# %>%
 # select(date_time,stationid,gdd10an_total, gdd10an, gdd10, gdd10_daily)

daily_df <- daily_df %>% mutate(diff = gdd10an_total - gdd10an)

annual_df <- annual_df %>% filter(year(date_time)==2012)

# annual_df %>% select(date_time, stationid, gdd10an_total) %>%
#   filter(stationid == 'BR2')
# 
# daily_df %>% select(date_time, stationid, gdd10an) %>%
#   filter(stationid == 'BR2')

