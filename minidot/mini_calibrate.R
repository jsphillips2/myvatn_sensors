#==========
#========== Preliminaries
#==========

# load packages
library(tidyverse)
library(lubridate)
Sys.setenv(TZ = "GMT")

# set cores for parallel
options(mc.cores = parallel::detectCores()-2)

# import data
minidot <- list.files("minidot/raw") %>%
  
  parallel::mclapply(function(x){read_csv(paste0("minidot/raw/",x), skip = 8) %>% # read files
      set_names(read_csv(paste0("minidot/raw/",x), skip = 7) %>% names()) %>% # set column names
      mutate(md = str_split(x, "_") %>% map_chr(~.x[1])) # parse names
  }) %>%
  bind_rows() %>%
  rename(date_time = "UTC_Date_&_Time",
         temp = "Temperature",
         do = "Dissolved Oxygen",
         do_sat = "Dissolved Oxygen Saturation",
         q = Q) %>%
  select(md, date_time,  temp,  do, do_sat, q) %>%
  mutate(
    date_time = as_datetime(date_time, tz = "GMT"),
    # tempreature in Kelvin
    temp_k = temp + 273.15,
    # Schmidt number and associated conversion from CO2 to O2
    # based on Wanninkhoff 1992; Holtgrieve et al 2010; Staehr
    sch_o2 = 1800.6 - 120.10*temp + 3.7818*temp^2 - 0.047608*temp^3,
    sch_conv = (sch_o2/600)^(-0.5),
    # O2 solubility in mL/L
    # based on Weiss 1970
    do_sol = exp(-173.4292 + 249.6339*(100/temp_k) + 143.3483*log(temp_k/100) - 21.8492*(temp_k/100)),
    # convert to mg/L of DO
    # use ideal gas law, solved for the number of moles n = P*V/(R*T)
    # use pressure in kPA corrected for Myvatn's elevation of ~300m = 98 kPA
    # use R = 8.3144598 L*kPa/(K*mol)  
    # use O2 molar mass of 2*32; multiply by 1000 to convert from g to mg 
    do_eq = 32*(98*do_sol)/(8.3144598*temp_k)
  ) %>%
  arrange(md, date_time)

# import metadata
meta <- read_csv("minidot/deployment_log.csv", col_types = c("cccddDtDtddcdc"))

# import sonde data
sonde <- read_csv("sonde/sonde_clean.csv", col_types = c("Tdddddd"))





#==========
#========== Summer 2017====
#==========

# identify calibration date/md
cal17 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2017,
         calibrating == "y")

# extract sonde data
sonde_cal17 <- sonde %>%
  mutate(year  = year(date_time)) %>%
  filter(year == 2017,
         as.Date(date_time) > cal17$date_in & as.Date(date_time) <= cal17$date_out)

# extract minidot data
mini_cal17 <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal17) %>%
  filter(as.Date(date_time) > date_in & as.Date(date_time) <= date_out)

# plot temp
sonde_cal17 %>%
  select(date_time,  temp) %>%
  mutate(type  = "sonde") %>%
  bind_rows(mini_cal17 %>%
              select(date_time,  temp) %>%
              mutate(type  = "minidot")) %>%
  filter(date_time > "2017-06-16 11:00:00" & date_time <= "2017-06-20 10:00:00") %>%
  ggplot(aes(date_time,  temp, color  = type))+
  geom_line()+
  theme_bw()
  
# plot temp
sonde_cal17 %>%
  select(date_time,  do) %>%
  mutate(type  = "sonde") %>%
  bind_rows(mini_cal17 %>%
              select(date_time,  do) %>%
              mutate(type  = "minidot")) %>%
  filter(date_time > "2017-06-16 11:00:00" & date_time <= "2017-06-20 10:00:00") %>%
  ggplot(aes(date_time,  do, color  = type))+
  geom_line()+
  theme_bw()  

# create calibration data
cal_data17 <- sonde_cal17 %>%
  select(date_time,  do) %>%
  rename(do_sonde = do) %>%
  left_join(mini_cal17 %>%
              select(date_time,  do) %>%
              rename(do = do) %>%
              mutate(date_time =  date_time  +  2*(60))) %>%
  filter(date_time > "2017-06-16 11:00:00" & date_time <= "2017-06-20 10:00:00") %>%
  summarize(corr = mean(do_sonde/do, na.rm = T))

# plot
cal_data17  %>%
  ggplot(aes(do, do_sonde))+
  geom_point()+
  theme_bw()

# correct mini17
minidot17_correct <- minidot %>%
  mutate(year  = year(date_time),
         date = date(date_time)) %>%
  inner_join(meta %>%
               mutate(year  = year(date_in)) %>%
               filter(year == 2017, date_in %in% c(as.Date("2017-06-30"), as.Date("2017-07-13"))))  %>% 
  filter(date(date_time) >= date_in & date(date_time) <= date_out) %>%
  arrange(date_time) %>%
  mutate(do_cor = cal_data17$corr*do,
         cal_group = "summer17") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)

# examine
minidot17_correct %>%
  ggplot(aes(date_time, do))+
  geom_line(size = 0.8)+
  geom_line(aes(y = do_cor), color = "red", size = 0.8)+
  theme_bw()

# for comparison to bucket test, calculate correction factor
minidot17_correct %>%
  summarize(corr = mean(mean(do)/do))


#==========
#========== 2018 buckets=====
#==========

#### June

# extract calibration date/md
buck_jun18 <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(meta %>%
               mutate(year  = year(date_in)) %>%
               filter(year == 2018,
                      calibrating == "y",
                      date_in == "2018-06-10")) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out) %>%
  arrange(date_time)


# plot
buck_jun18 %>%
  filter(date_time >= "2018-06-10 18:00:00" & date_time <= "2018-06-11 9:00:00") %>%
  select(date_time, do, temp, md) %>%
  gather(var, val, do, temp) %>%
  group_by(md) %>%
  ggplot(aes(date_time, val, color = md))+
  facet_wrap(~var, nrow = 2, scales = "free")+
  geom_line()+
  theme_bw()

# calculate correction factor
corr_jun18 <- buck_jun18 %>%
  filter(date_time >= "2018-06-10 18:00:00" & date_time <= "2018-06-11 9:00:00") %>%
  mutate(do_mean = mean(do)) %>%
  group_by(md) %>%
  summarize(corr = mean(do_mean/do))


#### August
buck_aug18 <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(meta %>%
               mutate(year  = year(date_in)) %>%
               filter(year == 2018,
                      calibrating == "y",
                      date_in == "2018-08-17")) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out) %>%
  arrange(date_time)

# plot
buck_aug18 %>%
  filter(date_time >= "2018-08-18 00:00:00" & date_time <= "2018-08-18 16:00:00") %>%
  select(date_time, do, temp, md) %>%
  gather(var, val, do, temp) %>%
  group_by(md) %>%
  ggplot(aes(date_time, val, color = md))+
  facet_wrap(~var, nrow = 2, scales = "free")+
  geom_line()+
  theme_bw()

# calculate correction factor
corr_aug18 <- buck_aug18 %>%
  filter(date_time >= "2018-08-18 00:00:00" & date_time <= "2018-08-18 16:00:00") %>%
  mutate(do_mean = mean(do)) %>%
  group_by(md) %>%
  summarize(corr = mean(do_mean/do))




#==========
#========== 2017 winter correction====
#==========


# identify winter 2017 date/md
win17 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2017,
         date_in >= "2017-10-01")

# extract deployment data and correct
mini_win17 <- minidot %>%
  inner_join(win17) %>%
  filter(date_time > date_in & date_time <= date_out) %>%
  left_join(corr_jun18) %>%
  mutate(do_cor = corr*do) %>%
  mutate(cal_group  = "june18") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)

# plot
mini_win17 %>%
  gather(var, val, do, do_cor) %>%
  ggplot(aes(date_time, val, color = var))+
  facet_wrap(~site)+
  geom_line()+
  theme_bw()






#==========
#========== 2018 summer correction
#==========


# identify winter 2017 date/md
sum18 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2018,
         date_in >= "2018-06-13" & date_in <= "2018-08-15")

# extract deployment data and correct
mini_sum18 <- minidot %>%
  inner_join(sum18) %>%
  filter(date_time > date_in & date_time <= date_out) %>%
  left_join(corr_jun18) %>%
  mutate(do_cor = corr*do) %>%
  mutate(cal_group  = "june18") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)

# plot
mini_sum18 %>%
  gather(var, val, do, do_cor) %>%
  ggplot(aes(date_time, val, color = var))+
  facet_grid(layer~site)+
  geom_line()+
  theme_bw()




#==========
#========== 2018 winter correction
#==========


# identify winter 2018 date/md
win18 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2018,
         date_in >= "2018-08-19" & date_in <= "2019-06-11")

# extract deployment data and correct
mini_win18 <- minidot %>%
  inner_join(win18) %>%
  filter(date_time > date_in & date_time <= date_out) %>%
  left_join(corr_aug18) %>%
  mutate(do_cor = corr*do) %>%
  mutate(cal_group  = "aug18") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)

# Temp
mini_win18 %>%
  ggplot(aes(date_time, temp))+
  facet_wrap(~site, nrow = 2)+
  geom_vline(data = mini_win18 %>%
               mutate(year = year(date_time)) %>%
               group_by(site) %>%
               filter(year == 2019, temp > 5) %>%
               summarize(date_time = min(date_time)),
             aes(xintercept = date_time), color = "red")+
  geom_hline(yintercept = 5, color = "red")+
  geom_line()+
  theme_bw()

# plot DO
mini_win18 %>%
  gather(var, val, do, do_cor) %>%
  ggplot(aes(date_time, val))+
  facet_wrap(~site, nrow = 2)+
  geom_vline(data = mini_win18 %>%
               mutate(year = year(date_time)) %>%
               group_by(site) %>%
               filter(year == 2019, temp > 5) %>%
               summarize(date_time = min(date_time)),
             aes(xintercept = date_time), color = "red")+
  geom_line()+
  theme_bw()






#==========
#========== 2019 summer calibration (first day)
#==========

# identify summer 2019 date/md
cal_sum19a <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2019, calibrating == "y", site != "buck",
         date_in == "2019-06-10")



# extract sonde data
sonde_cal_sum19a <- sonde %>%
  mutate(year  = year(date_time)) %>%
  filter(as.Date(date_time) >= cal_sum19a$date_in & as.Date(date_time) <= cal_sum19a$date_out)

# extract minidot data
mini_cal_sum19a <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal_sum19a) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out)

# plot temp
sonde_cal_sum19a %>%
  filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
  select(date_time,  temp) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum19a %>%
              filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
              select(md, date_time,  temp)) %>%
  ggplot(aes(date_time,  temp, color  = md))+
  geom_line()+
  theme_bw()

# plot temp
sonde_cal_sum19a %>%
  filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
  select(date_time,  do) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum19a %>%
              filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
              select(md, date_time,  do)) %>%
  ggplot(aes(date_time,  do, color  = md))+
  geom_line()+
  theme_bw()

mini_cal_sum19a %>%
  filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
  select(md, date_time,  do) %>%
  mutate(date_time  = round_date(date_time, unit = "hour")) %>%
  group_by(md, date_time) %>%
  summarize(do = mean(do)) %>%
  full_join(sonde_cal_sum19a %>%
              filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
              select(date_time,  do) %>%
              mutate(date_time  = round_date(date_time, unit = "hour")) %>%
              group_by(date_time) %>%
              summarize(do_sonde = mean(do))) %>%
  group_by(md) %>%
  summarize(corr = mean(do_sonde/do))

match_minis_cal_sum19a <- mini_cal_sum19a %>%
  filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
  select(md, date_time,  do) %>%
  mutate(hour  = round_date(date_time, unit = "hour")) %>%
  left_join(sonde_cal_sum19a %>%
              filter(date_time >= "2019-06-10 20:00" & date_time <= "2019-06-11 12:00") %>%
              select(date_time,  do) %>%
              mutate(hour  = round_date(date_time, unit = "hour")) %>%
              rename(date_time_sonde = date_time,
                     do_sonde = do)) %>%
  mutate(dist = abs(date_time_sonde - date_time)) %>%
  group_by(md, hour) %>%
  filter(dist == min(dist)) %>%
  filter(date_time == min(date_time)) %>%
  ungroup() %>%
  select(md, hour, date_time, do, do_sonde) 

match_minis_cal_sum19a %>%
  gather(var, val, do, do_sonde) %>%
  mutate(md = ifelse(var == "do_sonde","sonde",md)) %>%
  ggplot(aes(date_time,  val, color  = md))+
  geom_point()+
  geom_line()+
  geom_line(aes(group = hour), color = "black")+
  theme_bw()

corr_sum19a <- match_minis_cal_sum19a %>%
  group_by(md) %>%
  summarize(corr = mean(do_sonde/do))






#==========
#========== 2019 summer calibration (second day)
#==========

# identify summer 2019 date/md
cal_sum19b <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2019, calibrating == "y", site != "buck",
         date_in == "2019-06-15")



# extract sonde data
sonde_cal_sum19b <- sonde %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal_sum19b) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out)

# extract minidot data
mini_cal_sum19b <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal_sum19b) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out)

# plot temp
sonde_cal_sum19b %>%
  filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-16 11:00") %>%
  select(date_time,  temp) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum19b %>%
              filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-16 11:00") %>%
              select(md, date_time,  temp)) %>%
  ggplot(aes(date_time,  temp, color  = md))+
  geom_line()+
  theme_bw()

# plot do
sonde_cal_sum19b %>%
  filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-15 18:30") %>%
  select(date_time,  do) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum19b %>%
              filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-15 18:30") %>%
              select(md, date_time,  do)) %>%
  ggplot(aes(date_time,  do, color  = md))+
  geom_line()+
  theme_bw()

mini_cal_sum19b %>%
  filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-15 18:30") %>%
  select(md, date_time,  do) %>%
  mutate(date_time  = round_date(date_time, unit = "hour")) %>%
  group_by(md, date_time) %>%
  summarize(do = mean(do)) %>%
  full_join(sonde_cal_sum19b %>%
              filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-15 18:30") %>%
              select(date_time,  do) %>%
              mutate(date_time  = round_date(date_time, unit = "hour")) %>%
              group_by(date_time) %>%
              summarize(do_sonde = mean(do))) %>%
  group_by(md) %>%
  summarize(corr = mean(do_sonde/do))

match_minis_cal_sum19b <- mini_cal_sum19b %>%
  filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-15 18:30") %>%
  select(md, date_time,  do) %>%
  mutate(hour  = round_date(date_time, unit = "hour")) %>%
  left_join(sonde_cal_sum19b %>%
              filter(date_time >= "2019-06-15 12:30" & date_time <= "2019-06-15 18:30") %>%
              select(date_time,  do) %>%
              mutate(hour  = round_date(date_time, unit = "hour")) %>%
              rename(date_time_sonde = date_time,
                     do_sonde = do)) %>%
  mutate(dist = abs(date_time_sonde - date_time)) %>%
  group_by(md, hour) %>%
  filter(dist == min(dist)) %>%
  filter(date_time == min(date_time)) %>%
  ungroup() %>%
  select(md, hour, date_time, do, do_sonde) 

match_minis_cal_sum19b %>%
  gather(var, val, do, do_sonde) %>%
  mutate(md = ifelse(var == "do_sonde","sonde",md)) %>%
  ggplot(aes(date_time,  val, color  = md))+
  geom_point()+
  geom_line()+
  geom_line(aes(group = hour), color = "black")+
  theme_bw()

corr_sum19b <- match_minis_cal_sum19b %>%
  group_by(md) %>%
  summarize(corr = mean(do_sonde/do))







#==========
#========== 2019 summer corrections
#==========

# identify winter 2018 date/md
sum19 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2019,
         date_in %in% c(as.Date("2019-06-15"),as.Date("2019-06-16")) & date_out > "2019-06-16")



# extract deployment data and correct
mini_sum19 <- minidot %>%
  inner_join(sum19) %>%
  filter(date_time > date_in & date_time <= date_out) %>%
  left_join(corr_sum19a %>%
              bind_rows(corr_sum19b)) %>%
  mutate(do_cor = corr*do) %>%
  mutate(cal_group  = "jun19") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)


# Temp
mini_sum19 %>%
  ggplot(aes(date_time, temp, color = layer))+
  facet_wrap(~site, nrow = 2)+
  geom_line()+
  theme_bw()

# plot DO
mini_sum19 %>%
  ggplot(aes(date_time, do, color = layer))+
  facet_wrap(~site, nrow = 2)+
  geom_line()+
  scale_color_manual(values=c("firebrick","dodgerblue"))+
  theme_bw()

#====Winter 2019 Corrections=====

# extract minidots that were deployed over winter 2019-2020
cal_win19 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2019, 
         calibrating == "y", 
         site == "buck",
         date_in %in% c("2019-08-16"))

# extract minidot data from calibration
mini_cal_win19 <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal_win19) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out)

# plot data
mini_cal_win19 %>% 
  select(md, date_time, do, do_sat, temp) %>% 
  gather(var, val, do, do_sat, temp) %>% 
  ggplot(aes(x = date_time, y = val, col = md))+
  facet_wrap(~var, ncol = 1, scales = "free")+
  geom_line()+
  theme_bw()


#minidot 8 needs increase in time by 30 min


#find time with relatively low change in temp
mini_cal_win19 %>% 
  mutate(date_time  = if_else(md == "md8", date_time+30*60, date_time)) %>% 
  filter(date_time>"2019-08-16 19:00:00" & date_time<= "2019-08-17 06:00:00") %>%
  select(md, date_time, do, do_sat, temp) %>% 
  gather(var, val, do, do_sat, temp) %>% 
  ggplot(aes(x = date_time, y = val, col = md))+
  facet_wrap(~var, ncol = 1, scales = "free")+
  geom_line()+
  theme_bw()

corr_aug19 <- mini_cal_win19 %>%
  mutate(date_time  = if_else(md == "md8", date_time+30*60, date_time)) %>% 
  filter(date_time>"2019-08-16 19:00:00" & date_time<= "2019-08-17 06:00:00") %>%
  mutate(do_mean = mean(do)) %>%
  group_by(md) %>%
  summarize(corr = mean(do_mean/do))

# correct miniwin19
miniwin19<- minidot %>%
  mutate(year  = year(date_time),
         date = date(date_time)) %>%
  inner_join(meta %>%
               filter(date_in%in% c(as.Date("2019-08-20"), as.Date("2019-09-01"))))  %>%
  filter(date(date_time) >= date_in & date(date_time) <= date_out) %>%
  arrange(date_time) %>%
  full_join(corr_aug19) %>% 
  mutate(do_cor = corr*do,
         cal_group = "aug19") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)

# examine
miniwin19 %>%
  ggplot(aes(date_time, do))+
  geom_line(size = 0.8)+
  geom_line(aes(y = do_cor), color = "red", size = 0.8, alpha = 0.5)+
  theme_bw()


#=====Summer 2020 corrections=====




# identify summer 2020 date/md
cal_sum20a <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2020, calibrating == "y", site != "buck",
         date_in == "2020-07-26")



# extract sonde data
sonde_cal_sum20a <- sonde %>%
  mutate(year  = year(date_time)) %>%
  filter(as.Date(date_time) >= unique(cal_sum20a$date_in) & as.Date(date_time) <= unique(cal_sum20a$date_out))

# extract minidot data
mini_cal_sum20a <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal_sum20a) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out)

# plot temp
#find times
starttime <- as_datetime("2020-07-26 12:00:00")
endtime <- as_datetime("2020-07-28 12:00:00")

#check
sonde_cal_sum20a %>%
  select(date_time,  temp) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum20a %>%
              select(md, date_time,  temp)) %>%
  ggplot(aes(date_time,  temp, color  = md))+
  geom_line()+
  geom_vline(xintercept = starttime)+
  geom_vline(xintercept = endtime)+
  theme_bw()

#plot temp
sonde_cal_sum20a %>%
  filter(date_time >= starttime& date_time <= endtime) %>%
  select(date_time,  temp) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum20a %>%
              filter(date_time >= starttime & date_time <= endtime) %>%
              select(md, date_time,  temp)) %>%
  ggplot(aes(date_time,  temp, color  = md))+
  geom_line()+
  theme_bw()

# plot do
sonde_cal_sum20a %>%
  filter(date_time >= starttime& date_time <= endtime) %>%
  select(date_time,  do) %>%
  mutate(md  = "sonde") %>%
  bind_rows(mini_cal_sum20a %>%
              filter(date_time >= starttime & date_time <= endtime) %>%
              select(md, date_time,  do)) %>%
  ggplot(aes(date_time,  do, color  = md))+
  geom_line()+
  theme_bw()

mini_cal_sum20a %>%
  filter(date_time >= starttime & date_time <= endtime) %>%
  select(md, date_time,  do) %>%
  mutate(date_time  = round_date(date_time, unit = "hour")) %>%
  group_by(md, date_time) %>%
  summarize(do = mean(do)) %>%
  full_join(sonde_cal_sum20a %>%
              filter(date_time >= starttime & date_time <= endtime) %>%
              select(date_time,  do) %>%
              mutate(date_time  = round_date(date_time, unit = "hour")) %>%
              group_by(date_time) %>%
              summarize(do_sonde = mean(do))) %>%
  group_by(md) %>%
  filter(!is.na(do_sonde)) %>% 
  summarize(corr = mean(do_sonde/do))

match_minis_cal_sum20a <- mini_cal_sum20a %>%
  filter(date_time >= starttime & date_time <= endtime) %>%
  select(md, date_time,  do) %>%
  mutate(hour  = round_date(date_time, unit = "hour")) %>%
  left_join(sonde_cal_sum20a %>%
              filter(date_time >= starttime & date_time <= endtime) %>%
              select(date_time,  do) %>%
              mutate(hour  = round_date(date_time, unit = "hour")) %>%
              rename(date_time_sonde = date_time,
                     do_sonde = do)) %>%
  mutate(dist = abs(date_time_sonde - date_time)) %>%
  group_by(md, hour) %>%
  filter(dist == min(dist)) %>%
  filter(date_time == min(date_time)) %>%
  ungroup() %>%
  select(md, hour, date_time, do, do_sonde) 

match_minis_cal_sum20a %>%
  gather(var, val, do, do_sonde) %>%
  mutate(md = ifelse(var == "do_sonde","sonde",md)) %>%
  ggplot(aes(date_time,  val, color  = md))+
  geom_point()+
  geom_line()+
  geom_line(aes(group = hour), color = "black")+
  theme_bw()

corr_sum20a <- match_minis_cal_sum20a %>%
  group_by(md) %>%
  summarize(corr = mean(do_sonde/do))

# identify summmer 2020 date/md
sum20 <- meta %>%
  mutate(year  = year(date_in),
         dttm_in = as.POSIXct(paste(date_in, time_in), format="%Y-%m-%d %H:%M:%S"),
         dttm_out = as.POSIXct(paste(date_out, time_out), format="%Y-%m-%d %H:%M:%S")) %>% 
  filter(year == 2020,
         date_in %in% c(as.Date("2020-07-28"),as.Date("2020-07-29")))



# extract deployment data and correct
mini_sum20 <- minidot %>%
  inner_join(sum20) %>%
  filter(date_time > dttm_in & date_time <= dttm_out) %>%
  left_join(corr_sum20a) %>%
  mutate(do_cor = corr*do) %>%
  mutate(cal_group  = "jul20") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)


# Temp
mini_sum20 %>%
  ggplot(aes(date_time, temp, color = layer))+
  facet_wrap(~site, nrow = 2)+
  geom_line()+
  theme_bw()

# plot DO
mini_sum20 %>%
  ggplot(aes(date_time, do, color = layer))+
  facet_wrap(~site, nrow = 2)+
  geom_line()+
  scale_color_manual(values=c("firebrick","dodgerblue"))+
  theme_bw()


#====Winter 2020 corrections====

# extract minidots that were deployed over winter 2019-2020
cal_win20 <- meta %>%
  mutate(year  = year(date_in)) %>%
  filter(year == 2020, 
         calibrating == "y", 
         site == "buck",
         date_in == "2020-08-29")

# extract minidot data from calibration
mini_cal_win20 <- minidot %>%
  mutate(year  = year(date_time)) %>%
  inner_join(cal_win20) %>%
  filter(as.Date(date_time) >= date_in & as.Date(date_time) <= date_out)

# plot data
mini_cal_win20 %>% 
  select(md, date_time, do, do_sat, temp) %>% 
  gather(var, val, do, do_sat, temp) %>% 
  ggplot(aes(x = date_time, y = val, col = md))+
  facet_wrap(~var, ncol = 1, scales = "free")+
  geom_line()+
  theme_bw()


#minidot 8 needs increase in time by 30 min


#find time with relatively low change in temp
starttime <- "2020-08-29 23:00:00"
endtime <- "2020-09-01 8:00:00"

mini_cal_win20 %>% 
  filter(date_time>starttime& date_time<= endtime) %>%
  select(md, date_time, do, do_sat, temp) %>% 
  gather(var, val, do, do_sat, temp) %>% 
  ggplot(aes(x = date_time, y = val, col = md))+
  facet_wrap(~var, ncol = 1, scales = "free")+
  geom_line()+
  theme_bw()

corr_aug20 <- mini_cal_win20 %>%
  filter(date_time>starttime& date_time<= endtime) %>%
  mutate(do_mean = mean(do)) %>%
  group_by(md) %>%
  summarize(corr = mean(do_mean/do))

# correct miniwin20
miniwin20<- minidot %>%
  mutate(year  = year(date_time),
         date = date(date_time)) %>%
  inner_join(meta %>%
               filter(date_in == "2019-09-05"))  %>%
  filter(date(date_time) >= date_in & date(date_time) <= date_out) %>%
  arrange(date_time) %>%
  full_join(corr_aug19) %>% 
  mutate(do_cor = corr*do,
         cal_group = "aug20") %>%
  select(site, lat, lon, layer, sensor_depth, date_time, q, temp, do_eq, do, do_cor, do_sat,
         cal_group, flag)

# examine
miniwin20 %>%
  ggplot(aes(date_time, do))+
  geom_line(size = 0.8)+
  geom_line(aes(y = do_cor), color = "red", size = 0.8, alpha = 0.5)+
  theme_bw()



#=====Join all Files together=====
mini_full1 <- minidot17_correct %>% #summer 2017
  bind_rows(mini_win17 %>% #winter 2017-2018
              bind_rows(mini_sum18 %>% #summer2018
                          bind_rows(mini_win18 %>% #winter 2018-2019
                                      bind_rows(mini_sum19 %>% #summer 2019
                                                  bind_rows(miniwin19 %>% #winter 2019-2020
                                                              bind_rows(mini_sum20)))))) #summer 2020

#create date_times for removing times out of water
meta_full <- meta %>% 
  mutate(date_time_in = as_datetime(paste(date_in, time_in)),
         date_time_out = as_datetime(paste(date_out, time_out))) %>% 
  rename(meta_flag = flag) 

mini_full <- mini_full1 %>% 
  inner_join(meta_full) %>% 
  filter(date_time>=date_time_in &
           date_time<=date_time_out)

#plot to confirm
mini_full %>% 
  ggplot(aes(x = date_time, y = do_cor, group = cal_group))+
  facet_grid(site~layer, scales = "free_x")+
  geom_line()

mini_full %>% 
  ggplot(aes(x = date_time, y = do_cor, col = layer, group = cal_group))+
  facet_wrap(~site, scales = "free_x")+
  geom_line()

today <- format(Sys.Date(),  format = "%d%b%y")
# write_csv(mini_full, paste0("minidot/clean/minidot_clean_", today, ".csv"))


#====Explore some of the corrections====

corr <- bind_rows(corr_aug18 %>% mutate(group = "aug18"),  corr_aug19 %>% mutate(group = "aug19") %>% 
            bind_rows(corr_aug20 %>% mutate(group = "aug20") %>% 
                        bind_rows(corr_jun18 %>% mutate(group = "jun18") %>% 
                                    bind_rows(corr_sum19a %>% mutate(group = "sum19") %>% 
                                                bind_rows(corr_sum19b %>% mutate(group = "sum19") %>% 
                                                            bind_rows(corr_sum20a %>% mutate(group = "sum20") %>% 
                                                                        bind_rows(cal_data17 %>% mutate(md = "md1", group = "sum17"))))))))

corr %>% ggplot(aes(x = md, y = corr, col = group))+
  geom_jitter(width = 0.2)+theme_bw()



