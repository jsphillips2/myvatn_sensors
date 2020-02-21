#find out of water
#run mini_calibrate first.
#===2017====
minidot17_correct %>% 
  filter(temp>15) %>% 
  count(date(date_time))

minidot17_correct %>%
  filter(date(date_time)<min(date(date_time))+10) %>%
  ggplot(aes(y = do, x = date_time))+
  # geom_vline(xintercept = as_datetime("2017-09-25 09:00:00"))+
  geom_line()

minidot17_correct %>%
  filter(date(date_time)>max(date(date_time))-10) %>%
  ggplot(aes(y = temp, x = date_time))+
  geom_vline(xintercept = as_datetime("2017-09-25 09:00:00"))+
  geom_line()

time1 <- as_datetime("2017-07-13 10:50:00")
time2 <- as_datetime("2017-07-13 11:55:00")

minidot17_correct %>%
  filter(date(date_time)==as.Date("2017-07-13")) %>%
  ggplot(aes(y = temp, x = date_time))+
  geom_vline(xintercept = time1)+
  geom_vline(xintercept = time2)+
  geom_line()


minidot17_correct %>%
  filter(date_time> time1 & date_time<time2) %>% 
  as.data.frame()


#===Winter 2017====


mini_win17.2 <- mini_win17 %>%
  mutate(date = as.Date(date_time)) %>% 
  inner_join(meta) %>% 
  filter(date>=date_in,
         date<=date_out) 

time1 <- as_datetime("2017-10-03 14:00:00")


mini_win17.2 %>% 
  filter(date>date_out-2) %>% 
  ggplot(aes(x = date_time, y = temp))+
  facet_wrap(~site, ncol = 1, scales = "free")+
  geom_vline(xintercept = time1)+
  geom_line()
  
  
#===Summer 2018====
mini_sum18.2 <- mini_sum18 %>% 
  mutate(date = as.Date(date_time)) %>% 
  inner_join(meta) %>% 
  filter(date>=date_in,
         date<=date_out) 



time1 <- as_datetime("2018-07-21 11:00:00")
time2 <- as_datetime("2018-07-21 13:00:00")


mini_sum18.2 %>% 
  filter(site=="st33",
         layer =="ben",
         date==as.Date("2018-05-31")) %>% 
  ggplot(aes(x = date_time, y = temp, col = layer))+
  facet_wrap(~layer, ncol = 1, scales = "free")+
  geom_vline(xintercept = time1)+
  geom_vline(xintercept = time2)+
  geom_line()


mini_sum18.2 %>% 
  filter(site=="st33",
         date_time>time1&date_time<time2,
         layer == "ben") %>% 
  select(site, layer, date_time, temp) %>% 
  as.data.frame()

#====Winter 2018====
mini_win18.2 <- mini_win18%>% 
  mutate(date = as.Date(date_time)) %>% 
  inner_join(meta) %>% 
  filter(date>=date_in,
         date<=date_out) 

time1 <- as_datetime("2019-06-09 12:00:00")
time2 <- as_datetime("2019-06-10 13:00:00")

unique(mini_win18.2$site)

mini_win18.2 %>% 
  filter(site=="st33",
         layer =="ben",
         date>=date_out-1) %>% 
  ggplot(aes(x = date_time, y = temp, col = layer))+
  facet_wrap(~layer, ncol = 1, scales = "free")+
  geom_vline(xintercept = time1)+
  geom_vline(xintercept = time2)+
  geom_line()


mini_win18.2 %>% 
  filter(site=="st33",
         date_time>time1&date_time<time2,
         layer == "ben") %>% 
  select(site, layer, date_time, temp) %>% 
  as.data.frame()

#===Summer 2019====

mini_sum19.2 <- mini_sum19%>% 
  mutate(date = as.Date(date_time)) %>% 
  inner_join(meta) %>% 
  filter(date>=date_in,
         date<=date_out) 


time1 <- as_datetime("2019-08-15 12:00:00")
time2 <- as_datetime("2019-06-10 13:00:00")

unique(mini_sum19.2$site)

mini_sum19.2 %>% 
  filter(site=="e5",
         layer =="ben",
         date>=date_out-1) %>% 
  ggplot(aes(x = date_time, y = temp, col = layer))+
  facet_wrap(~layer, ncol = 1, scales = "free")+
  geom_vline(xintercept = time1)+
  geom_vline(xintercept = time2)+
  geom_line()


mini_sum19.2 %>% 
  filter(site=="st33",
         date_time>time1&date_time<time2,
         layer == "ben") %>% 
  select(site, layer, date_time, temp) %>% 
  as.data.frame()
