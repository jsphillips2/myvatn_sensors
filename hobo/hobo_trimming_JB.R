#====Cleaning MiniDot Hobos======
dhobo <- left_join(hobos %>% 
                     mutate(name = substr(name, 1, nchar(name)-4)), hobo_log_files, by = "name")

d <- dhobo %>% 
  filter(layer == "ben", 
         year(date_in)==2018,
         year(date_out)==2019)

x = 1
#trim starts
d %>% 
  filter(site == unique(d$site)[x],
         datetime<date_in+5) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  # geom_vline(xintercept = as_datetime("2019-06-16 18:00:00"))+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")


#trim bottoms
d %>% 
  filter(site == unique(d$site)[x],
         datetime>date_out-5) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  geom_vline(xintercept = as_datetime("2019-08-15 8:00:00"))+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")

#examine full time series
d %>% 
  filter(site == unique(d$site)[x]) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")

#====Cleaning Overlapping hobos=====
d <- dhobo %>% 
  filter(layer == "out", 
         year(date_in)==2018)

x = 1


#examine full time series
d %>% 
  filter(site == unique(d$site)[x]) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")

#zoom in on start
d %>% 
  filter(site == unique(d$site)[x],
         datetime< min(date_in)+5) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  geom_vline(xintercept = as_datetime("2017-05-24 21:00:00"))+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")

#zoom in on end
d %>% 
  filter(site == unique(d$site)[x],
         date_in==max(date_in),
         datetime > max(date_out)-5) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  geom_vline(xintercept = as_datetime("2017-10-04 24:00:00"))+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")


#zoom in on overlap
d %>% 
  filter(site == unique(d$site)[x],
         ifelse(name == unique(d$name)[1], datetime>=(min(date_out)-1), datetime<=(max(date_in)+1))) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  geom_vline(xintercept = as_datetime("2018-07-21 11:15:00"))+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")

#2017 because different
d %>% 
  filter(datetime> as_datetime("2017-08-20 00:00:00"),
         datetime< as_datetime("2017-08-21 00:00:00")) %>% 
  gather(var, val, temp, lux) %>% 
  ggplot(aes(x = datetime, y = val, col = name))+
  facet_wrap(~var, nrow  = 2, scales = "free_y")+
  geom_line()+
  geom_vline(xintercept = as_datetime("2017-08-20 11:30:00"))+
  labs(title = unique(d$site)[x])+
  theme(legend.position = "bottom")
