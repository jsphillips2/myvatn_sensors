#====load packages===
library(tidyverse)
library(lubridate)
library(lme4)

#===read files====
raw <- read_csv("hobo/clean/hobo_clean_12Oct21.csv")


light <- raw %>%
  filter(year(datetime)==2021,
         month(datetime)>5)


hobos <- light %>% 
  filter(sensor_depth>0) %>% 
  mutate(datetime = round_date(datetime, "30 mins")) %>% 
  select(site, datetime, sensor_depth, lux, temp)

surf <- light %>% 
  filter(sensor_depth<0) %>% 
  select(datetime, sensor_depth, lux, temp) %>% 
  mutate(sensor_depth = 0,
         datetime = round_date(datetime, "30 mins")) %>% 
  crossing(site = unique(hobos$site))


l <- full_join(hobos, surf) %>% 
  arrange(site, datetime, sensor_depth)


#fit light attenuation to hourly data. 
l %>% 
  ggplot(aes(x = datetime, y = lux, color = factor(sensor_depth)))+
  facet_wrap(~site)+
  geom_line()+
  scale_color_viridis_d()


atten <- l %>% 
  nest(data = c(sensor_depth, lux, temp)) %>% 
  mutate(fit = map(data, ~lm(log1p(lux)~sensor_depth, .)))


atten30 <- atten %>% 
  mutate(slope = map(fit, ~coef(.)[2])) %>% 
  unnest(slope) %>% 
  filter(!is.na(slope))


atten30 %>% 
  ggplot(aes(x = datetime, y = abs(slope)))+
  geom_line()+
  facet_wrap(~site)


#average daily

attenday <- l %>% 
  group_by(date = as.Date(datetime), site, sensor_depth) %>% 
  filter(date>"2021-06-19") %>% 
  summarise(lux = mean(lux)) %>% 
  nest(data = c(sensor_depth, lux)) %>% 
  mutate(fit = map(data, ~lm(log1p(lux)~sensor_depth, .)),
         slope =  map(fit, ~coef(.)[2])) %>% 
  unnest(slope) %>% 
  filter(!is.na(slope))


attenday %>% 
  ggplot(aes(x = date, y = abs(slope)))+
  facet_wrap(~site)+
  geom_line()+
  theme_bw()+
  labs(x = "Date",
       y = "Light Attenuation")

attenday %>% 
  ggplot(aes(x = date, color = site, y = abs(slope)))+
  geom_line()+
  theme_bw()+
  labs(x = "Date",
       y = "Light Attenuation")

# write_csv(attenday, "2021_hobostring_attenuation.csv")

#just the workday. 
attenday2 <- l %>% 
  filter(hour(datetime) %in% 9:5) %>% 
  group_by(date = as.Date(datetime), site, sensor_depth) %>% 
  summarise(lux = mean(lux)) %>% 
  nest(data = c(sensor_depth, lux)) %>% 
  mutate(fit = map(data, ~lm(log1p(lux)~sensor_depth, .)),
         slope =  map(fit, ~coef(.)[2])) %>% 
  unnest(slope) %>% 
  filter(!is.na(slope))


attenday2 %>% 
  ggplot(aes(x = date, color = site, y = abs(slope)))+
  geom_line()+
  theme_bw()+
  labs(x = "Date",
       y = "Light Attenuation")


datecheck = "2021-07-20"

l %>% 
  group_by(date = as.Date(datetime), site, sensor_depth) %>% 
  summarise(lux = mean(lux)) %>% 
  filter(date == datecheck) %>% 
  ggplot(aes(y = log1p(lux), x = sensor_depth))+
  facet_wrap(~site)+
  geom_point()+
  geom_smooth(method = "lm")


l %>% 
  group_by(date = as.Date(datetime), site, sensor_depth) %>% 
  filter(date == datecheck) %>% 
  ggplot(aes(y = log1p(lux), x = sensor_depth))+
  facet_wrap(~site)+
  geom_point(aes(col = datetime))+
  geom_line(aes(col = datetime, group = datetime))+
  geom_smooth(col = "black", method = "lm")


#using hourly values as variation 
attenday3 <- l %>%
  mutate(date = as.Date(datetime),
         datetime = as.character(datetime)) %>% 
  filter(date>"2021-06-19") %>% 
  nest(data = c(datetime, sensor_depth, lux, temp)) %>% 
  mutate(fit = map(data, ~lmer(log1p(lux)~sensor_depth + (1|datetime), .)),
         slope =  map(fit, ~fixef(.)[2])) %>% 
  unnest(slope) %>% 
  filter(!is.na(slope))


attenday3 %>% 
  ggplot(aes(x = date, color = site, y = abs(slope)))+
  geom_line()+
  theme_bw()+
  labs(x = "Date",
       y = "Light Attenuation")



