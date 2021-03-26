
# load packages
library(tidyverse)
library(lubridate)
Sys.setenv(TZ = "GMT")

# set cores for parallel
options(mc.cores = parallel::detectCores()-2)

# define flagging function
flag_fn <- function(x){x[!is.na(x)] %>% length()}

# import and process data

list <- list.files("hobo/raw", full.names = T)
hobo_list <- list.files("hobo/raw", full.names = T) %>%
  parallel::mclapply(function(x){
    
    type = substr(x, nchar(x)-2, nchar(x))
    
    if(type == "txt") 
    { data = read_csv(x, skip = 2, col_names = F)
    sn = {read_csv(x, skip = 1, col_names = F)[1, 3] %>%
        str_split("SEN ") %>% unlist()}[2] %>% substr(6, nchar(.)-1)
    } 
    
    if(type == "tsv") 
    { data = read_tsv(x, skip = 2, col_names = F)
    sn = {read_tsv(x, skip = 1, col_names = F)[1, 3] %>%
        str_split("SEN ") %>% unlist()}[2] %>% substr(6, nchar(.)-1)
    }
    
    data = data %>%
      select(2, 3, 4) %>%
      set_names(c("datetime","temp","lux")) %>%
      mutate(datetime = 
               parse_date_time(datetime, orders = 
                                 c("mdy HMS","mdy HM", "mdY HMS", "mdY HM",
                                   "mdy HMS p","mdy HM p", "mdY HMS p", "mdY HM p")),
             name = str_remove(x, "hobo/raw/"),
             name = str_remove(name, ".txt"),
             name = str_remove(name, ".tsv"),
             sn = sn) %>%
      select(name, sn, datetime, temp, lux)
    
    flag = if(min(apply(data, 2, flag_fn))< 2) "yes" else "no"
    
    data = data %>%
      mutate(flag = flag)
    
    return(data)
  })

# check for flags
hobo_list %>% lapply(function(x){
  x %>% select(name, flag) %>% unique()
}) %>%
  bind_rows() %>% 
  filter(flag == "yes")

# fix files with missing temp
if(hobo_list[[32]]$flag[1] == "yes") {
  hobo_list[[32]] <- hobo_list[[32]] %>%
    select(-lux) %>%
    rename(lux = temp) %>%
    mutate(flag = "no")
}

if(hobo_list[[33]]$flag[1] == "yes") {
  hobo_list[[33]] <- hobo_list[[33]] %>%
    select(-lux) %>%
    rename(lux = temp) %>%
    mutate(flag = "no")
}

if(hobo_list[[50]]$flag[1] == "yes") {
  hobo_list[[50]] <- hobo_list[[50]] %>%
    select(-lux) %>%
    rename(lux = temp) %>%
    mutate(flag = "no")
}

#check lux class
hobo_list %>% 
  lapply(function(x){class(x$lux)})


# bind rows
hobos = hobo_list %>%
  bind_rows() %>%
  arrange(datetime, name) 
  

summary(hobos)

#read in meta data
hobo_meta <- read_csv("hobo/hobo_log.csv") 

#read in trims
hobo_trim <- read_csv("hobo/hobo_trim.csv") %>% 
  mutate(dttm_in = as_datetime(paste(date_in, time_in)),
         dttm_out = as_datetime(paste(date_out, time_out)))

#join hobo data with metadata
hobos_full_raw <- hobos %>% 
  select(-flag) %>% 
  full_join(hobo_meta %>% 
              select(-contains("date")), by = "name")

#trim hobo data to appropriate times
hobos_full <- hobos_full_raw %>% 
  full_join(hobo_trim) %>% 
  filter(datetime>=dttm_in,
         datetime<=dttm_out)

#check all metadata accounted for
hobo_meta %>% 
  select(-contains("date")) %>% 
  full_join(hobo_trim) %>% 
  filter(is.na(site)) %>% 
  select(name) %>% 
  unique()

hobos_full%>% 
  filter(is.na(site)) %>% 
  select(name) %>% 
  unique()

#check that all years accounted for and numbers make sense
hobos_full %>% count(year(datetime))


#plot to confirm
hobos_full %>% 
  ggplot(aes(x = datetime, y = temp, col = layer, group = date_in))+
  geom_line()+
  facet_wrap(~site, scales = "free")


hobos_full %>% 
  ggplot(aes(x = datetime, y = lux, col = layer, group = date_in))+
  geom_line()+
  facet_wrap(~site, scales = "free")

hobos_full %>% 
  ggplot(aes(x = lux, y = temp, col = layer))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm")



hobos_full %>% 
  filter(layer == "ben", !is.na(hobo_id), date_in %in% c(as.Date("2019-09-01"), as.Date("2019-08-20"))) %>% 
  ggplot(aes(x = datetime, y = temp, col = sensor_depth, group = interaction(hobo_id,date_in)))+
  facet_wrap(~site)+
  geom_line()+
  scale_y_log10()+
  scale_color_viridis_c(trans = "log1p", direction = -1)


#probably needs more attention than this, but ... 
# hobos_full %>% 
#   filter(layer == "ben", !is.na(hobo_id), date_in %in% c(as.Date("2019-09-01"), as.Date("2019-08-20"))) %>% 
#   group_by(site, date = as.Date(datetime), hour = hour(datetime), sensor_depth) %>% 
#   summarise(temp = mean(temp)) %>% 
#   group_by(site, date, hour) %>% 
#   summarise(thermo.depth = rLakeAnalyzer::thermo.depth(depths = sensor_depth, temp)) %>% 
#   ggplot(aes(x = date, y = thermo.depth))+
#   facet_wrap(~site)+
#   geom_line()+
#   scale_y_reverse()

today <- format(Sys.Date(),  format = "%d%b%y")
# write_csv(hobos_full, paste0("hobo/clean/hobo_clean_", today, ".csv"))
