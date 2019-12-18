
# load packages
library(tidyverse)
library(lubridate)
Sys.setenv(TZ = "GMT")

# set cores for parallel
options(mc.cores = parallel::detectCores()-2)

# define flagging function
flag_fn <- function(x){x[!is.na(x)] %>% length()}

# import and process data
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
                                 c("mdy HMS","mdy HM", "mdY HMS", "mdY HM")),
             name = str_remove(x, "hobo/raw/"),
             sn = sn) %>%
      select(name, sn, datetime, temp, lux)
    
    flag = if(min(apply(data, 2, flag_fn))== 0) "yes" else "no"
    
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

# fix file with missing temp
if(hobo_list[[32]]$flag[1] == "yes") {
  hobo_list[[32]] <- hobo_list[[32]] %>%
    select(-lux) %>%
    rename(lux = temp) %>%
    mutate(flag = "no")
}

# bind rows
hobos = hobo_list %>%
  bind_rows() %>%
  arrange(datetime, name)

plot_fn <- function(x){
  x %>% 
    gather(var, val, temp, lux) %>%
    ggplot(aes(datetime, val))+
    facet_wrap(~var, scales = "free_y", nrow = 2)+
    geom_line()+
    theme_bw()+
    labs(title = x$name)
}

# check plots and datetime ranges
n = 1
plot_fn(hobo_list[[n]])
# hobo_list[[n]] %>%
#   filter(datetime > "2019-07-01", datetime < "2019-07-10") %>%
#   plot_fn()
hobo_list[[n]]$datetime %>% min()
hobo_list[[n]]$datetime %>% max()

# read log file 
hobo_log <- read_csv("hobo/hobo_log.csv") %>%
  mutate(row = row_number())

# combine log file with file names
hobo_log_files <- tibble(name = list.files("hobo/raw") %>% 
                          {substr(., 1, nchar(.) - 4)},
                    name2 = name) %>%
  separate(name2, c("site","layer","date_in","date_out","hobo_id"), sep = "_") %>%
  mutate(date_in = parse_date(date_in, format = c("%d%b%Y")),
         date_out = parse_date(date_out, format = c("%d%b%Y"))) %>%
  full_join(hobo_log)

# export
write_csv(hobo_log_files, "hobo/hobo_log_files.csv")
