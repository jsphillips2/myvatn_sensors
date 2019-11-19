#==========
#========== Preliminaries
#==========

# load packages
library(tidyverse)
library(lubridate)

# set cores for parallel
options(mc.cores = parallel::detectCores()-2)

# import data
minidot <- list.files("minidot/raw") %>%
  
  parallel::mclapply(function(x){read_csv(paste0("minidot/raw/",x), skip = 8) %>% # read files
      set_names(read_csv(paste0("minidot/raw/",x), skip = 7) %>% names()) %>% # set column names
      mutate(name = str_split(x, "_") %>% map_chr(~.x[1])) # parse names
  }) %>%
  bind_rows() %>%
  rename(date_time = "UTC_Date_&_Time",
         temp = "Temperature",
         do = "Dissolved Oxygen",
         do_sat = "Dissolved Oxygen Saturation",
         q = Q) %>%
  select(name, date_time,  temp,  do, do_sat, q)

# import metadata
meta <- read_csv("minidot/deployment_log19Nov19.csv", col_types = c("cccdDtDtddcdc"))

# import sonde data
sonde <- read_csv("sonde/sonde_clean.csv", col_types = c("Tdddddd"))





#==========
#========== 2017
#==========

sonde17 <- sonde %>%
  mutate(year  = year(date_time))