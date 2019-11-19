
# load packages
library(tidyverse)

# import data
data <- list.files("minidot/raw") %>%
  lapply(function(x){read_csv(paste0("minidot/raw/",x), skip = 8) %>%
      set_names(read_csv(paste0("minidot/raw/",x), skip = 7) %>% names()) %>%
      mutate(name = str_split(x, "_") %>% map_chr(~.x[1]))
  }) %>%
  bind_rows() %>%
  rename(date_time = "UTC_Date_&_Time",
         temp = "Temperature",
         do = "Dissolved Oxygen",
         do_sat = "Dissolved Oxygen Saturation",
         q = Q)
