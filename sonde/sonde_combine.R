# load packages
library(tidyverse)
library(lubridate)

# import all files
sonde <- list.files("sonde/raw") %>%
  lapply(function(x){
    year = substr(x, nchar(x) - 7, nchar(x) - 4)
    x = read_lines(paste0("sonde/raw/",x))
    if(year %in% c("2012")) {
      lines = c(-2:-3)
    }
    if(year %in% c("2013")) {
      lines = c(-2)
    }
    if(year %in% c("2014","2015","2016","2017","2018","2019")) {
      lines = c(-1:-12, -14:-15)
    }
    
    x_sub = x[lines]
    
    # total number of columns (including blanks)
    ncols = length(str_split(x_sub[1],",")[[1]])
    
    # logical vector for non-empty column positions
    inds = str_split(x_sub[1],",")[[1]] %>%
      `!=`("\"\"")
    
    # clean 
    x_clean = x_sub %>% 
      str_split(",") %>%
      # omit lines with fewer than the appropriate number of columns  
      discard(~length(.x) < ncols) %>%
      # select non-empty columns
      map(~.x[inds]) %>%
      map_chr(~str_c(.x, collapse = ","))
    
    # return as csv
    x_csv = str_c(x_clean, collapse = "\n") %>% 
      read_csv() %>%
      mutate(Date_Time = ymd_hms(paste(mdy(Date), Time)))
    
    if (year %in% c("2014","2015","2016","2017","2018","2019")) {
      x_csv = x_csv %>% rename(TurbSC = TurbSC_1)
    }
    
    return(x_csv)
}) %>%
  bind_rows() %>%
  select(Date_Time, Temp, SpCond, TurbSC, `LDO%`, LDO, PCYV)
 



