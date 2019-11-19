#==========
#========== Preliminaries
#==========

# load packages
library(tidyverse)
library(lubridate)

# set cores for parallel
options(mc.cores = parallel::detectCores()-2)

# import all files
sonde <- list.files("sonde/raw") %>%
  parallel::mclapply(function(x){
    year = substr(x, nchar(x) - 7, nchar(x) - 4) # extract year
    x = read_lines(paste0("sonde/raw/",x)) # read  files
    
    if(year %in% c("2012")) { # lines to skip
      lines = c(-2:-3)
    }
    if(year %in% c("2013")) {
      lines = c(-2)
    }
    if(year %in% c("2014","2015","2016","2017","2018","2019")) {
      lines = c(-1:-12, -14:-15)
    }
    
    x_sub = x[lines] # skip lines
    
    ncols = length(str_split(x_sub[1],",")[[1]]) # total number of columns (including blanks)
    
    inds = str_split(x_sub[1],",")[[1]] %>% `!=`("\"\"") # logical vector for non-empty column positions
    
    x_clean = x_sub %>% 
      str_split(",") %>% # omit lines with fewer than the appropriate number of columns  
      discard(~length(.x) < ncols) %>% # select non-empty columns
      map(~.x[inds]) %>%
      map_chr(~str_c(.x, collapse = ","))
    
    x_csv = str_c(x_clean, collapse = "\n") %>% read_csv() %>% # create csv
      mutate(Date_Time = ymd_hms(paste(mdy(Date), Time))) # create date time
    
    if (year %in% c("2014","2015","2016","2017","2018","2019")) {
      x_csv = x_csv %>% rename(turbsc = TurbSC_1) # rename TurbSC_1
    }
    
    if (year == "2014") {
      x_csv = x_csv %>% 
        mutate(`LDO%` = NA,
               LDO = NA) # fill missing do's with  NA's
    }
    
    return(x_csv) # return
}) %>%
  bind_rows() %>% # combine files
  select(Date_Time, Temp, SpCond, turbsc, `LDO%`, LDO, PCYV) %>% # select columns 
  rename(date_time = Date_Time, 
         temp = Temp, 
         spcond = SpCond,
         do_sat = `LDO%`, 
         do = LDO, 
         pcyv = PCYV) %>%
  arrange(date_time)

# export
# write_csv(sonde, "sonde/sonde_clean.csv")



