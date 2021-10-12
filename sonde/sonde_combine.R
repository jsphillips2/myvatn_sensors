#==========
#========== Preliminaries
#==========

# load packages
library(tidyverse)
library(lubridate)
Sys.setenv(TZ = "GMT")


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
    if(year %in% c("2014","2015","2016","2017","2018","2019", "2020", "2021")) {
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
    
    if (year %in% c("2014","2015","2016","2017","2018","2019", "2020", "2021")) {
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
  arrange(date_time) %>%
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
  )

sonde_trim <- read_csv("sonde/sonde_trim.csv") 

sonde_trim <- sonde_trim %>% 
  mutate(dttm_in = as.POSIXct(paste(date_in, time_in), format="%Y-%m-%d %H:%M:%S"),
         dttm_out = as.POSIXct(paste(date_out, time_out), format="%Y-%m-%d %H:%M:%S"))


sonde_trimmed <- sonde %>% 
  crossing(sonde_trim) %>% 
  filter(date_time>dttm_in,
         date_time<dttm_out)

#plot
sonde_trimmed %>% 
  group_by(as.Date(date_time)) %>% 
  mutate(mean = mean(do),
         sd = sd(do)) %>% 
  filter(!is.na(date_time),
         do<mean+2*sd,
         do>mean-2*sd,
         do!=0) %>% 
  mutate(year = year(date_time)) %>% 
  filter(year!=2014) %>% 
  ggplot(aes(y = do, x = date_time, group = dttm_in))+
  facet_wrap(~year, scales = "free_x")+
  geom_line()


sonde_trimmed %>% 
  group_by(as.Date(date_time)) %>% 
  mutate(mean = mean(pcyv),
         sd = sd(pcyv)) %>% 
  filter(!is.na(date_time),
         pcyv<mean+2*sd,
         pcyv>mean-2*sd,
         pcyv<0.8) %>% 
  mutate(year = year(date_time)) %>% 
  ggplot(aes(y = pcyv, x = date_time, group = dttm_in))+
  facet_wrap(~year, scales = "free_x")+
  geom_line()


sonde_trimmed %>% 
  group_by(as.Date(date_time)) %>% 
  mutate(mean = mean(temp),
         sd = sd(temp)) %>% 
  filter(!is.na(date_time),
         temp<mean+2*sd,
         temp>mean-2*sd,
         temp<30) %>% 
  mutate(year = year(date_time)) %>% 
  ggplot(aes(y = temp, x = date_time, group = dttm_in))+
  facet_wrap(~year, scales = "free_x")+
  geom_line()

sonde_trimmed %>% 
  group_by(as.Date(date_time)) %>% 
  mutate(mean = mean(spcond),
         sd = sd(spcond)) %>% 
  filter(!is.na(date_time),
         spcond<mean+2*sd,
         spcond>mean-2*sd) %>% 
  mutate(year = year(date_time)) %>% 
  ggplot(aes(y = spcond, x = date_time, group = dttm_in))+
  facet_wrap(~year, scales = "free_x")+
  geom_line()

sonde_trimmed %>% 
  group_by(as.Date(date_time)) %>% 
  mutate(mean = mean(turbsc),
         sd = sd(turbsc)) %>% 
  filter(!is.na(date_time),
         turbsc<mean+2*sd,
         turbsc>mean-2*sd) %>% 
  mutate(year = year(date_time)) %>% 
  ggplot(aes(y = turbsc, x = date_time, group = dttm_in))+
  facet_wrap(~year, scales = "free_x")+
  geom_line()


# export
# write_csv(sonde_trimmed, "sonde/sonde_clean.csv")



