library(tidyr)
library(dplyr)

source(file.path(working_dir, "uncertainty_functions.R"))

location <- "schwerin"

weather_data <- read.delim(paste0("data/weather_raw/climate_", location,"_historical.txt"), sep=';')
evap_data <- read.delim(paste0("data/weather_raw/",location,"_evap_historical.txt"), sep=';')


meantemp <- weather_data %>%  
  filter(MESS_DATUM_BEGINN >= 19900101) %>% 
  select(MESS_DATUM_BEGINN, MESS_DATUM_ENDE, MO_TX) %>% 
  mutate(MO_TX = ifelse(MO_TX == -999, NA, MO_TX),
         Month = substr(MESS_DATUM_BEGINN, 5,6),
         Year = substr(MESS_DATUM_BEGINN, 1,4)) %>%
  select(-MESS_DATUM_BEGINN, -MESS_DATUM_ENDE) %>% 
  spread(Month, MO_TX) %>% 
  select(-Year) 


temp <- extract_weather_ranges(meantemp)
colnames(temp) <- c("min_temp", "max_temp", "mean_temp")



precip <- weather_data %>%  
  filter(MESS_DATUM_BEGINN >= 19900101) %>% 
  select(MESS_DATUM_BEGINN, MESS_DATUM_ENDE, MO_RR) %>% 
  mutate(MO_RR = ifelse(MO_RR == -999, NA, MO_RR),
         Month = substr(MESS_DATUM_BEGINN, 5,6),
         Year = substr(MESS_DATUM_BEGINN, 1,4)) %>%
  select(-MESS_DATUM_BEGINN, -MESS_DATUM_ENDE) %>% 
  spread(Month, MO_RR) %>% 
  select(-Year)

precip <- extract_weather_ranges(precip)
colnames(precip) <- c("min_precip", "max_precip", "mean_precip")


evap <- evap_data %>% 
  select("Monat", "Summe.von.VGSL") %>% 
  filter(Monat >= 199101) %>% 
  mutate(Month = substr(Monat, 5,6),
         Year = substr(Monat, 1,4)) %>% 
  select(-Monat) %>% 
  spread(Month,Summe.von.VGSL) %>% 
  select(-Year)

evap <- extract_weather_ranges(evap)
colnames(evap) <- c("min_evap", "max_evap", "mean_evap")


if(!dir.exists("data/weather_average")){dir.create("data/weather_average")}
weather_uncertainty <- cbind(evap,precip,temp)

write.csv(weather_uncertainty, paste0("data/weather_average/", tolower(location),"_uncertainty.csv"), row.names = F)

