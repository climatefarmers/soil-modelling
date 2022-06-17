# weather station Schwerin - calculating monthly mean temperature  
# over a period of 30 years 
library(tidyr)
library(dplyr)


location <- "schwerin"

weather_data <- read.delim(paste0("data/weather_raw/climate_", location,"_historical.txt"), sep=';')
evap_data <- read.delim(paste0("data/weather_raw/",location,"_evap_historical.txt"), sep=';')

# temp starts 01.08.1887 - 31.12.2020
# Model will be run with a 30 year average from 1990 to 2020 (Equilibrium Run)
# extract relevant columns (temperature and date)
# Add month as an additional column
# calculates the 30 Year mean
meantemp <- weather_data %>%  
  filter(MESS_DATUM_BEGINN >= 19900101) %>% 
  select(MESS_DATUM_BEGINN, MESS_DATUM_ENDE, MO_TX) %>% 
  mutate(MO_TX = ifelse(MO_TX == -999, NA, MO_TX),
         Month = substr(MESS_DATUM_BEGINN, 5,6),
         Year = substr(MESS_DATUM_BEGINN, 1,4)) %>%
  select(-MESS_DATUM_BEGINN, -MESS_DATUM_ENDE) %>% 
  spread(Month, MO_TX) %>% 
  select(-Year) %>% 
  summarise(temp = colMeans(., na.rm = TRUE),
            month = 1:12) %>% 
  spread(value = "temp", key = month)


######################################################################
# Precipitation
# Model will be run with a 30 year average from 1990 to 2020 (Equilibrium Run)

precip <- weather_data %>%  
  filter(MESS_DATUM_BEGINN >= 19900101) %>% 
  select(MESS_DATUM_BEGINN, MESS_DATUM_ENDE, MO_RR) %>% 
  mutate(MO_RR = ifelse(MO_RR == -999, NA, MO_RR),
         Month = substr(MESS_DATUM_BEGINN, 5,6),
         Year = substr(MESS_DATUM_BEGINN, 1,4)) %>%
  select(-MESS_DATUM_BEGINN, -MESS_DATUM_ENDE) %>% 
  spread(Month, MO_RR) %>% 
  select(-Year) %>% 
  summarise(precip = colMeans(., na.rm = TRUE),
            month = 1:12) %>% 
  spread(value = "precip", key = month)


#############################################################################
# Evaporation 
# extract relevant columns (evaporation and date)
evap <- evap_data %>% 
  select("Monat", "Summe.von.VGSL") %>% 
  filter(Monat >= 199101) %>% 
  mutate(Month = substr(Monat, 5,6),
         Year = substr(Monat, 1,4)) %>% 
  select(-Monat) %>% 
  spread(Month,Summe.von.VGSL) %>% 
  select(-Year) %>% 
  summarise(evap = colMeans(., na.rm = TRUE),
            month = 1:12) %>% 
  spread(value = "evap", key = month)


#############################################################################
# Save weather data

if(!dir.exists("data/weather_average")){dir.create("data/weather_average")}
weather_average <- rbind("evap" = evap,"precip" = precip,"temp" = meantemp)

write.csv(weather_average, paste0("data/weather_average/", tolower(location),"_average.csv"))
