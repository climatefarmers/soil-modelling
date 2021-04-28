if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve)


working_dir <- get_wd()

source(file.path(working_dir, "model_functions.R"))

# Inputs

# Temperature (Wetterstation Lindenberg)
# 40 Year MeanTemperature/Month
temp <- c(2.6, 4.2, 8.5, 14.5, 19.5, 22.3, 24.5, 24.3, 
          19.4, 13.7, 7.2, 3.5)

# 1deg C Temperature rise on 40 year average values
temp_cc <- temp + 1

# Precipitation - monthly mean values from 1981 to 2020
precip <- c(
  42, 35, 41, 34, 52, 62, 70, 60, 43, 39, 42, 47
)

# Evaporation - Mean values from 1991 - 2020
evap <- c( 10, 15, 33, 53, 52, 61, 69, 51, 32, 19, 9, 7)

soil_thick <- 25 # Soil thickness (organic layer topsoil), in cm
SOC <- 39 # Soil organic carbon in Mg/ha
clay <- 18 # Percent clay
c_inputs <- 7.56 # Annual C inputs to soil in Mg/ha/yr
pE <- 1.0 # Evaporation Coefficient
FallIOM <- 0.049 * SOC^(1.139) # IOM using Falloon method

time_horizon <- 10

# Sensitivity check
C0_df <- calc_soil_carbon(
  time_horizon = 500,
  bare = FALSE, 
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs,
  pE = pE,
  PS = c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=FallIOM),
  description = "Base Case"
)
C0 <- get_total_C(C0_df)

# TODO: Turn this into an average value
starting_soil_content <- as.numeric(tail(C0_df, 1))


# Sensitivity File
# TODO: Set this up to call in the variables from a csv and run in loop



desc_1 <- "Bare, No CC"
C1_df <- calc_soil_carbon(
  time_horizon = time_horizon,
  bare = TRUE, 
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs,
  pE = pE,
  PS = starting_soil_content,
  description = desc_1
)
C1 <- get_total_C(C1_df)

desc_2 <- "Covered, No CC"
C2_df <- calc_soil_carbon(
  time_horizon = time_horizon,
  bare = FALSE, 
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs,
  pE = pE,
  PS = starting_soil_content,
  description = desc_2
)
C2 <- get_total_C(C2_df)

desc_3 <- "Bare, CC"
C3_df <- calc_soil_carbon(
  time_horizon = time_horizon,
  bare = TRUE, 
  temp = temp_cc,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs,
  pE = pE,
  PS = starting_soil_content,
  description = desc_3
)
C3 <- get_total_C(C3_df)

desc_4 <- "Covered, CC"
C4_df <- calc_soil_carbon(
  time_horizon = time_horizon,
  bare = FALSE, 
  temp = temp_cc,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs,
  pE = pE,
  PS = starting_soil_content,
  description = desc_4
)
C4 <- get_total_C(C4_df)


# Comparison
Output = data.frame("Description" = c(desc_1, desc_2, desc_3, desc_4),
                    "Carbon Difference" = c(C1 - C0, C2 - C0, C3 - C0, C4 - C0))
Output



icbm_c <- ICBMModel(t = seq(1/12, time_horizon, by = 1/12),
                    ks = c(k1 = 0.8, k2 = 0.00605),
                    h = 0.13,
                    r = 1.32,
                    c0 = c(Y0 = 0.3, O0 = 3.96), 
                    In = c_inputs, 
                    solver = deSolve.lsoda.wrapper,  pass = FALSE)



