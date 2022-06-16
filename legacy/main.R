#!/usr/bin/env Rscript

library(SoilR);
library(ggplot2);
library(dplyr);
library(tidyr);
library(deSolve);
library(readr);
library(rjson);

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))


args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]

###############################################################################
# Read input parameters from JSON
###############################################################################

params = fromJSON(file = file.path(dir, "params.json"))

###############################################################################
# Modelling run
###############################################################################

c_df <- calc_carbon_over_time(
  time_horizon = params$time_horizon,
  field_carbon_in = c(params$c_in),
  dr_ratios = c(params$dr_ratio),
  bare_profile = c(params$bare_profile), 
  temp = c(params$temp),
  precip = c(params$precip),
  evap = c(params$evap),
  soil_thick = params$soil_thick,
  clay = params$clay,
  pE = params$pE,    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporat,
  PS = c(DPM=params$SOC_pools$DPM, 
         RPM=params$SOC_pools$RPM, 
         BIO=params$SOC_pools$BIO, 
         HUM=params$SOC_pools$HUM, 
         IOM=params$SOC_pools$IOM),
  tilling_factor = params$tilling_factor
)

c_df$TOTAL = rowSums(c_df[,c(1,2,3,4,5)])

print(c_df)
write.csv(c_df, file.path(dir, "out.csv"), row.names=FALSE)