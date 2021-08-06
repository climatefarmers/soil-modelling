#!/usr/bin/env Rscript

library(SoilR);
library(ggplot2);
library(dplyr);
library(tidyr);
library(deSolve);
library(readr);
library(rjson);
library(pracma);


working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))


args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]

params = fromJSON(file = file.path(dir, "params.json"))

###############################################################################
# Input Parameters
###############################################################################

SOC_target <- params$SOC_target

evap <- c(params$evap)
precip <- c(params$precip)
temp0 <- c(params$temp)
soil_thick <- params$soil_thick
clay <- params$clay
pE <- params$pE    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporat

time_horizon <- params$time_horizon
temp_adjustment <- params$temp_adjustment

desc <- params$desc


###############################################################################
# Derived parameters
###############################################################################

temp <- temp0 + temp_adjustment 

###############################################################################
# Modelling run
###############################################################################


c_pool_ini <- solve_for_c_0(
  SOC_target = SOC_target,
  time_horizon = time_horizon,
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  pE = pE,
)

colnames(c_pool_ini) <- c("DPM","RPM","BIO","HUM","IOM")

write.csv(c_pool_ini, file.path(dir, "out.csv"), row.names=FALSE)