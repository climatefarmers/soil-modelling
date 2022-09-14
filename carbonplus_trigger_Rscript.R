#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=FALSE)

####################################################################
# Main script to trigger results generation and upload
# Commandline arguments should be:
# - init_file
# - farmId
####################################################################

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, mongolite, ggplot2, dplyr, tidyr, tidyverse, soilassessment, deSolve, readr, pracma, jsonlite, log4r)

farmId = as.character(args[1])
my_logfile = paste(farmId,'__',str_replace_all(Sys.time(), c(" "="__", ":"="_")),'.log',sep="")
my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", 
                           appenders= list(my_console_appender,my_file_appender))
log4r::info(my_logger, paste("farmId = ",farmId,sep=""))
init_file <- fromJSON("~/Stages/ClimateFarmers/soil-modelling/tech/parameter_files/init_mongoAtlas_suhas.json")
soilModelling_RepositoryPath <- init_file$soil_loc
CO2emissions_RepositoryPath <- init_file$co2_emissions_loc
source(file.path(soilModelling_RepositoryPath,"scripts","run_soil_model.R"))
#source(file.path(CO2emissions_RepositoryPath, "scripts", "main.R"))
step_in_table_final <- run_soil_model(init_file, farmId = farmId)
step_in_table_final$yearly_co2emissions <- rep(0,10) #get_co2emissions(init_file, farmId = farmId)
step_in_table_final <- step_in_table_final %>% 
  mutate(yearly_certificates_mean = yearly_certificates_mean - yearly_co2emissions)
readLines(my_logfile)
# pushing results to mongoDB
connection_string <- init_file$connection_string
carbonresults_collection = mongo(collection="carbonresults", db="carbonplusdb", url=connection_string)
currentYear = format(Sys.Date(), "%Y")
carbonresults_collection$update(paste('{"farmId":"',farmId,'","resultsGenerationYear":',currentYear,'}',sep=""),
                                paste('{"$set":{"yearlyCarbonResults":[',step_in_table_final$yearly_certificates_mean[1],
                                      ',',step_in_table_final$yearly_certificates_mean[2],
                                      ',',step_in_table_final$yearly_certificates_mean[3],
                                      ',',step_in_table_final$yearly_certificates_mean[4],
                                      ',',step_in_table_final$yearly_certificates_mean[5],
                                      ',',step_in_table_final$yearly_certificates_mean[6],
                                      ',',step_in_table_final$yearly_certificates_mean[7],
                                      ',',step_in_table_final$yearly_certificates_mean[8],
                                      ',',step_in_table_final$yearly_certificates_mean[9],
                                      ',',step_in_table_final$yearly_certificates_mean[10],
                                      '],"startYear":',
                                      as.numeric(farms_everything$farmInfo$startYear),'}}',sep=""),
                                upsert=TRUE)#year',i,'": ',round(step_in_table_final$yearly_certificates_mean[i]),'}}',sep=""))
