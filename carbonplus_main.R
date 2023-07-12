carbonplus_main <- function(init_file, farmId=NA, JSONfile=NA){
  
  ####################################################################
  # This script has the following functions:
  # - prepare log files
  # - set general model settings
  # - get farm data from mongoDB
  # - get farm environmental zone
  # - call the soil model and emissions calculations
  # - upload resulting co2eq to mongoDB
  ####################################################################
  
  
  ## Loading libraries ---------------------------------------------------------
  
  library(pacman)
  p_load('pacman', 'SoilR', 'mongolite', 'dplyr', 'tidyr', 'tidyverse',
         'soilassessment', 'readr','aws.s3', 'log4r', 'jsonlite',
         'httr', 'logger', 'ncdf4', 'ncdf4.helpers')

  
  ## Prepare log files ---------------------------------------------------------
  
  #landUseSummaryOrPractices_schema_281022 <- readRDS('landUseSummaryOrPractices_schema_281022')
  if(!dir.exists('logs')) {dir.create('logs')}
  my_logfile = file.path('logs', paste(farmId,'__',str_replace_all(Sys.time(), c(" "="__", ":"="_")),'.log',sep=""))
  my_console_appender = console_appender(layout = default_log_layout())
  my_file_appender = file_appender(my_logfile, append = TRUE, 
                                   layout = default_log_layout())
  my_logger <- log4r::logger(threshold = "INFO", 
                             appenders= list(my_console_appender,my_file_appender))
  log4r::info(my_logger, paste("farmId = ",farmId,sep=""))
  
  
  ## Soil model settings -------------------------------------------------------
  
  pars = list(
    n_run = 3,
    sd_future_mod=1,
    sd_field_carbon_in=0.10,
    CFmade_grazing_estimations_Yes_No="No"
    )
  
  # To copy the practice of a single year to all others
  yearX_landuse <- 1  # setting to 0 will copy baseline
  yearX_livestock <- 1  # setting to 0 will copy baseline
  copy_yearX_to_following_years_landUse <- FALSE
  copy_yearX_to_following_years_livestock <- FALSE
  
  
  ## General setting -----------------------------------------------------------
  
  # Set environmental variables for AWS 
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_file$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_file$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_file$AWS_DEFAULT_REGION
  )
  init_file=fromJSON("../sensitive-data/init_file.json")
  soilModelling_RepositoryPath <- init_file$soil_loc
  CO2emissions_RepositoryPath <- init_file$co2_emissions_loc
  source(file.path(soilModelling_RepositoryPath,"scripts","run_soil_model.R"), local = TRUE)
  source(file.path(CO2emissions_RepositoryPath, "scripts", "call_lca.R"), local = TRUE)
  
  
  ## Getting the data from mongoDB ---------------------------------------------

  # Check that only one source of farm data was provided
  if(!is.na(farmId) & !is.na(JSONfile)){
    stop("Both farmId AND JSON files were fed to the model. Please choose only one.")
  }
  
  # Check if JSONfile or farmId exists and read the farm data from the JSON file or MongoDB, respectively
  if(!is.na(JSONfile)){
    JSONfile_entered = TRUE
    farms_everything = fromJSON(JSONfile)
  } else if(!is.na(farmId)) {
    connection_string = init_file$connection_string_cfdev # Other options: init_file$connection_string_prod
    farms_collection = mongo(collection="farms", db="carbonplusdb", # Other servers (test, prod) have different db names!
                             url=connection_string)
    farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""))
  } else {stop("Neither farmId nor a JSON file were fed to the model.")}
  
  # Checking correctness and unicity of farmIds
  if (is.null(farms_everything$farmInfo)){ # Can this be TRUE? Because already used above to select the data. Move to above?
    log4r::error(my_logger, "farmId wasn't found.")
  } else if (length(farms_everything$farmInfo$farmId)>1){
    log4r::error(my_logger, 
                 paste("Multiple identical farmIds were found. Number of farmIds matching =",
                       length(farms_everything$farmInfo$farmId),".", sep="")
    )
  } else if (farms_everything$farmInfo$farmId==farmId){
    log4r::info(my_logger, paste("farm with farmId = ",farmId," has been read succesfully. 
                                  \nMail adress = ",farms_everything$farmInfo$email,'.', sep=""))
  }
  
  # Selecting only the first case if more than one farmId match in mongoDB
  if (length(farms_everything$farmInfo$farmId)>1){
    farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""),
                                             limit = 1)
    log4r::info(my_logger,paste("After multiple matches, only the first profile with farmId = ",
                                farmId," was selected.",sep=""))
  }
  
  
  ## Fetching pedo-climatic zone -----------------------------------------------
  
  farm_parameters = mongo(collection="farmparameters", 
                          db="carbonplus_production_db", 
                          url=init_file$connection_string_prod
  )
  farm_EnZ =  farm_parameters$find(paste('{"farmId":"',farmId,'"}',sep=""))
  if (length(unique(farm_EnZ$enz))==1){
    farm_EnZ = unique(farm_EnZ$enz)
    log4r::info(
      my_logger, 
      paste("farmparameters collection contain unique info on EnZ for farmId", farmId, sep=" ")
    )
  } else if (length(unique(farm_EnZ$enz))==0){
    log4r::error(
      my_logger, 
      paste("Caution: farmparameters collection doesn't contain info on EnZ for farmId", 
            farmId, 
            sep=" ")
    )
  } else if (length(unique(farm_EnZ$enz))>1){
    log4r::error(
      my_logger, 
      paste("Caution: farmparameters collection content SEVERAL EnZ for farmId",
            farmId,"leading to conflicts",
            sep=" ")
    )
  }
  
  
  ## Running the soil model and emissions calculations ---------------------
  
  step_in_table_final <- run_soil_model(init_file=init_file,
                                        pars=pars,
                                        farms_everything=farms_everything,
                                        farm_EnZ=farm_EnZ)
  
  emissions <- call_lca(init_file=init_file,
                        farms_everything=farms_everything,
                        farm_EnZ = farm_EnZ)
  
  step_in_table_final$yearly_co2emissions_diff <- emissions$total_emissions_diff_tCO2_eq[2:11]
  
  step_in_table_final <- step_in_table_final %>%
    mutate(yearly_certificates_mean = yearly_certificates_mean - yearly_co2emissions_diff)
  readLines(my_logfile)
  
  
  ## Push results to mongoDB ---------------------------------------------------
  browser()
  # Get code version info
  
  tag <- system2("git describe")
    
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
  return(step_in_table_final)
}
