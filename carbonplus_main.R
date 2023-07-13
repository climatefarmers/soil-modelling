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
    n_run = 30,
    sd_future_mod=1,
    sd_field_carbon_in=0.10,
    CFmade_grazing_estimations_Yes_No="No"
    )
  
  # To copy the practice of a single year to all others
  yearX_landuse <- 1  # setting to 0 will copy baseline
  yearX_livestock <- 1  # setting to 0 will copy baseline
  copy_yearX_to_following_years_landUse <- FALSE
  copy_yearX_to_following_years_livestock <- FALSE
  
  server <- "dev"  # One of: "prod", dev", "test"
  
  
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
  
  
  ## Get the farm data from the JSON file or MongoDB ---------------------------

  # Check that only one source of farm data was provided
  if(!is.na(farmId) & !is.na(JSONfile)){
    stop("Both farmId AND JSON files were passed to the model. Please choose only one.")
  }
  
  if(is.na(farmId) & is.na(JSONfile)){
    stop("Both farmId AND JSON files are missing. One must be passed.")
  }
  
  if(!is.na(JSONfile)){
    JSONfile_entered = TRUE
    farms_everything = fromJSON(JSONfile)
  }
  
  if(!is.na(farmId)) {
    if(server == "prod") {
      connection_string = init_file$connection_string_prod
      db <- "carbonplus_production_db"
    } else if(server == "dev") {
      connection_string = init_file$connection_string_cfdev
      db <- "carbonplusdb"
    } else if(server == "test") {
      connection_string = init_file$connection_string_test
      db <- "test_server_db"
    } else {stop("Wrong value for variable: server")}
    farms_collection = mongo(collection="farms", db=db, url=connection_string)
    farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""))
  }
  
  
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
  
  soil_results_out <- run_soil_model(init_file=init_file,
                                        pars=pars,
                                        farms_everything=farms_everything,
                                        farm_EnZ=farm_EnZ)
  
  soil_results_yearly <- soil_results_out[[1]]
  soil_results_monthly <- soil_results_out[[2]]
  
  emissions <- call_lca(init_file=init_file,
                        farms_everything=farms_everything,
                        farm_EnZ = farm_EnZ)

  yearly_results <- soil_results_yearly %>%
    mutate(CO2eq_soil_final=yearly_CO2diff_final,
           CO2eq_soil_mean=yearly_CO2diff_mean,
           CO2eq_soil_sd=yearly_CO2diff_sd) %>% 
    select(year, CO2eq_soil_final, CO2eq_soil_mean, CO2eq_soil_sd) %>%
    mutate(CO2eq_emissions=emissions$emissions_diff_tCO2_eq[2:11],
           CO2eq_leakage=emissions$leakage_tCO2_eq[2:11])
  
  yearly_results <- yearly_results %>%
    mutate(CO2eq_total = CO2eq_soil_final - CO2eq_emissions - CO2eq_leakage)
  
  readLines(my_logfile)
  
  
  ## Push results to mongoDB ---------------------------------------------------
  
  # Get code version and time info
  tag <- system2(command = "git", args = "describe", stdout = TRUE)
  full_tag <- paste0("R-model-version: ", tag)
  currentTime <- format(Sys.time(), "%Y-%m-%d %H:%M")
  currentYear <- format(Sys.time(), "%Y")
  
  farms_everything$modelInfo <- data.frame(
    modelVersion=full_tag,
    resultsGenerationYear=currentYear,
    resultsGenerationTime=currentTime,
    n_runs=pars['n_run']
  )

  farms_everything$modelResults <- data.frame(
    yearlyCO2eqTotal=NA,
    yearlyCO2eqSoil=NA,
    yearlyCO2eqEmissions=NA,
    yearlyCO2eqLeakage=NA
  )
  farms_everything$modelResults$yearlyCO2eqTotal=list(c(yearly_results$CO2eq_total))
  farms_everything$modelResults$yearlyCO2eqSoil=list(c(yearly_results$CO2eq_soil_final))
  farms_everything$modelResults$yearlyCO2eqEmissions=list(c(yearly_results$CO2eq_emissions))
  farms_everything$modelResults$yearlyCO2eqLeakage=list(c(yearly_results$CO2eq_leakage))
  
  farms_everything$modelParameters <- data.frame(pars) 

  # Upload to database
  carbonresults_collection = mongo(collection="carbonresults", db=db, url=connection_string)
  carbonresults_collection$insert(farms_everything)
  
  
  ## Plotting ------------------------------------------------------------------
  name<-paste0("Results_farm_", farmId)
  graph <- ggplot(data = soil_results_monthly, aes(x = time, y = SOC_farm_mean, colour=scenario)) +
    geom_line()+
    #geom_errorbar(aes(ymin=SOC_farm_mean-SOC_farm_sd, ymax=SOC_farm_mean+SOC_farm_sd), width=.1) +
    scale_color_manual(values = c("darkred","#5CB85C"),labels = c("Modern-day","Holistic"))+
    theme(legend.position = "bottom")+
    labs(title = name)+
    xlab("Time")+
    ylab("SOC (in tonnes per hectare)")
  print(graph)
  # # png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  # # print(graph)
  # # dev.off()
  # 
  #   name<-paste("Results_farm_",project_name,sep = "")
  #   graph <- ggplot(data = yearly_results, aes(x=year, group = 1)) +
  #     geom_bar(aes(y = baseline_step_total_CO2_mean), stat="identity", fill="darkred", alpha=0.7)+
  #     geom_errorbar(aes(ymin = baseline_step_total_CO2_mean-1.96*sqrt(baseline_step_total_CO2_var),
  #                       ymax = baseline_step_total_CO2_mean+1.96*sqrt(baseline_step_total_CO2_var)), colour="black", width=.5)+
  #     geom_bar(aes(y = holistic_step_total_CO2_mean), stat="identity", fill="#5CB85C", alpha=0.7)+
  #     geom_errorbar(aes(ymin = holistic_step_total_CO2_mean-1.96*sqrt(holistic_step_total_CO2_var),
  #                       ymax = holistic_step_total_CO2_mean+1.96*sqrt(holistic_step_total_CO2_var), color = "95% CI"), colour="black", width=.5, show.legend = T)+
  #     labs(title = name)+
  #     xlab("Time")+
  #     ylab("tCO2 sequestered (each year)")
  #   print(graph)
  #   # png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  #   # print(graph)
  #   # dev.off()
  
  histogram <- ggplot(yearly_results, aes(x=year, group = 1)) +
    geom_bar(aes(y=CO2eq_soil_mean), stat="identity", fill="#5CB85C", alpha=0.7) +
    geom_errorbar(aes(ymin = CO2eq_soil_mean-1.96*CO2eq_soil_sd,
                      ymax = CO2eq_soil_mean+1.96*CO2eq_soil_sd, color = "95% CI"), colour="black", width=.5, show.legend = T) +
    geom_bar(aes(y=CO2eq_emissions), stat="identity", fill="#0C785C", alpha=0.7) +
    geom_bar(aes(y=CO2eq_total), stat="identity", fill="darkred", alpha=0.7) +
    xlab("Time")+
    ylab("Number of certificates issuable (per year)")
  print(histogram)
  # png(file.path(project_loc, project_name, "results", paste0(farmId, ".png")))
  # print(histogram)
  # dev.off()
  
  ## End function --------------------------------------------------------------
  return(yearly_results)
}
