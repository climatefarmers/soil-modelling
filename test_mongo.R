library(pacman)
p_load('pacman', 'mongolite', 'dplyr', 'tidyverse',
        'readr', 'jsonlite')

sensitive_data_loc <- "../sensitive-data"

init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))

farmId <- 'edf5cce8-eee2-40a8-af32-520d2b93ab5c' # Troya

server <- "test"

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

# Get code version and time info
tag <- system2(command = "git", args = "describe", stdout = TRUE)
full_tag <- paste0("R-model-version: test ", tag)
currentTime <- format(Sys.time(), "%Y-%m-%d %H:%M")
currentYear <- format(Sys.time(), "%Y")

farms_everything$runInfo <- data.frame(
  modelVersion=full_tag,
  resultsGenerationYear=currentYear,
  resultsGenerationTime=currentTime
  )

# Upload to database
carbonresults_collection = mongo(collection="carbonresults", db=db, url=connection_string)

carbonresults_collection$insert(farms_everything)


