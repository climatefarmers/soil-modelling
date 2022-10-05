

initialise_Rscripts <- function(JSONobject){
  readJSONobject <- fromJSON(JSONobject)
  farmId <- readJSONobject$farmId
  source("carbonplus_trigger_Rscripts.R")
  launching_Rscripts(farmId=farmId)
}