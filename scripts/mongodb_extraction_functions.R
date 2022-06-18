#MongoDB parameters extraction functions


extract_latitude_landUseSummaryOrPractices <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and the inde corresping to the parcel.
  #extracts the mean latitude of parcel's corners
  latitudes = c()
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$coordinates))){
    latitudes <- append(latitudes,landUseSummaryOrPractices[[parcel_index]]$coordinates[[i]][[2]])}
  return(mean(latitudes))
}

extract_longitude_landUseSummaryOrPractices <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and the inde corresping to the parcel.
  #extracts the mean longitude of parcel's corners
  longitudes = c()
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$coordinates))){
    longitudes <- append(longitudes,landUseSummaryOrPractices[[parcel_index]]$coordinates[[i]][[1]])}
  return(mean(longitudes))
}
