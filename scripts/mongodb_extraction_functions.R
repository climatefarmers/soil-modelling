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
  #takes a landUseSummaryOrPractices from farms collection and a parcel index
  #extracts the mean longitude of parcel's corners
  longitudes = c()
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$coordinates))){
    longitudes <- append(longitudes,landUseSummaryOrPractices[[parcel_index]]$coordinates[[i]][[1]])}
  return(mean(longitudes))
}

extract_total_grazing_amount <- function(landUseSummaryOrPractices){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts the overall grazing yield and bale grazing yield from the whole farm
  bale_grazing_yield = 0
  grazing_yield = 0
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      for (k in c(1:12)){
        if (landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$grazing[[k]][[1]]=="Yes"){
          grazing_yield = grazing_yield + as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$harvestGrazingYiel[[k]][[1]])
        }
        if (landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$baleGrazing=="Yes"){
          bale_grazing_yield = bale_grazing_yield + as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$hayStrawApplication)*
            as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$residueLeftAfterBaleGrazing)
        }
      }
    }
  }
  return(bale_grazing_yield+grazing_yield)
}

extract_grazing_amount_parcel_i <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and a parcel index i
  #extracts grazing yield and bale grazing yield from parcel index i
  bale_grazing_yield = 0
  grazing_yield = 0
  for (j in c(0:10)){
    for (k in c(1:12)){
      if (landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$grazing[[k]][[1]]=="Yes"){
        grazing_yield = grazing_yield + as.numeric(landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$harvestGrazingYiel[[k]][[1]])
      }
      if (landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$baleGrazing=="Yes"){
        bale_grazing_yield = bale_grazing_yield + as.numeric(landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$hayStrawApplication)*
          as.numeric(landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$residueLeftAfterBaleGrazing)
      }
    }
  }
  return(bale_grazing_yield+grazing_yield)
}