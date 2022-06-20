#MongoDB parameters extraction functions

### TOOL FUNCTIONS

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

get_clay_content <- function(){
  # CAUTION - NEEDS TO BE BUILT
  return(20) #average value waiting for better, from soil maps or soil samples
}

### GET INPUT FUNCTIONS

get_add_manure_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts manure application inputs dataframe 
  add_manure_inputs = data.frame(parcel_ID = c(), scenario = c(), manure_source = c(), quantity_kg_ha = c(), imported_frac = c(), remaining_frac = c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]
      if (year_chosen$manureApplication>0){
        add_manure_inputs <- rbind(add_manure_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
          scenario = c(paste('year',j,sep="")), 
          manure_source = c("Other Cattle"), # AN UNFOLDING LIST OF MANURE TYPE MIGHT HAVE TO BE ADDED TO UI
          quantity_kg_ha = c(as.numeric(year_chosen$manureApplication)), 
          imported_frac = c(as.numeric(year_chosen$percentManureImported)), 
          remaining_frac = c(1)))
      }
      if (year_chosen$compostApplication>0){
        add_manure_inputs <- rbind(add_manure_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
          scenario = c(paste('year',j,sep="")), 
          manure_source = c("Green compost"), # CAUTION the fact that compost entry is GREEN compost might have to be specified
          quantity_kg_ha = c(as.numeric(year_chosen$compostApplication)), 
          imported_frac = c(as.numeric(year_chosen$percentCompostImported)), 
          remaining_frac = c(1)))
      }
      if (year_chosen$hayStrawApplication>0){
        add_manure_inputs <- rbind(add_manure_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
          scenario = c(paste('year',j,sep="")), 
          manure_source = c("Hay"),
          quantity_kg_ha = c(as.numeric(year_chosen$hayStrawApplication)), 
          #imported_frac = c(year_chosen$percentHawStrawImported),# CAUTION this attribute need to be added in landUseSchema by Suhas
          remaining_frac = c(ifelse(year_chosen$baleGrazing=="Yes",
                                    as.numeric(year_chosen$residueLeftAfterBaleGrazing),1)))) #CAUTION, check that baleGrazing actually takes values "Yes" and "No"
      }
    }
  }
  return(add_manure_inputs)
}

get_agroforestry_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts agroforestry inputs dataframe 
  agroforestry_inputs = data.frame(parcel_ID = c(), scenario = c(), tree_species = c(), dbh = c(),
                                   tree_density = c(), area = c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      for (k in c(1:3)){
        year_chosen = landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]
        agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
          scenario = c(paste('year',j,sep="")), 
          tree_species = c(year_chosen$species[[k]]),
          dbh = c(year_chosen$typeOfTrees[[k]]$treeAvgDBH), 
          tree_density = c(year_chosen$typeOfTrees[[k]]$avgNoOfTrees), 
          area = c(as.numeric(landUseSummaryOrPractices[[i]]$area)/10000)))
      }
    }
  }
  return(agroforestry_inputs)
}

get_animal_inputs = function(landUseSummaryOrPractices,livestock){
  # takes landUseSummaryOrPractices & livestock from farms collection
  # extracts animal inputs dataframe 
  animal_inputs = data.frame(parcel_ID = c(), scenario = c(), species = c(), n_animals = c(), grazing_days = c(),
                             area = c(), grazing_management = c(), productivity = c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c("currentManagement","futureManagement")){
      for (k in c(1:length(livestock[[j]]))){
        year_chosen = landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]
        animal_inputs <- rbind(animal_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
          scenario = c(paste('year',j,sep="")), 
          species = c(livestock[[j]][[k]]$species),
          # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
          n_animals = c(livestock[[j]][[k]]$numberOfHeads*
                          extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i)/
                          extract_total_grazing_amount(landUseSummaryOrPractices)), 
          grazing_days = c(livestock[[j]][[k]]$grazingOrPasturedDaysPerYear), 
          area = c(as.numeric(landUseSummaryOrPractices[[i]]$area)/10000)),
          grazing_management = c(livestock[[j]][[k]]$management), 
          productivity = c(livestock[[j]][[k]]$management)) # CAUTION, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
      }
    }
  }
  return(animal_inputs)
}

get_bare_field_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts bare soil inputs dataframe 
  bare_field_inputs = data.frame(parcel_ID = c(), scenario = c())
  for (k in c(1:12)){
    bare_field_inputs[[paste("bare_profile_",k,sep="")]] = c()
  }
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]
      bare_field_inputs_temp = data.frame(parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
                                          scenario = c(paste('year',j,sep="")))
      for (k in c(1:12)){
        bare_field_inputs_temp[[paste("bare_profile_",k,sep="")]] = ifelse(
          year_chosen$bareSoilFallow[[k]][[1]]=="Yes", TRUE, FALSE)
      }
      bare_field_inputs <- rbind(bare_field_inputs,bare_field_inputs_temp)
    }
  }
  return(bare_field_inputs)
}

get_parcels_input = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  parcels_input = data.frame(parcel_ID = c(), area = c(), longitude = c(),latitude=c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    parcels_input <- rbind(parcels_input,data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
      area = c(as.numeric(landUseSummaryOrPractices[[i]]$area)/10000), 
      longitude = c(as.numeric(extract_longitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i))),
      latitude=c(as.numeric(extract_latitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i)))))
  }
  return(parcels_input)
}

get_soil_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  soil_inputs = data.frame(parcel_ID = c(), clay = c(), irrigation=c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
    soil_inputs <- rbind(soil_inputs,data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName),
      clay = c(get_clay_content()),
      irrigation = c(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$irrigation)))
    }
  }
  return(soil_inputs)
}

get_tilling_inputs = function(landUseSummaryOrPractices, tilling_factors, farm_EnZ){ 
  # takes landUseSummaryOrPractices from farms collection, farm_country (from farmInfo) and tilling factors table
  # extracts tilling inputs dataframe 
  
  # CAUTION --- SHOULD RATHER USE LOCATION AND BE BASED ON PEDOCLIMATIC ZONE
  tilling_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$tilling_factor
  minimum_tillage_factor = (tilling_factors %>% filter(country == farm_country))$minimum_tillage_factor
  tilling_inputs = data.frame(parcel_ID = c(), scenario = c(), tilling_factor = c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      bare_field_inputs_temp = 
        for (k in c(1:12)){
          tilling_inputs <- rbind(tilling_inputs, data.frame(
            year_chosen = landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]],
            parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
            scenario = c(paste('year',j,sep=""))),
            tilling_factor = ifelse(year_chosen$tillingEvent[[k]][[1]]=="Yes", tilling_factor, 1),
            minimum_tillage_factor = ifelse(year_chosen$minimumTillingEvent[[k]][[1]]=="Yes", minimum_tillage_factor, 1))
        }
    }
  }
  return(bare_field_inputs)
}