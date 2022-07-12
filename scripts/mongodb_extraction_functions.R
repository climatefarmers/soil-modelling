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
      if (landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$baleGrazing=="Yes"){
        bale_grazing_yield = bale_grazing_yield + as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$hayStrawApplication)*
          as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$residueLeftAfterBaleGrazing)
      }
      for (k in c(1:12)){
        if (landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$grazing[[k]][[1]]=="Yes"){
          grazing_yield = grazing_yield + as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$harvestGrazingYiel[[k]][[1]])
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
    if (landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$baleGrazing=="Yes"){
      bale_grazing_yield = bale_grazing_yield + as.numeric(landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$hayStrawApplication)*
        as.numeric(landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$residueLeftAfterBaleGrazing)
    }
    for (k in c(1:12)){
      if (landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$grazing[[k]][[1]]=="Yes"){
        grazing_yield = grazing_yield + as.numeric(landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$harvestGrazingYiel[[k]][[1]])
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

get_parcel_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  parcel_inputs = data.frame(parcel_ID = c(), area = c(), longitude = c(),latitude=c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    parcel_inputs <- rbind(parcel_inputs,data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
      area = c(as.numeric(landUseSummaryOrPractices[[i]]$area)/10000), 
      longitude = c(as.numeric(extract_longitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i))),
      latitude=c(as.numeric(extract_latitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i)))))
  }
  return(parcel_inputs)
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
  minimum_tillage_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$minimum_tillage_factor
  tilling_inputs = data.frame(parcel_ID = c(), scenario = c(), tilling_factor = c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]
      for (k in c(1:12)){
        tilling_inputs <- rbind(tilling_inputs, data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
          scenario = c(paste('year',j,sep="")),
          tilling_factor = c(ifelse(year_chosen$tillingEvent[[1]][[k]]=="Yes", tilling_factor, 
                                  ifelse(year_chosen$minimumTillingEvent[[1]][[k]]=="Yes", minimum_tillage_factor, 1)))))
      }
    }
  }
  return(bare_field_inputs)
}

get_pasture_inputs <- function(landUseSummaryOrPractices, grazing_factors, farm_EnZ){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts yield and residues left on site when grazing happening
  pasture_efficiency_potential_difference = unique((grazing_factors %>% filter(pedo_climatic_area==farm_EnZ))$pasture_efficiency_potential_difference)
  previous_AMP_years = ifelse(landUseSummaryOrPractices[[parcel_index]][['year0']]$adaptiveMultiPaddockGrazing$usage=="Yes", nbYears_initialLandUse_wasApplied, 0) 
  current_AMP_years = 0
  endWinterSeason = 5 # month index
  endSummerSeason = 10 # month index
  pasture_inputs = data.frame(scenario = c(), parcel_ID = c(), grass = c(), perennial_frac = c(), n_fixing_frac = c(), 
                              dry_yield = c(), fresh_yield = c(), dry_residual = c(), fresh_residual = c(), 
                              ag_dry_peak = c(), ag_fresh_peak = c(), pasture_efficiency = c())
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      #counting AMP years to calculate related efficiency
      current_AMP_years = ifelse(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$adaptiveMultiPaddockGrazing$usage=="Yes",
                                 1, ifelse(previous_AMP_years+current_AMP_years>0,-1,0)) # efficiency is assumed to be reversable
      # calculate relating efficiency if it was a permanent grassland
      pasture_efficiency = 1 + pasture_efficiency_potential_difference *
        (exp(-0.36*previous_AMP_years)-exp(-0.36*(previous_AMP_years+current_AMP_years)))#0.36 factor allows to reach 2/3 of AMP efficiency increase after 3 years of AMP
      # selecting the right type of landuse were grazing manangement affects most pasture efficiency 
      if(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$landUseType=='Arable Crops'){
        pasture_efficiency= mean(1,pasture_efficiency) # assumption that grazing management impacts less arable crops
        }
      # monthly yield and residue
      monthly_grazing_yield = data.frame(grazing_yield=rep(0,12),residue_left=rep(0,12))
      for (k in c(1:12)){
        # grazing without cash crop
        if (landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$grazing[[k]][[1]]=="Yes" &
            landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$cashCropMonthlyData[[k]][[1]]=="-"){ #cash crop not grazed
          monthly_grazing_yield$grazing_yield[k] = as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$harvestGrazingYiel[[k]][[1]])
          monthly_grazing_yield$residue_left[k] = as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$estimatedPlantResidue[[k]][[1]])
        }
        # grazing cash crop residues
        if (landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$grazing[[k]][[1]]=="Yes" &
            landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$cashCropMonthlyData[[k]][[1]]!="-"){ #cash crop not grazed but residue might be
          monthly_grazing_yield$grazing_yield[k] = as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$estimatedPlantResidue[[k]][[1]])
        }
      }
      # biomass peak that will be used to calculate root biomass
      indexWinterSeasonPeak = which.max(monthly_grazing_yield$grazing_yield[c(1:endWinterSeason,(endSummerSeason+1):12)])
      indexSummerSeasonPeak = which.max(monthly_grazing_yield$grazing_yield[c((endWinterSeason+1):endSummerSeason)])
      # building df for C inputs calculation
      pasture_inputs <- rbind(pasture_inputs, 
                              data.frame(scenario = c(paste('year',j,sep="")),
                                         parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName), 
                                         perennial_frac = c(ifelse(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$landUseType=='Arable Crops', 
                                                                   0, (previous_AMP_years+current_AMP_years)*0.02)), #CAUTION assumption that perennials increase linearly with AMP by a rate of 2% per year
                                         n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                         dry_yield = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$grazing_yield),0)), 
                                         fresh_yield = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$grazing_yield),0)), 
                                         dry_residual = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$residue_left),0)), 
                                         fresh_residual = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$residue_left),0)), 
                                         ag_dry_peak = c(ifelse(dryOrFresh=="Dry", (monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[endWinterSeason] +
                                                                  (monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[endSummerSeason],0)), 
                                         ag_fresh_peak = c(ifelse(dryOrFresh=="Fresh", (monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[endWinterSeason] +
                                                                    (monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[endSummerSeason],0)), 
                                         pasture_efficiency = c(pasture_efficiency) ))
    }
  }
  return(pasture_inputs)
}

get_crop_inputs <- function(landUseSummaryOrPractices){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts harvesting yield and residue left on site
  endWinterSeason = 5 # month index
  endSummerSeason = 10 # month index
  crop_inputs = data.frame(scenario = c(), parcel_ID = c(), crop = c(), n_fixing_frac = c(), 
                           dry_yield = c(), fresh_yield = c(), dry_residual = c(), fresh_residual = c(), 
                           ag_dry_peak = c(), ag_fresh_peak = c() )
  for (i in c(1:length(landUseSummaryOrPractices))){
    for (j in c(0:10)){
      # monthly yield and residue
      monthly_harvesting_yield = data.frame(crop=rep('-',12), coverCrop=rep('No',12), productiveFallow=rep('No',12),
                                            harvesting_yield=rep(0,12),residue_left=rep(0,12))
      for (k in c(1:12)){
        monthly_harvesting_yield$crop = landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$cashCropMonthlyData[[k]][[1]]
        monthly_harvesting_yield$coverCrop = landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$coverCropMonthlyData[[k]][[1]]
        monthly_harvesting_yield$productiveFallow = landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$productiveFallow[[k]][[1]]
        monthly_harvesting_yield$grazing = landUseSummaryOrPractices[[parcel_index]][[paste('year',j,sep="")]]$grazing[[k]][[1]]
        monthly_harvesting_yield$harvesting_yield[k] = as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$harvestGrazingYield[[k]][[1]])
        monthly_harvesting_yield$residue_left[k] = as.numeric(landUseSummaryOrPractices[[i]][[paste('year',j,sep="")]]$estimatedPlantResidue[[k]][[1]])
      }
      # case of cash crop with no grazing
      for (crop in unique(monthly_harvesting_yield$crop)){
        if (crop!='-'){ #cash crops
          harvesting_yield = sum((monthly_harvesting_yield %>% filter(crop==crop))$harvesting_yield) #all cash crops are harvested
          residue_left = sum((monthly_harvesting_yield %>% filter(crop==crop & grazing=="No"))$residue_left) #if grazed, counted in pasture inputs
          crop_inputs <- rbind(crop_inputs, 
                               data.frame(scenario = c(paste('year',j,sep="")),
                                          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName),
                                          crop = crop,
                                          n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                          dry_yield = c(ifelse(dryOrFresh=="Dry", harvesting_yield,0)), 
                                          fresh_yield = c(ifelse(dryOrFresh=="Fresh", harvesting_yield,0)), 
                                          dry_residual = c(ifelse(dryOrFresh=="Dry", residue_left,0)), 
                                          fresh_residual = c(ifelse(dryOrFresh=="Fresh", residue_left,0)), 
                                          ag_dry_peak = c(ifelse(dryOrFresh=="Dry", max(harvesting_yield),0)), 
                                          ag_fresh_peak = c(ifelse(dryOrFresh=="Fresh",  max(harvesting_yield),0))))
          
          
        }
        
        if (crop=='-'){# no cash crop
          # with no grazing -> assumed to be generic grass
          harvesting_yield = sum((monthly_harvesting_yield %>% filter(crop==crop & grazing=="No"))$harvesting_yield)
          residue_left = sum((monthly_harvesting_yield %>% filter(crop==crop & grazing=="No"))$residue_left)
          crop_inputs <- rbind(crop_inputs, 
                               data.frame(scenario = c(paste('year',j,sep="")),
                                          parcel_ID = c(landUseSummaryOrPractices[[i]]$parcelName),
                                          crop = "Generic annual grasses",
                                          n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                          dry_yield = c(ifelse(dryOrFresh=="Dry", harvesting_yield,0)), 
                                          fresh_yield = c(ifelse(dryOrFresh=="Fresh", harvesting_yield,0)), 
                                          dry_residual = c(ifelse(dryOrFresh=="Dry", residue_left,0)), 
                                          fresh_residual = c(ifelse(dryOrFresh=="Fresh", residue_left,0)), 
                                          ag_dry_peak = c(ifelse(dryOrFresh=="Dry", max(harvesting_yield),0)), 
                                          ag_fresh_peak = c(ifelse(dryOrFresh=="Fresh",  max(harvesting_yield),0))))
          
          
        }
      }
    }
  }
  return(crop_inputs)
}
