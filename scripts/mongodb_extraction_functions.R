#MongoDB parameters extraction functions

### TOOL FUNCTIONS

extract_latitude_landUseSummaryOrPractices <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and the inde corresping to the parcel.
  #extracts the mean latitude of parcel's corners
  latitudes = c()
  for (parcels in landUseSummaryOrPractices[[1]]$coordinates){
    for (i in c(1:nrow(parcels))){
      latitudes <- append(latitudes,parcels[[i,2]])
    }
  }
  return(mean(latitudes))
}
#schema_fixed
extract_longitude_landUseSummaryOrPractices <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and a parcel index
  #extracts the mean longitude of parcel's corners
  longitudes = c()
  for (parcels in landUseSummaryOrPractices[[1]]$coordinates){
    for (i in c(1:nrow(parcels))){
      longitudes <- append(longitudes,parcels[[i,1]])
    }
  }
  return(mean(longitudes))
}
#schema_fixed
extract_total_grazing_amount <- function(landUseSummaryOrPractices, year = j){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts the overall grazing yield and bale grazing yield from the whole farm
  bale_grazing_yield = 0
  grazing_yield = 0
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i]=="Yes"){
      bale_grazing_yield = bale_grazing_yield + as.numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$hayStrawApplication[i])*
        as.numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[i])
    }
    for (k in c(1:12)){
      if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazing[i][[1]][[k]]=="Yes"){
        grazing_yield = grazing_yield + as.numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$harvestGrazingYiel[i][[1]][[k]])
      }
    }
  }
  return(bale_grazing_yield+grazing_yield)
}
#schema_fixed
extract_grazing_amount_parcel_i <- function(landUseSummaryOrPractices, parcel_index = i, year = j){
  #takes a landUseSummaryOrPractices from farms collection and a parcel index i
  #extracts grazing yield and bale grazing yield from parcel index i
  bale_grazing_yield = 0
  grazing_yield = 0
  if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[parcel_index]=="Yes"){
    bale_grazing_yield = bale_grazing_yield + as.numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$hayStrawApplication[parcel_index])*
      as.numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[parcel_index])
  }
  for (k in c(1:12)){
    if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazing[parcel_index][[1]][[k]]=="Yes"){
      grazing_yield = grazing_yield + as.numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$harvestGrazingYiel[parcel_index][[1]][[k]])
    }
  }
  
  return(bale_grazing_yield+grazing_yield)
}
#schema_fixed
get_clay_content <- function(soilAnalysis, soilMapsData){
  if (is.na(soilAnalysis$clayContentPercent)==FALSE){
    if(5<as.numeric(soilAnalysis$clayContentPercent) & as.numeric(soilAnalysis$clayContentPercent)<80){
      return(as.numeric(soilAnalysis$clayContentPercent))
    } else {
      log4r::error(my_logger, paste("Clay content input = ", 
                                    as.numeric(soilAnalysis$clayContentPercent),
                                    "%. Check unit/values with farmer.", sep=""))
    }
  }
  if (is.na(soilAnalysis$clayContentPercent)==TRUE){
    return(soilMapsData$clay)
  }
}

get_SOC_content <- function(soilAnalysis, soilMapsData){
  if (is.na(soilAnalysis$carbonContent)==FALSE){ # SOC in t/ha = g/kg
    if(4<as.numeric(soilAnalysis$carbonContent) & as.numeric(soilAnalysis$carbonContent)<40){
      return(as.numeric(soilAnalysis$carbonContent))
    } 
    if (0.35<as.numeric(soilAnalysis$carbonContent) & as.numeric(soilAnalysis$carbonContent)<4){ #SOC in %
      return(as.numeric(soilAnalysis$carbonContent)*10)
    } else {
      log4r::error(my_logger, paste("SOC content input = ", as.numeric(soilAnalysis$carbonContent),
                                    soilAnalysis$carbonContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
  if (is.na(soilAnalysis$carbonContent)==TRUE & is.na(soilAnalysis$organicMatterContent)==FALSE){ # SOC in t/ha = g/kg
    if(8<as.numeric(soilAnalysis$organicMatterContent) & as.numeric(soilAnalysis$organicMatterContent)<80 & soilAnalysis$organicMatterContentMetric!="%"){
      return(as.numeric(soilAnalysis$organicMatterContent)*0.55)
    } 
    if (0.7<as.numeric(soilAnalysis$organicMatterContent) & as.numeric(soilAnalysis$organicMatterContent)<8){ #SOC in %
      return(as.numeric(soilAnalysis$organicMatterContent)*5.5)
    } else {
      log4r::error(my_logger, paste("OM content input = ", as.numeric(soilAnalysis$organicMatterContent),
                                    soilAnalysis$organicMatterContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
  if (is.na(soilAnalysis$clayContentPercent)==TRUE & is.na(soilAnalysis$organicMatterContent)==TRUE){
    return(soilMapsData$SOC)
  }
}

### GET INPUT FUNCTIONS

get_add_manure_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts manure application inputs dataframe 
  add_manure_inputs = data.frame(parcel_ID = c(), scenario = c(), manure_source = c(), quantity_kg_ha = c(), imported_frac = c(), remaining_frac = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
      if (year_chosen$manureApplication[i]>=0){
        add_manure_inputs <- rbind(add_manure_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = c(paste('year',j,sep="")), 
          manure_source = c("Other Cattle"), # AN UNFOLDING LIST OF MANURE TYPE MIGHT HAVE TO BE ADDED TO UI
          quantity_kg_ha = c(as.numeric(year_chosen$manureApplication[i])), 
          imported_frac = c(as.numeric(year_chosen$percentManureImported[i])), 
          remaining_frac = c(1)))
      }
      if (year_chosen$compostApplication[i]>=0){
        add_manure_inputs <- rbind(add_manure_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = c(paste('year',j,sep="")), 
          manure_source = c("Green compost"), # CAUTION the fact that compost entry is GREEN compost might have to be specified
          quantity_kg_ha = c(as.numeric(year_chosen$compostApplication[i])), 
          imported_frac = c(as.numeric(year_chosen$percentCompostImported[i])), 
          remaining_frac = c(1)))
      }
      if (year_chosen$hayStrawApplication[i]>=0){
        add_manure_inputs <- rbind(add_manure_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = c(paste('year',j,sep="")), 
          manure_source = c("Hay"),
          quantity_kg_ha = c(as.numeric(year_chosen$hayStrawApplication[i])), 
          imported_frac = c(as.numeric(year_chosen$percentageOfHayStrawImported[i])),# CAUTION this attribute need to be added in landUseSchema by Suhas
          remaining_frac = c(ifelse(year_chosen$baleGrazing=="Yes",
                                    as.numeric(year_chosen$residueLeftAfterBaleGrazing[i]),1))))
      }
    }
  }
  add_manure_inputs <- rbind(add_manure_inputs,add_manure_inputs%>%
                               filter(scenario=='year0')%>%
                               mutate(scenario='baseline')) # Manure addition baseline is based on previous years
  return(add_manure_inputs)
}
#schema_fixed
get_agroforestry_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts agroforestry inputs dataframe 
  agroforestry_inputs = data.frame(parcel_ID = c(), scenario = c(), tree_species = c(), dbh = c(),
                                   tree_density = c(), area = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      row_index = 0
      c = c()
      for (tree in landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$typeOfTrees[i][[1]]$treeName){
        row_index = row_index + 1
        if (tree!=""){ #filter out if no tree information given
          c = append(c, row_index)
        }
      }
      typeOfTrees = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$typeOfTrees[i][[1]][c,]
      if(nrow(typeOfTrees)>0){
        for (k in c(1:nrow(typeOfTrees))){
          agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(paste('year',j,sep="")), 
            tree_species = c(typeOfTrees$treeName[[k]]),
            dbh = c(as.numeric(typeOfTrees$treeAvgDBH[[k]])), 
            tree_density = c(as.numeric(typeOfTrees$avgNoOfTrees[[k]])), 
            area = c(as.numeric(landUseSummaryOrPractices[[1]]$area[i])/10000)))
          if (j==0){ #baseline based on pre-project trees
            for (k in c(1:nrow(typeOfTrees))){
              agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
                parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                scenario = c("baseline"), 
                tree_species = c(typeOfTrees$treeName[[k]]),
                dbh = c(as.numeric(typeOfTrees$treeAvgDBH[[k]])), 
                tree_density = c(as.numeric(typeOfTrees$avgNoOfTrees[[k]])), 
                area = c(as.numeric(landUseSummaryOrPractices[[1]]$area[i])/10000)))
            }
          }
        }
      }
    }
  }
  return(agroforestry_inputs)
}
#schema_fixed
get_animal_inputs = function(landUseSummaryOrPractices,livestock){
  # takes landUseSummaryOrPractices & livestock from farms collection
  # extracts animal inputs dataframe 
  animal_inputs = data.frame(parcel_ID = c(), scenario = c(), species = c(), n_animals = c(), grazing_days = c(),
                             area = c(), grazing_management = c(), productivity = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    status ="currentManagement"
    for (k in c(1:nrow(livestock[[status]][[1]]))){
      animal_inputs <- rbind(animal_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        scenario = c(paste('year',0,sep="")), 
        species = c(livestock[[status]][[1]]$species[[k]]),
        # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
        n_animals = c(as.numeric(livestock[[status]][[1]]$numberOfHeads[[k]])*
                        extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,0)/
                        extract_total_grazing_amount(landUseSummaryOrPractices,0)), 
        grazing_days = c(as.numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]])), 
        area = c(as.numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
        grazing_management = c(livestock[[status]][[1]]$management[[k]]), 
        productivity = c(livestock[[status]][[1]]$management[[k]]))) # CAUTION, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
    }
    # baseline based on year 0 livestock
    for (k in c(1:nrow(livestock[[status]][[1]]))){
      animal_inputs <- rbind(animal_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        scenario = c("baseline"), 
        species = c(livestock[[status]][[1]]$species[[k]]),
        # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
        n_animals = c(as.numeric(livestock[[status]][[1]]$numberOfHeads[[k]])*
                        extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,0)/
                        extract_total_grazing_amount(landUseSummaryOrPractices,0)), 
        grazing_days = c(as.numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]])), 
        area = c(as.numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
        grazing_management = c("Set Stock Grazing"), 
        productivity = c(livestock[[status]][[1]]$management[[k]]))) # CAUTION, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
    }
    status ="futureManagement"
    for (year in c(1:10)){
      scenario = c(paste('year',year,sep=""))
      for (k in c(1:nrow(livestock[[status]][[1]][[scenario]]))){
        animal_inputs <- rbind(animal_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = scenario, 
          species = c(livestock[[status]][[1]][[scenario]]$species[[k]]),
          # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
          n_animals = c(as.numeric(livestock[[status]][[1]][[scenario]]$numberOfHeads[[k]])*
                          extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,year)/
                          extract_total_grazing_amount(landUseSummaryOrPractices,year)), 
          grazing_days = c(as.numeric(livestock[[status]][[1]][[scenario]]$grazingOrPasturedDaysPerYear[[k]])), 
          area = c(as.numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
          grazing_management = c(livestock[[status]][[1]][[scenario]]$management[[k]]), 
          productivity = c(livestock[[status]][[1]][[scenario]]$management[[k]]))) # CAUTION, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
      }
    }
  }
  return(animal_inputs)
}
#schema_fixed
get_bare_field_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts bare soil inputs dataframe 
  bare_field_inputs = data.frame(parcel_ID = c(), scenario = c())
  for (k in c(1:12)){
    bare_field_inputs[[paste("bare_profile_",k,sep="")]] = c()
  }
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    bare_field_inputs_temp <- data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                         scenario = c('baseline'))
    bare_field_inputs <- rbind(bare_field_inputs,cbind(bare_field_inputs_temp, soil_cover_data %>% filter(pedo_climatic_area == farm_EnZ) %>%
                                                         select(-country,-pedo_climatic_area)))
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
      bare_field_inputs_temp = data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                          scenario = c(paste('year',j,sep="")))
      for (k in c(1:12)){
        bare_field_inputs_temp[[paste("bare_profile_",k,sep="")]] = ifelse(
          year_chosen$bareSoilFallow[[1]][[k]]=="Yes", TRUE, FALSE)
      }
      bare_field_inputs <- rbind(bare_field_inputs,bare_field_inputs_temp)
    }
  }
  return(bare_field_inputs)
}
#schema_fixed
get_crop_inputs <- function(landUseSummaryOrPractices){
  #takes a landUseSummaryOrPractices from farms collectionsidue left on site
  endWinterSeason = 5 # month index
  endSummerSeason = 10 # month index
  crop_inputs = data.frame(scenario = c(), parcel_ID = c(), crop = c(), n_fixing_frac = c(), 
                           dry_yield = c(), fresh_yield = c(), dry_residue = c(), fresh_residue = c(), 
                           dry_agb_peak = c(), fresh_agb_peak = c() )
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      # monthly yield and residue
      monthly_harvesting_yield = data.frame(crop=rep('No',12), coverCrop=rep('No',12), productiveFallow=rep('No',12),
                                            harvesting_yield=rep(0,12),residue_left=rep(0,12))
      monthly_harvesting_yield$crop = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$cashCropMonthlyData[[i]]
      monthly_harvesting_yield$coverCrop = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$coverCropMonthlyData[[i]]
      monthly_harvesting_yield$productiveFallow = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$productiveFallow[[i]]
      monthly_harvesting_yield$grazing = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$grazing[[i]]
      monthly_harvesting_yield$harvesting_yield = as.numeric(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$harvestGrazingYield[[i]])
      monthly_harvesting_yield$residue_left = as.numeric(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$estimatedPlantResidue[[i]])
      # fresh or dry tOM/ha
      dryOrFresh = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$yieldsResiduesDryOrFresh[i]
      # case of cash crop with no grazing
      for (crop_chosen in unique(monthly_harvesting_yield$crop)){
        if (crop_chosen!='No'){ #cash crops
          harvesting_yield = sum((monthly_harvesting_yield %>% filter(crop==crop_chosen))$harvesting_yield) #all cash crops are harvested
          residue_left = sum((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$residue_left) #if grazed, counted in pasture inputs
          crop_inputs <- rbind(crop_inputs, 
                               data.frame(scenario = c(paste('year',j,sep="")),
                                          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
                                          crop = crop_chosen,
                                          n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                          dry_yield = c(ifelse(dryOrFresh=="Dry", harvesting_yield,0)), 
                                          fresh_yield = c(ifelse(dryOrFresh=="Fresh", harvesting_yield,0)), 
                                          dry_residue = c(ifelse(dryOrFresh=="Dry", residue_left,0)), 
                                          fresh_residue = c(ifelse(dryOrFresh=="Fresh", residue_left,0)), 
                                          dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$harvesting_yield+
                                                                                           (monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$residue_left),0)), 
                                          fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh",  max((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$harvesting_yield+
                                                                                                (monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$residue_left),0))))
          
          
        }
        
        if (crop_chosen=='No'){# no cash crop
          # with no grazing -> assumed to be generic grass
          harvesting_yield = sum((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$harvesting_yield)
          residue_left = sum((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$residue_left)
          crop_inputs <- rbind(crop_inputs, 
                               data.frame(scenario = c(paste('year',j,sep="")),
                                          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
                                          crop = "Non-N-fixing dry forages",
                                          n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                          dry_yield = c(ifelse(dryOrFresh=="Dry", harvesting_yield,0)), 
                                          fresh_yield = c(ifelse(dryOrFresh=="Fresh", harvesting_yield,0)), 
                                          dry_residue = c(ifelse(dryOrFresh=="Dry", residue_left,0)), 
                                          fresh_residue = c(ifelse(dryOrFresh=="Fresh", residue_left,0)), 
                                          dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$harvesting_yield+
                                                                                           (monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$residue_left),0)), 
                                          fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh", max((monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$harvesting_yield+
                                                                                               (monthly_harvesting_yield %>% filter(crop==crop_chosen & grazing!="Yes"))$residue_left),0))))
          
          
        }
      }
    }
  }
  #extracts harvesting yield and re
  return(crop_inputs)
}
#schema_fixed
get_baseline_crop_inputs <- function(landUseSummaryOrPractices, crop_inputs, my_logger){
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    if(landUseSummaryOrPractices[[1]]$year0$yearCroppingBegan[i]==""){
      log4r::error(my_logger,"Number of years that practices have been applied until now is NOT entered.")
      }
    if (as.numeric(landUseSummaryOrPractices[[1]]$year0$yearCroppingBegan[i])>4){
      # choice that if an arable crop has been run for more than 4 years in a way, this way must be the baseline
      crop_inputs <- rbind(crop_inputs,crop_inputs%>%
                                   filter(parcel_ID==landUseSummaryOrPractices[[1]]$parcelName[i], scenario=='year0')%>%
                                   mutate(scenario='baseline')) # arable crop baseline is based on previous years
    } else {
      if(nrow(crop_inputs %>% filter(crop=='Wheat' | crop=='Winter wheat' | crop=='Spring wheat'))>0){#if we have wheat data from the farmer
        crop_inputs_temp <- crop_inputs %>% filter(crop=='Wheat' | crop=='Winter wheat' | crop=='Spring wheat') %>%
          summarize(parcel_ID=landUseSummaryOrPractices[[1]]$parcelName[i], scenario='baseline',
                    crop = 'Wheat', n_fixing_frac=0,
                    dry_yield=mean(dry_agb_peak)*0.95, fresh_yield = mean(fresh_agb_peak)*0.95,
                    dry_residue=mean(dry_agb_peak)*0.05, fresh_residue=mean(fresh_agb_peak)*0.05, #assumption that only 5% of aboveground biomass is left-on-site
                    dry_agb_peak=mean(dry_agb_peak), fresh_agb_peak=mean(fresh_agb_peak))
        crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
      } else {
        # if no wheat yield data is provided by the farmer
        dry_agb_peak = (crop_data %>% filter(pedo_climatic_area==farm_EnZ))$dry_yield
        crop_inputs_temp <- data.frame(parcel_ID=landUseSummaryOrPractices[[1]]$parcelName[i], scenario='baseline',
                                       crop = 'Wheat', n_fixing_frac=0,
                                       dry_yield=mean(dry_agb_peak)*0.95, fresh_yield = 0,
                                       dry_residue=mean(dry_agb_peak)*0.05, fresh_residue=0, #assumption that only 5% of aboveground biomass is left-on-site
                                       dry_agb_peak=mean(dry_agb_peak), fresh_agb_peak=0)
        crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
      }
    }
  }
  return(crop_inputs)
}
#schema_fixed
get_parcel_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  parcel_inputs = data.frame(parcel_ID = c(), area = c(), longitude = c(),latitude=c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    parcel_inputs <- rbind(parcel_inputs,data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
      area = c(as.numeric(landUseSummaryOrPractices[[1]]$area[i])/10000), 
      longitude = c(as.numeric(extract_longitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i))),
      latitude=c(as.numeric(extract_latitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i)))))
  }
  return(parcel_inputs)
}
#schema_fixed
get_pasture_inputs <- function(landUseSummaryOrPractices, grazing_factors, farm_EnZ, my_logger){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts yield and residues left on site when grazing happening
  pasture_efficiency_potential_difference = unique((grazing_factors %>% filter(pedo_climatic_area==farm_EnZ))$pasture_efficiency_potential_difference)
  endWinterSeason = 5 # month index
  endSummerSeason = 10 # month index
  pasture_inputs = data.frame(scenario = c(), parcel_ID = c(), grass = c(), perennial_frac = c(), n_fixing_frac = c(), 
                              dry_yield = c(), fresh_yield = c(), dry_residual = c(), fresh_residual = c(), 
                              dry_agb_peak = c(), fresh_agb_peak = c(), pasture_efficiency = c())
  if(landUseSummaryOrPractices[[1]]$year0$yearCroppingBegan[i]==""){
    log4r::error(my_logger,"Number of years that practices have been applied until now is NOT entered.")
  }
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    nbYears_initialLandUse_wasApplied = as.numeric(landUseSummaryOrPractices[[1]][['year0']]$yearCroppingBegan[i])
    previous_AMP_years = ifelse(landUseSummaryOrPractices[[1]][['year0']]$adaptiveMultiPaddockGrazing[i]=="Yes", nbYears_initialLandUse_wasApplied, 0) 
    current_AMP_years = 0
    for (j in c(0:10)){
      #counting AMP years to calculate related efficiency
      current_AMP_years = current_AMP_years + ifelse(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$adaptiveMultiPaddockGrazing[i]=="Yes",
                                                     1, ifelse(previous_AMP_years+current_AMP_years>0,-1,0)) # efficiency is assumed to be reversable
      # calculate relating efficiency if it was a permanent grassland
      if(previous_AMP_years+current_AMP_years>=0){ #case where AMP is steadly applied
        pasture_efficiency = 1 + pasture_efficiency_potential_difference *
        (exp(-0.36*previous_AMP_years)-exp(-0.36*(previous_AMP_years+current_AMP_years)))#0.36 factor allows to reach 2/3 of AMP efficiency increase after 3 years of AMP
      } else { #case where AMP is no (longer) applied 
        pasture_efficiency = (1 + pasture_efficiency_potential_difference*(1-exp(-0.36*(previous_AMP_years+current_AMP_years))))/
          (1+pasture_efficiency_potential_difference*(1-exp(-0.36*previous_AMP_years)))
      }
        # selecting the right type of landuse were grazing manangement affects most pasture efficiency 
      if(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$landUseType[i]=='Arable Crops' |
         landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$landUseType[i]=='Arablecrops'){
        pasture_efficiency= mean(1,pasture_efficiency) # assumption that grazing management impacts less arable crops
      }
      # monthly yield and residue
      monthly_grazing_yield = data.frame(grazing_yield=rep(0,12),residue_left=rep(0,12))
      for (k in c(1:12)){
        # grazing without cash crop
        if (landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$grazing[i][[1]][[k]]=="Yes" &
            landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$cashCropMonthlyData[i][[1]][[k]]=="-"){ #cash crop not grazed
          monthly_grazing_yield$grazing_yield[k] = as.numeric(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$harvestGrazingYiel[i][[1]][[k]])
          monthly_grazing_yield$residue_left[k] = as.numeric(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$estimatedPlantResidue[i][[1]][[k]])
        }
        # grazing cash crop residues
        if (landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$grazing[i][[1]][[k]]=="Yes" &
            landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$cashCropMonthlyData[i][[1]][[k]]!="-"){ #cash crop not grazed but residue might be
          monthly_grazing_yield$grazing_yield[k] = as.numeric(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$estimatedPlantResidue[i][[1]][[k]])
        }
      }
      # biomass peak that will be used to calculate root biomass
      indexWinterSeasonPeak = which.max(monthly_grazing_yield$grazing_yield[c(1:endWinterSeason,(endSummerSeason+1):12)])
      indexSummerSeasonPeak = which.max(monthly_grazing_yield$grazing_yield[c((endWinterSeason+1):endSummerSeason)])
      # fresh or dry tOM/ha
      dryOrFresh = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$yieldsResiduesDryOrFresh[i]
      # building df for C inputs calculation
      pasture_inputs <- rbind(pasture_inputs, 
                              data.frame(scenario = c(paste('year',j,sep="")),
                                         parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                         grass = c("Generic grasses"),
                                         perennial_frac = c(ifelse(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$landUseType[i]=='Arable Crops', 
                                                                   0, (previous_AMP_years+current_AMP_years)*0.02)), #CAUTION assumption that perennials increase linearly with AMP by a rate of 2% per year
                                         n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                         dry_yield = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$grazing_yield),0)), 
                                         fresh_yield = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$grazing_yield),0)), 
                                         dry_residual = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$residue_left),0)), 
                                         fresh_residual = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$residue_left),0)), 
                                         dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(1:endWinterSeason,endSummerSeason:12)]) +
                                                                   max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(endWinterSeason:endSummerSeason)]),0)), 
                                         fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh", max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(1:endWinterSeason,endSummerSeason:12)]) +
                                                                     max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(endWinterSeason:endSummerSeason)]),0)), 
                                         pasture_efficiency = c(pasture_efficiency) ))
      # baseline scenario
      if (j==0){ # CAUTION :the value used there should have been monitored, update to year 1 is year 0 isn't accurate enough
        pasture_inputs <- rbind(pasture_inputs, 
                                data.frame(scenario = c("baseline"),
                                           parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                           grass = c("Generic grasses"),
                                           perennial_frac = c(0),
                                           n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                           dry_yield = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$grazing_yield),0)), 
                                           fresh_yield = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$grazing_yield),0)), 
                                           dry_residual = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$residue_left),0)), 
                                           fresh_residual = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$residue_left),0)), 
                                           dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(1:endWinterSeason,endSummerSeason:12)]) +
                                                                     max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(endWinterSeason:endSummerSeason)]),0)), 
                                           fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh", max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(1:endWinterSeason,endSummerSeason:12)]) +
                                                                       max((monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left)[c(endWinterSeason:endSummerSeason)]),0)), 
                                           pasture_efficiency = c(1/(1+pasture_efficiency_potential_difference*(1-exp(-0.36*previous_AMP_years))))))
      }
    }
  }
  return(pasture_inputs)
}
#schema_fixed
get_soil_inputs = function(landUseSummaryOrPractices, soilAnalysis, soilMapsData){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  soil_inputs = data.frame(parcel_ID = c(), scenario = c(), clay = c(), irrigation=c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      soil_inputs <- rbind(soil_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
        scenario = c(paste('year',j,sep="")),
        clay = c(get_clay_content(soilAnalysis, soilMapsData)),
        SOC = c(get_SOC_content(soilAnalysis, soilMapsData)),
        irrigation = c(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])))
      if (j==0){
        soil_inputs <- rbind(soil_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
          scenario = c("baseline"),
          clay = c(get_clay_content(soilAnalysis, soilMapsData)),
          SOC = c(get_SOC_content(soilAnalysis, soilMapsData)),
          irrigation = c(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])))
        }
    }
  }
  return(soil_inputs)
}
#IN_PROGRESS
get_tilling_inputs = function(landUseSummaryOrPractices, tilling_factors, farm_EnZ){ 
  # takes landUseSummaryOrPractices from farms collection, farm_country (from farmInfo) and tilling factors table
  # extracts tilling inputs dataframe 
  tilling_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$tilling_factor
  minimum_tillage_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$minimum_tillage_factor
  tilling_inputs = data.frame(parcel_ID = c(), scenario = c(), tilling_factor = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    tilling_inputs <- rbind(tilling_inputs, data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
      scenario = c('baseline'),
      tilling_factor = c(tilling_factor))) # assumption
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
      for (k in c(1:12)){
        tilling_inputs <- rbind(tilling_inputs, data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = c(paste('year',j,sep="")),
          tilling_factor = c(ifelse(year_chosen$tillingEvent[i][[1]][[k]]=="Yes", tilling_factor, 
                                    ifelse(year_chosen$minimumTillingEvent[i][[1]][[k]]=="Yes", minimum_tillage_factor, 1)))))
      }
    }
  }
  tilling_inputs = tilling_inputs %>% group_by(parcel_ID, scenario) %>%
    summarise(tilling_factor = max(tilling_factor)) # ATM JUST TAKE THE MAX IMPACT EVENT
  return(tilling_inputs)
}
