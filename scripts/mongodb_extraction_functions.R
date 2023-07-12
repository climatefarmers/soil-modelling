#MongoDB parameters extraction functions

### TOOL FUNCTIONS
## Helper function to convert inputs to numeric
new.as_numeric <- function(input){
  for(i in c(1:length(input))){
    if(is.null(input[i])==TRUE){
    input[i]=0
    } else if(is.na(input[i])==TRUE){
      input[i]=0
    } else if(input[i]==""){
      input[i]=0
    }
  }
  return(as.numeric(sub(",", ".", input, fixed = TRUE)))
}

## Helper function to extract latitude
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

## Helper function to extract longitude
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

## Helper function to calculate animal_inputs: extracts total grazing amount
extract_total_grazing_amount <- function(landUseSummaryOrPractices, year = j, area){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts the overall grazing yield and bale grazing yield from the whole farm
  bale_grazing_yield = 0
  grazing_yield = 0
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    if (is.null(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i])==TRUE){
      bale_grazing_yield = bale_grazing_yield + 0
    } else if (is.na(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i])==TRUE){
      bale_grazing_yield = bale_grazing_yield + 0
    } else if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i]==FALSE){
      bale_grazing_yield = bale_grazing_yield + 0
    } else if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i]==TRUE){
      bale_grazing_yield = bale_grazing_yield + new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$hayStrawApplication[i])*
        (1-ifelse(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[i]=="10-15",
                  0.125,
                  new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[i])/100))
    } 
    
    for (k in c(1:12)){
      if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[i][[1]][[k]]!=""){
        grazing_yield = grazing_yield + new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[i][[1]][[k]])
      }
    }
  }
  return((bale_grazing_yield+grazing_yield)*area)
}

## Helper function to calculate animal_inputs: extracts grazing amount per parcel 
extract_grazing_amount_parcel_i <- function(landUseSummaryOrPractices, parcel_index = i, year = j, area){
  #takes a landUseSummaryOrPractices from farms collection and a parcel index i
  #extracts grazing yield and bale grazing yield from parcel index i
  if (is.null(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[parcel_index])==TRUE){
    bale_grazing_yield = 0
  } else if (is.na(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[parcel_index])==TRUE){
    bale_grazing_yield = 0
  } else if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[parcel_index]==FALSE){
    bale_grazing_yield = 0
  } else if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[parcel_index]==TRUE){
    bale_grazing_yield = new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$hayStrawApplication[parcel_index])*
      (1-ifelse(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[parcel_index]=="10-15",
             0.125,
             new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[parcel_index])/100))
  }
  grazing_yield = 0
  for (k in c(1:12)){
    if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[parcel_index][[1]][[k]]!=""){
      grazing_yield = grazing_yield + new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[parcel_index][[1]][[k]])
    }
  }
  return((bale_grazing_yield+grazing_yield)*area)
}

## Helper function to extract total grazing and bale grazing yield from the whole farm over all years
get_total_grazing_table <- function(landUseSummaryOrPractices, livestock, animal_factors, parcel_inputs){
  #takes a landUseSummaryOrPractices from farms collection
  #extracts the overall grazing yield and bale grazing yield from the whole farm
  total_grazing_table = data.frame(scenario = c(), bale_grazing_total = c(), 
                                   grazing_total = c(), grazing_yield_non_arable_lands = c())
  for (year in c(0:10)){
    bale_grazing_yield = 0
    grazing_yield = 0
    grazing_yield_non_arable_lands = 0
    # Bale grazing yield from hay application
    for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
      if (is.null(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i])==TRUE){
        bale_grazing_yield = bale_grazing_yield + 0
      } else if (is.na(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i])==TRUE){
        bale_grazing_yield = bale_grazing_yield + 0
      } else if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i]==FALSE){
        bale_grazing_yield = bale_grazing_yield + 0
      } else if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$baleGrazing[i]==TRUE){
        bale_grazing_yield = bale_grazing_yield + new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$hayStrawApplication[i])*
          (1-ifelse(is.na(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[i])==TRUE | 
                      landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[i]=="10-15",
                    0.125,
                    new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$residueLeftAfterBaleGrazing[i])/100))*
          parcel_inputs$area[i]
      } 
      
      for (k in c(1:12)){
        # grazing yield from monthly grazing yield data
        if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[i][[1]][[k]]!=""){
          grazing_yield = grazing_yield + new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[i][[1]][[k]])*
            parcel_inputs$area[i]
          # grazing yield of non-arable land from monthly grazing yield data
          if (landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$landUseType[i]!="Arablecrops"){
            grazing_yield_non_arable_lands = grazing_yield_non_arable_lands + new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$grazingYield[i][[1]][[k]])*
              parcel_inputs$area[i]
          }
        }
      }
    }
    total_grazing_table = rbind(total_grazing_table,data.frame(
      scenario = c(paste("year" ,year, sep="")), 
      bale_grazing_total = c(bale_grazing_yield), 
      grazing_total = c(grazing_yield),
      grazing_yield_non_arable_lands = c(grazing_yield_non_arable_lands)))
  }
  # Supposed estimated grazing needs :
  animals = data.frame(scenario = c(), species = c(), n_animals = c(), grazing_days = c())
  status ="currentManagement"
  for (k in c(1:nrow(livestock[[status]][[1]]))){
    if (is.na(livestock[[status]][[1]]$species[[k]])==TRUE){next}
    animals = rbind(animals, data.frame(
      scenario = c("year0"),
      species = c(livestock[[status]][[1]]$species[[k]]),
      n_animals = c(new.as_numeric(livestock[[status]][[1]]$numberOfHeads[[k]])), 
      grazing_days = c(new.as_numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]]))
    ))
  }
  status = "futureManagement"
  for (year in c(1:10)){
    for (k in c(1:nrow(livestock[[status]][[1]][[paste('year',year,sep="")]]))){
      if (is.na(livestock[[status]][[1]][[paste('year',year,sep="")]]$species[[k]])==TRUE){next}
      animals <- rbind(animals,data.frame(
        scenario = paste('year',year,sep=""), 
        species = c(livestock[[status]][[1]][[paste('year',year,sep="")]]$species[[k]]),
        n_animals = c(new.as_numeric(livestock[[status]][[1]][[paste('year',year,sep="")]]$numberOfHeads[[k]])), 
        grazing_days = c(new.as_numeric(livestock[[status]][[1]][[paste('year',year,sep="")]]$grazingOrPasturedDaysPerYear[[k]]))
      ))
    }
  }
  animals = merge(x = animals, y = animal_factors, by = "species", all.x = TRUE)
  animal_needs_table = animals %>%
    mutate(yearly_grazing_needs_tDM = n_animals*mass_kg_per_animal*grazing_days*0.025/1000)
  total_grazing_needs_table = animal_needs_table  %>%
    group_by(scenario) %>%
    summarise(expected_grazing_needs_tDM = sum(yearly_grazing_needs_tDM))
  total_grazing_table = merge(x = total_grazing_table, y = total_grazing_needs_table, by = "scenario", all.x = TRUE) 
  total_grazing_table = total_grazing_table %>%
    mutate(relative_difference_perc = paste(round((grazing_total-expected_grazing_needs_tDM)/expected_grazing_needs_tDM*100),"%"),
           expected_grazing_needs_tDM_pastures=expected_grazing_needs_tDM*grazing_yield_non_arable_lands/grazing_total,
           pasture_yield_weighted_bale_grazing=bale_grazing_total*grazing_yield_non_arable_lands/grazing_total # distributing bale grazing as grazing yields are distributed. May be improved
    )
  return(total_grazing_table)
}

# Helper function that extracts crop type per month per parcel:
# In case of crop rotation there can be two different cash crops within one year (cash crop 1 & cash crop 2)
get_monthly_cash_crop <- function(parcel_index = i, year_chosen){
  crop=rep(NA,12)
  for (k in c(1:12)){
    if(is.na(year_chosen$cashCrop1MonthlyData[[parcel_index]][k])==FALSE &
       year_chosen$cashCrop1MonthlyData[[parcel_index]][k] != "-"){ # cash crop 1 checked for month k
      if(is.na(year_chosen$cashCrop2MonthlyData[[parcel_index]][k])==FALSE &
         year_chosen$cashCrop2MonthlyData[[parcel_index]][k] != "-"){ # case of conflict, we assume that the correct is cash crop 1
        log4r::error(my_logger, paste('Two different cash crops checked for month ',k,' in parcel ',landUseSummaryOrPractices[[1]]$parcelName[i],'.',sep='')) # flag
      } else {
        crop[k] = year_chosen$cashCrop1MonthlyData[[parcel_index]][k]
      }
    } else if (is.na(year_chosen$cashCrop2MonthlyData[[parcel_index]][k])==FALSE &
               year_chosen$cashCrop2MonthlyData[[parcel_index]][k] != "-"){
      crop[k] = year_chosen$cashCrop2MonthlyData[[parcel_index]][k]
    } else {
      crop[k] = NA # crop[k] stays NA
    }
  }
  return(crop)
}

# UNDER CONSTRUCTION:
detect_crop_rotations <- function(landUseSummaryOrPractices, parcel_index = i){
  ### Get landUse data for a parcel i
  ### Return a year where rotations starts 
  ### and a number of years of a cycle. 0 means no rotations? 1 means no change in management
  # Listing arablecrop years
  list_arablecrop_years = c()
  for (year in c(0:10)){
    if(landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]$landUseType[i]=="Arablecrops"){
      list_arablecrop_years = c(list_arablecrop_years,year)
    }
  }
  # If occurence of arable crops, looking for crop rotations
  if (length(list_arablecrop_years) > 0){
    df_crops = data.frame(year = c(), crops = c())
    for (year in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',year,sep="")]]
      df_crops = rbind(df_crops, data.frame(
        year = year,
        crops = unique(na.omit(get_monthly_cash_crop(i, year_chosen)))
        ))
    }
  }
}
  
## Helper function to get clay content in %
# if soil samples available: farmer's input (%)
# else: soil maps
get_clay_content <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$clayContentPercent)==TRUE){
    return(soilMapsData$clay)
  } else if (soilAnalysis$clayContentPercent==""){
    return(soilMapsData$clay)
  } else { # variable found and a value is provided
    if(5<new.as_numeric(soilAnalysis$clayContentPercent) & new.as_numeric(soilAnalysis$clayContentPercent)<80){ # assumed to be %
      return(new.as_numeric(soilAnalysis$clayContentPercent))
    } else {
      log4r::error(my_logger, paste("Clay content input = ", 
                                    new.as_numeric(soilAnalysis$clayContentPercent),
                                    "%. Check unit/values with farmer.", sep=""))
    }
  }
}

## Helper function to get silt content in %
# if soil samples available: farmer's input (%)
# else: soil maps
get_silt_content <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$siltContentPercent)==TRUE){
    return(soilMapsData$silt)
  } else if (soilAnalysis$siltContentPercent==""){
    return(soilMapsData$silt)
  } else { # variable found and a value is provided
    if(5<new.as_numeric(soilAnalysis$siltContentPercent) & new.as_numeric(soilAnalysis$siltContentPercent)<80){ # assumed to be %
      return(new.as_numeric(soilAnalysis$siltContentPercent))
    } else {
      log4r::error(my_logger, paste("silt content input = ", 
                                    new.as_numeric(soilAnalysis$siltContentPercent),
                                    "%. Check unit/values with farmer.", sep=""))
    }
  }
}

## Helper function to get carbon content in kg/ha?
get_SOC_content <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$clayContentPercent)==TRUE & is.null(soilAnalysis$organicMatterContent)==TRUE){ #case that SOC & SOM variables weren't found
    return(soilMapsData$SOC)
  }
  if (is.null(soilAnalysis$carbonContent)==TRUE & is.null(soilAnalysis$organicMatterContent)==FALSE){ 
    if (soilAnalysis$organicMatterContent==""){ # case that SOC variable wasn't found and SOM wasn't known
      return(soilMapsData$SOC)
    }
    if(8<new.as_numeric(soilAnalysis$organicMatterContent) & new.as_numeric(soilAnalysis$organicMatterContent)<80 & soilAnalysis$organicMatterContentMetric!="%"){ # SOC in t/ha = g/kg
      return(new.as_numeric(soilAnalysis$organicMatterContent)*0.55)
    } 
    if (0.7<new.as_numeric(soilAnalysis$organicMatterContent) & new.as_numeric(soilAnalysis$organicMatterContent)<8){ #SOC in %
      return(new.as_numeric(soilAnalysis$organicMatterContent)*5.5)
    } else {
      log4r::error(my_logger, paste("OM content input = ", new.as_numeric(soilAnalysis$organicMatterContent),
                                    soilAnalysis$organicMatterContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
  if (soilAnalysis$carbonContent!=""){ # SOC variable exists and a value was entered
    if(4<new.as_numeric(soilAnalysis$carbonContent) & new.as_numeric(soilAnalysis$carbonContent)<40){ # SOC in t/ha = g/kg
      return(new.as_numeric(soilAnalysis$carbonContent))
    } 
    if (0.35<new.as_numeric(soilAnalysis$carbonContent) & new.as_numeric(soilAnalysis$carbonContent)<4){ #SOC in %
      return(new.as_numeric(soilAnalysis$carbonContent)*10)
    } else {
      log4r::error(my_logger, paste("SOC content input = ", new.as_numeric(soilAnalysis$carbonContent),
                                    soilAnalysis$carbonContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  } else { # SOC variable exists and no value was entered
    return(soilMapsData$SOC)
  }
}


## Helper function to get bulk density
get_bulk_density <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$bulkDensity)==TRUE){
    return(soilMapsData$bulk_density)
  } else if (soilAnalysis$bulkDensity==""){
    return(soilMapsData$bulk_density)
  } else { # variable found and a value is provided
    if(0.7<new.as_numeric(soilAnalysis$bulkDensity) & new.as_numeric(soilAnalysis$bulkDensity)<2){
      return(new.as_numeric(soilAnalysis$bulkDensity))
    } else if (700<new.as_numeric(soilAnalysis$bulkDensity) & new.as_numeric(soilAnalysis$bulkDensity)<2000){
      return(new.as_numeric(soilAnalysis$bulkDensity)*1e-3)
    } else {
      log4r::error(my_logger, paste("bulk density input = ", 
                                    new.as_numeric(soilAnalysis$bulkDensity),
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
}

### GET INPUT FUNCTIONS
get_add_manure_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts manure application inputs dataframe 
  add_manure_inputs = data.frame(parcel_ID = c(), scenario = c(), manure_source = c(), 
                                 quantity_t_ha = c(), imported_frac = c(), remaining_frac = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]
      if(is.na(year_chosen$manureApplication[i])==FALSE){
        # Manure (animal dung)
        if (year_chosen$manureApplication[i]>=0){
          add_manure_inputs <- rbind(add_manure_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(paste('year', j, sep="")), 
            manure_source = c("Other Cattle"), # AN UNFOLDING LIST OF MANURE TYPE MIGHT HAVE TO BE ADDED TO UI
            quantity_t_ha = c(new.as_numeric(year_chosen$manureApplication[i])), 
            imported_frac = c(ifelse(is.null(year_chosen$percentManureImported[i])==TRUE,0,
                                     ifelse(is.na(year_chosen$percentManureImported[i])==TRUE,0,
                                            new.as_numeric(year_chosen$percentManureImported[i])/100))),
            remaining_frac = c(1)))
        }
      }
      # Compost
      if(is.na(year_chosen$compostApplication[i])==FALSE){
        if (year_chosen$compostApplication[i]>=0){
          add_manure_inputs <- rbind(add_manure_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(paste('year',j,sep="")), 
            manure_source = c("Green compost"), # CAUTION the fact that compost entry is GREEN compost might have to be specified
            quantity_t_ha = c(new.as_numeric(year_chosen$compostApplication[i])), 
            imported_frac = c(ifelse(is.null(year_chosen$percentCompostImported[i])==TRUE,0,
                                     ifelse(is.na(year_chosen$percentCompostImported[i])==TRUE,0,
                                            new.as_numeric(year_chosen$percentCompostImported[i])/100))),
            remaining_frac = c(1)))
        }
      }
      # Hay
      if(is.na(year_chosen$hayStrawApplication[i])==FALSE){
        if (year_chosen$hayStrawApplication[i]>=0){
          add_manure_inputs <- rbind(add_manure_inputs, data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(paste('year',j,sep="")), 
            manure_source = c("Hay"),
            quantity_t_ha = c(new.as_numeric(year_chosen$hayStrawApplication[i])), 
            imported_frac = c(ifelse(is.null(year_chosen$percentageOfHayStrawImported[i])==TRUE,0,
                                     ifelse(is.na(year_chosen$percentageOfHayStrawImported[i])==TRUE,0,
                                            new.as_numeric(year_chosen$percentageOfHayStrawImported[i])/100))),
            remaining_frac = c(ifelse(is.null(year_chosen$baleGrazing[i])==TRUE, 1, # case were variable isn't found
                                      ifelse(is.na(year_chosen$baleGrazing[i])==TRUE, 1, # case were variable had no value
                                             ifelse(year_chosen$baleGrazing[i]==TRUE, # case were baleGrazing happens
                                                    ifelse(landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]$residueLeftAfterBaleGrazing[i]=="10-15", 12.5, # single case hand fix
                                                           new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]$residueLeftAfterBaleGrazing[i]))/100,
                                                    1)))))) #case were no grazing happens meaning it is 100% amended to the soil
        }
      }
    }
  }
  add_manure_inputs <- rbind(add_manure_inputs, add_manure_inputs%>%
                               filter(scenario=='year0')%>%
                               mutate(scenario='baseline')) # Manure addition baseline is based on previous years
  return(add_manure_inputs)
}

get_agroforestry_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts agroforestry inputs dataframe 
  agroforestry_inputs = data.frame(parcel_ID = c(), scenario = c(), tree_species = c(), other_name = c(), dbh = c(),
                                   tree_density = c(), area = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      row_index = 0
      c = c()
      for (tree in landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$typeOfTrees[i][[1]]$treeName){
        row_index = row_index + 1
        if (is.na(tree)==FALSE){
          if (tree!=""){ #filter out if no tree information given
            c = append(c, row_index)
          }
        }
      }
      typeOfTrees = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$typeOfTrees[i][[1]][c,]
      if(nrow(typeOfTrees)>0){
        for (k in c(1:nrow(typeOfTrees))){
          agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(paste('year',j,sep="")), 
            tree_species = c(typeOfTrees$treeName[[k]]),
            other_name = c(typeOfTrees$otherTreeName[[k]]),
            dbh = c(new.as_numeric(typeOfTrees$treeAvgDBH[[k]])), 
            tree_density = c(new.as_numeric(typeOfTrees$avgNoOfTrees[[k]])), 
            area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000)))
        }
        if (j==0){ #baseline based on pre-project trees
          for (k in c(1:nrow(typeOfTrees))){
            agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
              parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
              scenario = c("baseline"), 
              tree_species = c(typeOfTrees$treeName[[k]]),
              other_name = c(typeOfTrees$otherTreeName[[k]]),
              dbh = c(new.as_numeric(typeOfTrees$treeAvgDBH[[k]])), 
              tree_density = c(new.as_numeric(typeOfTrees$avgNoOfTrees[[k]])), 
              area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000)))
          }
        }
      }
    }
  }
  NA_rows = nrow(agroforestry_inputs)-nrow(na.omit(agroforestry_inputs))
  if(NA_rows>0){
    log4r::error(my_logger, paste('CAUTION: ',NA_rows,' rows contained NAs in agroforestry_inputs.', paste=''))
  }
  return(na.omit(agroforestry_inputs))
}

get_animal_inputs = function(landUseSummaryOrPractices,livestock, parcel_inputs){
  # takes landUseSummaryOrPractices & livestock from farms collection
  # extracts animal inputs dataframe 
  animal_inputs = data.frame(parcel_ID = c(), scenario = c(), species = c(), n_animals = c(), grazing_days = c(),
                             area = c(), grazing_management = c(), productivity = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    status ="currentManagement"
    for (k in c(1:nrow(livestock[[status]][[1]]))){
      if (is.na(livestock[[status]][[1]]$species[[k]])==TRUE){next}
      animal_inputs <- rbind(animal_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        scenario = c(paste('year',0,sep="")), 
        species = c(livestock[[status]][[1]]$species[[k]]),
        # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
        n_animals = c(ifelse(extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i])==0,0,
                             new.as_numeric(livestock[[status]][[1]]$numberOfHeads[[k]])*
                               extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,0,parcel_inputs$area[i])/
                               extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i]))), 
        grazing_days = c(new.as_numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]])), 
        area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000), #CAUTION, SHOULD TAKE FROM PARCEL INPUT DIRECTLY
        grazing_management = c("Daily Spread"), 
        productivity = c("Low Productivity"))) # CAUTION, NEEDED FOR LCA SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
    }
    # baseline based on year 0 livestock
    for (k in c(1:nrow(livestock[[status]][[1]]))){
      if (is.na(livestock[[status]][[1]]$species[[k]])==TRUE){next}
      animal_inputs <- rbind(animal_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        scenario = c("baseline"), 
        species = c(livestock[[status]][[1]]$species[[k]]),
        # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
        n_animals = c(ifelse(extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i])==0,0,
                             new.as_numeric(livestock[[status]][[1]]$numberOfHeads[[k]])*
                               extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,0,parcel_inputs$area[i])/
                               extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i]))), 
        grazing_days = c(new.as_numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]])), 
        area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
        grazing_management = c("Daily Spread"), 
        productivity = c("Low Productivity"))) # CAUTION, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
    }
    status = "futureManagement"
    for (year in c(1:10)){
      scenario = c(paste('year',year,sep=""))
      for (k in c(1:nrow(livestock[[status]][[1]][[scenario]]))){
        if (is.na(livestock[[status]][[1]][[scenario]]$species[[k]])==TRUE){next}
        animal_inputs <- rbind(animal_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = scenario, 
          species = c(livestock[[status]][[1]][[scenario]]$species[[k]]),
          # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
          n_animals = c(ifelse(extract_total_grazing_amount(landUseSummaryOrPractices,year,parcel_inputs$area[i])==0,0,
                               new.as_numeric(livestock[[status]][[1]][[scenario]]$numberOfHeads[[k]])*
                                 extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,year,parcel_inputs$area[i])/
                                 extract_total_grazing_amount(landUseSummaryOrPractices,year,parcel_inputs$area[i]))), 
          grazing_days = c(new.as_numeric(livestock[[status]][[1]][[scenario]]$grazingOrPasturedDaysPerYear[[k]])), 
          area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
          grazing_management = c("Daily Spread"), 
          productivity = c("Low Productivity"))) # CAUTION, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
      }
    }
  }
  return(animal_inputs)
}

get_bare_field_inputs = function(landUseSummaryOrPractices, soil_cover_data, farm_EnZ, pars){
  # takes landUseSummaryOrPractices from farms collection
  # extracts bare soil inputs dataframe 
  bare_field_inputs = data.frame(parcel_ID = c(), scenario = c())
  # one column per month
  for (k in c(1:12)){
    bare_field_inputs[[paste("bare_profile_", k, sep="")]] = c()
  }
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    bare_field_inputs_temp <- data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                         scenario = c('baseline'))
    bare_field_inputs <- rbind(bare_field_inputs, cbind(bare_field_inputs_temp, soil_cover_data %>% filter(pedo_climatic_area == farm_EnZ) %>%
                                                         select(-country,-pedo_climatic_area)))
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]
      bare_field_inputs_temp = data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                          scenario = c(paste('year', j, sep="")))
      for (k in c(1:12)){
        bare_field_inputs_temp[[paste("bare_profile_", k, sep="")]] = ifelse(
          year_chosen$bareSoilFallow[[1]][[k]]==TRUE, TRUE, FALSE)
      }
      bare_field_inputs <- rbind(bare_field_inputs, bare_field_inputs_temp)
    }
  }
  return(bare_field_inputs)
}

get_crop_inputs <- function(landUseSummaryOrPractices, pars){
  crop_inputs = data.frame(scenario = c(), parcel_ID = c(), crop = c(), dry_yield = c(), 
                           fresh_yield = c(), dry_grazing_yield = c(), fresh_grazing_yield = c(),
                           dry_residue = c(), fresh_residue = c(), 
                           dry_agb_peak = c(), fresh_agb_peak = c() )
  for (j in c(0:10)){ #years
    year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
    for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
      # we exclude holistic grazing compatible land-uses (no pasture efficiency coef will be used in crop inputs)
      if (year_chosen$landUseType[i]=="Arablecrops"){
        # creating the data frame storing monthly yield and residue
        monthly_harvesting_yield = data.frame(crop=logical(12), 
                                              coverCrop=logical(12), 
                                              productiveFallow=logical(12),
                                              harvesting_yield=logical(12),
                                              residue_left=logical(12))
        # getting actual data
        monthly_harvesting_yield$crop = get_monthly_cash_crop(parcel_index = i, year_chosen)
        monthly_harvesting_yield$coverCrop = year_chosen$coverCropMonthlyData[[i]]
        monthly_harvesting_yield$productiveFallow = year_chosen$productiveFallow[[i]]
        monthly_harvesting_yield$grazing_yield = new.as_numeric(year_chosen$grazingYield[[i]])
        monthly_harvesting_yield$harvesting_yield = new.as_numeric(year_chosen$harvestYield[[i]])
        monthly_harvesting_yield$residue_left = new.as_numeric(year_chosen$estimationAfterResidueGrazingHarvest[[i]])
        # if willing to correct total grazing yield by using a CF-made estimation, we re-weight the grazing yields
        if (pars$CFmade_grazing_estimations_Yes_No == "Yes"){
          grazing_table_temp = total_grazing_table %>% filter(scenario==paste('year',j,sep=""))
          if (grazing_table_temp$bale_grazing_total>grazing_table_temp$expected_grazing_needs_tDM){
            log4r::error(my_logger,"CAUTION ! Bale grazing alone overcomes expected grazing needs, to be checked.")
          } else if (grazing_table_temp$grazing_total==0){
            # grazing arbitrarily equally distributed over grazed land, 2 month a year (6 months apart) if no grazing yield announced
            half_yearly_grazing_yield_per_ha = 1/2*(grazing_table_temp$expected_grazing_needs_tDM-grazing_table_temp$bale_grazing_total*0.85)/sum(parcel_inputs$area) 
            monthly_harvesting_yield$grazing_yield = c(half_yearly_grazing_yield_per_ha,rep(0,5),half_yearly_grazing_yield_per_ha,rep(0,5))
          } else {
            # grazing arbitrarily equally distributed over time weighted by parcel grazing yield relatively to farm level, if known
            half_yearly_grazing_yield_per_ha = 1/2*sum(monthly_harvesting_yield$grazing_yield)/
              (grazing_table_temp$grazing_total-grazing_table_temp$grazing_yield_non_arable_lands)* #grazing yield arable lands
              ((grazing_table_temp$expected_grazing_needs_tDM-grazing_table_temp$expected_grazing_needs_tDM_pastures)-(grazing_table_temp$bale_grazing_total-grazing_table_temp$pasture_yield_weighted_bale_grazing)*0.85) # expected grazing yield arable lands after deduction of bale grazing distributed in arable lands 
            monthly_harvesting_yield$grazing_yield = c(half_yearly_grazing_yield_per_ha,rep(0,5),half_yearly_grazing_yield_per_ha,rep(0,5))
          }
        }
        # fresh or dry tOM/ha
        if (is.na(year_chosen$yieldsResiduesDryOrFresh[i])==TRUE){
          dryOrFresh = "Dry"
          log4r::info(my_logger, paste("CAUTION: dryOrFresh is NA in parcel ",landUseSummaryOrPractices[[1]]$parcelName[i],
                                        " for year ",j,". Was ASSUMED to be dry.", sep=""))
        } else {
          dryOrFresh = year_chosen$yieldsResiduesDryOrFresh[i]
        }
        # case of cash crop with no grazing
        for (crop_chosen in unique(monthly_harvesting_yield$crop)){
          if(is.na(crop_chosen)==FALSE){
            harvesting_yield = sum((monthly_harvesting_yield %>% filter(crop==crop_chosen))$harvesting_yield)
            grazing_yield = sum(new.as_numeric((monthly_harvesting_yield %>% filter(crop==crop_chosen))$grazing_yield))
            residue_left = sum((monthly_harvesting_yield %>% filter(crop==crop_chosen))$residue_left)
            crop_inputs <- rbind(crop_inputs, 
                                 data.frame(scenario = c(paste('year',j,sep="")),
                                            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
                                            crop = crop_chosen,
                                            dry_yield = c(ifelse(dryOrFresh=="Dry", harvesting_yield,0)), 
                                            fresh_yield = c(ifelse(dryOrFresh=="Fresh", harvesting_yield,0)), 
                                            dry_grazing_yield = c(ifelse(dryOrFresh=="Dry", grazing_yield,0)), 
                                            fresh_grazing_yield = c(ifelse(dryOrFresh=="Fresh", grazing_yield,0)), 
                                            dry_residue = c(ifelse(dryOrFresh=="Dry", residue_left+grazing_yield*0.15,0)), 
                                            fresh_residue = c(ifelse(dryOrFresh=="Fresh", residue_left+grazing_yield*0.15,0)), 
                                            dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max((monthly_harvesting_yield %>% filter(crop==crop_chosen))$harvesting_yield+
                                                                                             new.as_numeric((monthly_harvesting_yield %>% filter(crop==crop_chosen))$grazing_yield)+
                                                                                             (monthly_harvesting_yield %>% filter(crop==crop_chosen))$residue_left),0)), 
                                            fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh",  max((monthly_harvesting_yield %>% filter(crop==crop_chosen))$harvesting_yield+
                                                                                                  new.as_numeric((monthly_harvesting_yield %>% filter(crop==crop_chosen))$grazing_yield)+
                                                                                                  (monthly_harvesting_yield %>% filter(crop==crop_chosen))$residue_left),0))))
          } else { # crop_chosen = NA, meaning no cash crop
            harvesting_yield = sum((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$harvesting_yield)
            grazing_yield = sum(new.as_numeric((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$grazing_yield))
            residue_left = sum((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$residue_left)
            crop_inputs <- rbind(crop_inputs, 
                                 data.frame(scenario = c(paste('year',j,sep="")),
                                            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
                                            # assumed to be "generic grass" if no cash crop
                                            crop = "Non-N-fixing dry forages",# SHOULD WE DIFFERENTIATE PRODUCTIVE FALLOW AND COVER CROPS ?
                                            dry_yield = c(ifelse(dryOrFresh=="Dry", harvesting_yield,0)), 
                                            fresh_yield = c(ifelse(dryOrFresh=="Fresh", harvesting_yield,0)), 
                                            dry_grazing_yield = c(ifelse(dryOrFresh=="Dry", grazing_yield,0)), 
                                            fresh_grazing_yield = c(ifelse(dryOrFresh=="Fresh", grazing_yield,0)), 
                                            dry_residue = c(ifelse(dryOrFresh=="Dry", residue_left+grazing_yield*0.15,0)), 
                                            fresh_residue = c(ifelse(dryOrFresh=="Fresh", residue_left+grazing_yield*0.15,0)), 
                                            dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$harvesting_yield+
                                                                                             new.as_numeric((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$grazing_yield)+
                                                                                             (monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$residue_left),0)), 
                                            fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh",  max((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$harvesting_yield+
                                                                                                  new.as_numeric((monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$grazing_yield)+
                                                                                                  (monthly_harvesting_yield %>% filter(is.na(crop)==TRUE))$residue_left),0))))
          }
        }
      }
    }
  }
  return(crop_inputs)
}

get_baseline_crop_inputs <- function(landUseSummaryOrPractices, crop_inputs, crop_data, my_logger, farm_EnZ){
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    if (nrow(crop_inputs)==0){ # no crops previously found
      return(crop_inputs) # so no crop baselines to be created, returned empty
    }
    #ONLY ARABLECROPS LANDUSE TYPE IS CURRENTLY CONSIDERED IN get_crop_inputs
    #SHOULD BE REFINED IN CASE OF CASH CROPS UNDER AGROFORESTRY
    # if(landUseSummaryOrPractices[[1]][['year0']]$landUseType[i]=="Agroforestry" |
    #    landUseSummaryOrPractices[[1]][['year0']]$landUseType[i]=="Forestry" ){
    #   # We assume that for the above land uses soil cover management baseline is the current state
    #   crop_inputs <- rbind(crop_inputs,crop_inputs%>%
    #                          filter(parcel_ID==landUseSummaryOrPractices[[1]]$parcelName[i], scenario=='year0')%>%
    #                          mutate(scenario='baseline')) # arable crop baseline is based on previous years
    # }
    if(landUseSummaryOrPractices[[1]][['year0']]$landUseType[i]=="Arablecrops"){
      # AT THE MOMENT PERMANENT COVER CROPS ARE ALSO ASSOCIATED TO CEREAL-BASELINE
      if(landUseSummaryOrPractices[[1]]$year0$applyingThesePracticesInYears[i]==""){
        log4r::error(my_logger,"Number of years that practices have been applied until now is NOT entered.")
      } else if (new.as_numeric(landUseSummaryOrPractices[[1]]$year0$applyingThesePracticesInYears[i])>3){
        # choice that if an arable crop has been run for more than 3 years in a way, this way must be the baseline
        crop_inputs <- rbind(crop_inputs,crop_inputs%>%
                               filter(parcel_ID==landUseSummaryOrPractices[[1]]$parcelName[i], scenario=='year0')%>%
                               mutate(scenario='baseline')) # arable crop baseline is based on previous years
      } else {
        if(nrow(crop_inputs %>% filter(crop=='Wheat' | crop=='Winter wheat' | crop=='Spring wheat'))>0){#if we have wheat data from the farmer
          crop_inputs_temp <- crop_inputs %>% filter(crop=='Wheat' | crop=='Winter wheat' | crop=='Spring wheat') %>%
            summarize(parcel_ID=landUseSummaryOrPractices[[1]]$parcelName[i], scenario='baseline',
                      crop = 'Wheat', 
                      dry_yield=mean(dry_agb_peak)*0.95, fresh_yield = mean(fresh_agb_peak)*0.95,
                      dry_grazing_yield=0, fresh_grazing_yield=0,
                      dry_residue=mean(dry_agb_peak)*0.05, fresh_residue=mean(fresh_agb_peak)*0.05, #assumption that only 5% of aboveground biomass  is left-on-site
                      dry_agb_peak=mean(dry_agb_peak), fresh_agb_peak=mean(fresh_agb_peak))
          crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
        } else {
          # if no wheat yield data is provided by the farmer
          dry_agb_peak = (crop_data %>% filter(pedo_climatic_area==farm_EnZ))$ag_dm_peak
          crop_inputs_temp <- data.frame(parcel_ID=landUseSummaryOrPractices[[1]]$parcelName[i], scenario='baseline',
                                         crop = 'Wheat', 
                                         dry_yield=mean(dry_agb_peak)*0.95, fresh_yield = 0,
                                         dry_grazing_yield=0, fresh_grazing_yield=0,
                                         dry_residue=mean(dry_agb_peak)*0.05, fresh_residue=0, #assumption that only 5% of aboveground biomass is left-on-site
                                         dry_agb_peak=mean(dry_agb_peak), fresh_agb_peak=0)
          crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
        }
      }
    }
  }
  return(crop_inputs)
}

get_fertilizer_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts fertilizer inputs dataframe 
  fertilizer_inputs = data.frame(parcel_ID = c(), field_area = c(), scenario = c(), usage_boolean=c(), 
                                 fertilizer_type=c(), quantity_t_ha=c(), n_content_perc=c())
  list_missing_data = c()
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
      fertilizer_inputs <- rbind(fertilizer_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        field_area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i])==TRUE,
                            c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                            ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i])==TRUE |
                                     landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                                   c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                                   c(new.as_numeric(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
        scenario = c(paste('year',j,sep="")),
        usage_boolean = year_chosen$syntheticFertilizer$usage[i],
        fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
        quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
        n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)))
      if (j==0){
        fertilizer_inputs <- rbind(fertilizer_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          field_area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i])==TRUE,
                              c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                              ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i])==TRUE |
                                       landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                                     c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                                     c(new.as_numeric(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
          scenario = c("baseline"),
          usage_boolean = year_chosen$syntheticFertilizer$usage[i],
          fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
          quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
          n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)))
      }
      last_index = nrow(fertilizer_inputs)
      if (fertilizer_inputs$usage_boolean[last_index]==TRUE){
        if (fertilizer_inputs$quantity_t_ha[last_index]==0){
          list_missing_data = c(list_missing_data,paste(fertilizer_inputs$parcel_ID[last_index],
                                                        ' (year',j,"): quantity_t_ha missing", sep=""))
        }
        if (fertilizer_inputs$n_content_perc[last_index]==0){
          list_missing_data = c(list_missing_data,paste(fertilizer_inputs$parcel_ID[last_index],
                                                        ' (year',j,"): n_content_perc missing", sep=""))
        }
      }
    }
  }
  if (length(list_missing_data)>0){
    log4r::error(my_logger, paste('CAUTION: Fertilizer data: ',list(list_missing_data),'.', paste=''))
  }
  return(fertilizer_inputs)
}

get_fuel_inputs = function(fuel){
  # extracts fuel inputs dataframe 
  fuel_inputs = data.frame(scenario = c(), typeOfFuel = c(), amountInLiters = c())
  status ="currentFuelUsage"
  for (k in c(1:nrow(fuel[[status]][[1]]))){
    if (is.na(fuel[[status]][[1]]$typeOfFuel[[k]])==TRUE | fuel[[status]][[1]]$typeOfFuel[[k]]==""){next}
    fuel_inputs <- rbind(fuel_inputs,data.frame(
      scenario = c(paste('year',0,sep="")), 
      fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
      value_l = new.as_numeric(fuel[[status]][[1]]$amountInLiters[[k]])))
    fuel_inputs <- rbind(fuel_inputs,data.frame(
      scenario = c("baseline"), 
      fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
      value_l = new.as_numeric(fuel[[status]][[1]]$amountInLiters[[k]])))
  }
  status = "futureFuelUsage"
  for (year in c(1:10)){
    for (k in c(1:nrow(fuel[[status]][[1]]))){
      if (is.na(fuel[[status]][[1]]$typeOfFuel[[k]])==TRUE | fuel[[status]][[1]]$typeOfFuel[[k]]==""){next}
      fuel_inputs <- rbind(fuel_inputs,data.frame(
        scenario = c(paste('year',year,sep="")), 
        fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
        value_l = new.as_numeric(fuel[[status]][[1]]$amountInLiters[[k]])))
    }
  }
  return(fuel_inputs)
}

## Helper function to extract land use type (not used!)
get_land_use_type <- function(landUseSummaryOrPractices, parcel_inputs){
  landUseType = data.frame(parcel_ID = c(), area = c(), uniqueLandUseType_Yes_No = c(), landUseType = c())
  temp_df=data.frame(landUseType_year0 = rep("-",nrow(parcel_inputs)))
  for (j in c(0:10)){
    temp_df[[paste("landUseType_year",j,sep="")]]=landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$landUseType
  }
  temp_df = data.frame(t(temp_df))
  colnames(temp_df) = landUseSummaryOrPractices[[1]]$parcelName
  for (i in c(1:nrow(parcel_inputs))){
    landUseType = rbind(landUseType, data.frame(
      parcel_ID = parcel_inputs$parcel_ID[i], 
      area = parcel_inputs$area[i],
      uniqueLandUseType_Yes_No = ifelse(nrow(unique(temp_df[i]))==1, TRUE, FALSE),
      landUseType = ifelse(nrow(unique(temp_df[i]))==1, as.character(unique(temp_df[i])),
                           as.character(temp_df[i]))
    ))
  }
  return(landUseType)
}

get_parcel_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 

  parcel_inputs = data.frame(parcel_ID = c(), area = c(), longitude = c(),latitude=c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    parcel_inputs <- rbind(parcel_inputs,data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
      area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i])==TRUE,
                    c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                    ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i])==TRUE |
                             landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                           c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                           c(new.as_numeric(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
      longitude = c(new.as_numeric(extract_longitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i))),
      latitude=c(new.as_numeric(extract_latitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i)))))
    
  }
    return(parcel_inputs)
}

get_pasture_inputs <- function(landUseSummaryOrPractices, grazing_factors, farm_EnZ, total_grazing_table, my_logger, pars){
 #takes a landUseSummaryOrPractices from farms collection
  #extracts yield and residues left on site when grazing happened
  pasture_efficiency_potential_difference = unique((grazing_factors %>% filter(pedo_climatic_area==farm_EnZ))$pasture_efficiency_potential_difference)
  pasture_inputs = data.frame(scenario = c(), parcel_ID = c(), grass = c(), perennial_frac = c(), n_fixing_frac = c(), 
                              dry_yield = c(), fresh_yield = c(), dry_residual = c(), fresh_residual = c(), 
                              dry_agb_peak = c(), fresh_agb_peak = c(), pasture_efficiency = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    if(landUseSummaryOrPractices[[1]]$year0$applyingThesePracticesInYears[i]==""){
      log4r::error(my_logger,"Number of years that practices have been applied until now is NOT entered.")
    } else {
      nbYears_initialLandUse_wasApplied = new.as_numeric(landUseSummaryOrPractices[[1]][['year0']]$applyingThesePracticesInYears[i])
      }
    previous_AMP_years = ifelse(is.null(landUseSummaryOrPractices[[1]][['year0']]$adaptiveMultiPaddockGrazing[i])==TRUE, 0,
                                ifelse(is.na(landUseSummaryOrPractices[[1]][['year0']]$adaptiveMultiPaddockGrazing[i])==TRUE, 0,
                                       ifelse(landUseSummaryOrPractices[[1]][['year0']]$adaptiveMultiPaddockGrazing[i]==FALSE, 0,
                                              nbYears_initialLandUse_wasApplied)))
    current_AMP_years = 0
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
      ## counting AMP years to calculate related efficiency
      # efficiency is assumed to be reversible
      # if AMP is not happening, efficiency will go backward
      till=unlist(year_chosen$tillingEvent[i])
      minTill=unlist(year_chosen$minimumTillingEvent[i])
      if (sum(till)>0){ # in case of conventional tillage over a grassland 
        # AMP related productivity benefits got erased
        current_AMP_years = - previous_AMP_years
      } else if (sum(minTill)>0){ # in case of minimum tillage
        # AMP related productivity benefits got penalized
        years_lost_by_tilling = 1
        for (k in c(1:years_lost_by_tilling)){
          current_AMP_years = current_AMP_years + ifelse(previous_AMP_years+current_AMP_years>0,-1,0)
        }
      } else {
        current_AMP_years = current_AMP_years + ifelse(is.na(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$adaptiveMultiPaddockGrazing[i])==TRUE, ifelse(previous_AMP_years+current_AMP_years>0,-1,0),
                                                       ifelse(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$adaptiveMultiPaddockGrazing[i]==FALSE, ifelse(previous_AMP_years+current_AMP_years>0,-1,0),
                                                              1))
      }
      # Calculation of pasture_efficiency: an index of enhanced productivity due to AMP grazing
      pasture_efficiency = 1 + pasture_efficiency_potential_difference *
        (exp(-0.36*previous_AMP_years)-exp(-0.36*(previous_AMP_years+current_AMP_years)))#0.36 factor allows to reach 2/3 of potential efficiency increase after 3 years of AMP
      # selecting the type of land use were grazing management affects most pasture efficiency 
      # monthly yield and residue (to avoid double-counting we will only look at grasslands)
      if (year_chosen$landUseType[i]!='Arablecrops'){
        # monthly yield and residue
        monthly_grazing_yield = data.frame(grazing_yield=rep(0,12),residue_left=rep(0,12))
        for (k in c(1:12)){
          monthly_grazing_yield$grazing_yield[k] = new.as_numeric(year_chosen$grazingYield[i][[1]][[k]])
          monthly_grazing_yield$residue_left[k] = new.as_numeric(year_chosen$estimationAfterResidueGrazingHarvest[i][[1]][[k]])
        }
        # if willing to correct total grazing yield by using a CF-made estimation, we re-weight the grazing yields
        if (pars$CFmade_grazing_estimations_Yes_No == "Yes"){
          grazing_table_temp = total_grazing_table %>% filter(scenario==paste('year',j,sep=""))
          if (grazing_table_temp$bale_grazing_total>grazing_table_temp$expected_grazing_needs_tDM){
            log4r::error(my_logger,"CAUTION ! Bale grazing alone overcomes expected grazing needs, to be checked.")
          } else if (grazing_table_temp$grazing_total==0){
            # grazing arbitrarily equally distributed over grazed land, 2 month a year (6 months apart) if no grazing yield announced
            half_yearly_grazing_yield_per_ha = 1/2*(grazing_table_temp$expected_grazing_needs_tDM-grazing_table_temp$bale_grazing_total*0.85)/sum(parcel_inputs$area) 
            monthly_grazing_yield$grazing_yield = c(half_yearly_grazing_yield_per_ha,rep(0,5),half_yearly_grazing_yield_per_ha,rep(0,5))
          } else {
            # grazing arbitrarily equally distributed over time weighted by parcel grazing yield relatively to farm level, if known
            half_yearly_grazing_yield_per_ha = 1/2*sum(monthly_grazing_yield$grazing_yield)/grazing_table_temp$grazing_yield_non_arable_lands*(grazing_table_temp$expected_grazing_needs_tDM_pastures-grazing_table_temp$pasture_yield_weighted_bale_grazing*0.85) 
            monthly_grazing_yield$grazing_yield = c(half_yearly_grazing_yield_per_ha,rep(0,5),half_yearly_grazing_yield_per_ha,rep(0,5))
          }
        }
        # fresh or dry tOM/ha
        if (is.na(year_chosen$yieldsResiduesDryOrFresh[i])==TRUE){
          dryOrFresh = "Dry"
          log4r::info(my_logger, paste("CAUTION: dryOrFresh is NA in parcel ",landUseSummaryOrPractices[[1]]$parcelName[i],
                                        " for year ",j,". Was ASSUMED to be dry.", sep=""))
        } else {
          dryOrFresh = year_chosen$yieldsResiduesDryOrFresh[i]
        }
        ### building df for C inputs calculation
        # PDZ with 2 grass growing season
        if (farm_EnZ == "Mediterranean north" | farm_EnZ == "Mediterranean south"){
          endWinterSeason = 5 # month index
          endSummerSeason = 10 # month index
          # Project scenario: year 2 to 10
          pasture_inputs <- rbind(pasture_inputs, 
                                  data.frame(scenario = c(paste('year',j,sep="")),
                                             parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                             grass = c("Generic grasses"),
                                             perennial_frac = c(ifelse(year_chosen$landUseType[i]=='Arablecrops', 
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
                                               dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max(monthly_grazing_yield$grazing_yield[c(1:endWinterSeason,endSummerSeason:12)]+monthly_grazing_yield$residue_left[c(1:endWinterSeason,endSummerSeason:12)]) +
                                                                         max(monthly_grazing_yield$grazing_yield[c(endWinterSeason:endSummerSeason)]+monthly_grazing_yield$residue_left[c(endWinterSeason:endSummerSeason)]),0)), 
                                               fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh", max(monthly_grazing_yield$grazing_yield[c(1:endWinterSeason,endSummerSeason:12)]+monthly_grazing_yield$residue_left[c(1:endWinterSeason,endSummerSeason:12)]) +
                                                                    max(monthly_grazing_yield$grazing_yield[c(endWinterSeason:endSummerSeason)]+monthly_grazing_yield$residue_left[c(endWinterSeason:endSummerSeason)]),0)), 
                                               pasture_efficiency = c(1/(1+pasture_efficiency_potential_difference*(1-exp(-0.36*previous_AMP_years))))))
          }
        # PDZ with only one growing season 
        } else { # PDZ with only one growing season
          pasture_inputs <- rbind(pasture_inputs, 
                                  data.frame(scenario = c(paste('year',j,sep="")),
                                             parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                             grass = c("Generic grasses"),
                                             perennial_frac = c(ifelse(year_chosen$landUseType[i]=='Arablecrops', 
                                                                       0, (previous_AMP_years+current_AMP_years)*0.02)), #CAUTION assumption that perennials increase linearly with AMP by a rate of 2% per year
                                             n_fixing_frac = c(0), # CAUTION: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                             dry_yield = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$grazing_yield),0)), 
                                             fresh_yield = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$grazing_yield),0)), 
                                             dry_residual = c(ifelse(dryOrFresh=="Dry", sum(monthly_grazing_yield$residue_left),0)), 
                                             fresh_residual = c(ifelse(dryOrFresh=="Fresh", sum(monthly_grazing_yield$residue_left),0)), 
                                             dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max(monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left),0)), 
                                             fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh", max(monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left),0)), 
                                             pasture_efficiency = c(pasture_efficiency) ))
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
                                               dry_agb_peak = c(ifelse(dryOrFresh=="Dry", max(monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left))),
                                               fresh_agb_peak = c(ifelse(dryOrFresh=="Fresh", max(monthly_grazing_yield$grazing_yield+monthly_grazing_yield$residue_left))),
                                               pasture_efficiency = c(1/(1+pasture_efficiency_potential_difference*(1-exp(-0.36*previous_AMP_years))))))
          }
        }
      }
    }
  }
  return(pasture_inputs)
}

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
        silt = c(get_silt_content(soilAnalysis, soilMapsData)),
        SOC = c(get_SOC_content(soilAnalysis, soilMapsData)),
        bulk_density = c(get_bulk_density(soilAnalysis, soilMapsData)),
        irrigation = c(ifelse(is.null(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])==TRUE,FALSE,
                              ifelse(is.na(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])==TRUE,FALSE,
                                     landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])))))
      if (j==0){
        soil_inputs <- rbind(soil_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
          scenario = c("baseline"),
          clay = c(get_clay_content(soilAnalysis, soilMapsData)),
          silt = c(get_silt_content(soilAnalysis, soilMapsData)),
          SOC = c(get_SOC_content(soilAnalysis, soilMapsData)),
          bulk_density = c(get_bulk_density(soilAnalysis, soilMapsData)),
          irrigation = c(ifelse(is.null(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])==TRUE,FALSE,
                                ifelse(is.na(landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])==TRUE,FALSE,
                                       landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]$irrigation[i])))))
      }
    }
  }
  return(soil_inputs)
}

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
          tilling_factor = c(ifelse(year_chosen$tillingEvent[i][[1]][[k]]==TRUE, tilling_factor, 
                                    ifelse(year_chosen$minimumTillingEvent[i][[1]][[k]]==TRUE, minimum_tillage_factor, 1)))))
      }
    }
  }
  tilling_inputs = tilling_inputs %>% group_by(parcel_ID, scenario) %>%
    summarise(tilling_factor = max(tilling_factor)) # ATM JUST TAKE THE MAX IMPACT EVENT
  return(tilling_inputs)
}
