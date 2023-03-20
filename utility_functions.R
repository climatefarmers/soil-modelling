## UTILITY FUNCTIONS


#Functions for extracting LB2 Data from soil model results
#Project activity: Residue management
get_residue_management_data <- function(pasture_inputs,crop_inputs, parcel_inputs) {
  
  # Needs to be adapted depending on considering dry or fresh residues
  grass_residues = pasture_inputs$dry_residual
  grass_residues_input <- left_join(pasture_inputs, parcel_inputs, by = "parcel_ID")
  grass_residues_input <- grass_residues_input %>% mutate(residues_parcel = grass_residues*area) %>% group_by(scenario) %>% summarise(sum_grass_residues = sum(residues_parcel))
  # crop_inputs_new <- crop_inputs %>% group_by(scenario) %>% mutate(sum_year_residues = sum(dry_residual))
  # 
  crop_residues = crop_inputs$dry_residue
  crop_residues_input <- left_join(crop_inputs, parcel_inputs, by = "parcel_ID")
  crop_residues_input <- crop_residues_input %>% mutate(residues_parcel = crop_residues*area) %>% group_by(scenario) %>% summarise(sum_crop_residues = sum(residues_parcel))
  
  total_residues_input <- left_join(grass_residues_input,crop_residues_input, by = "scenario") %>% mutate(sum_residues = sum_grass_residues + sum_crop_residues)
  return(total_residues_input)
  
}

residues = get_residue_management_data(pasture_inputs,crop_inputs, parcel_inputs)

#Project activity: Add manures
get_manure_data <- function(add_manure_inputs, parcel_inputs) {
  
  # Needs to be adapted depending on considering dry or fresh residues
  total_add_manure_table <- left_join(add_manure_inputs, parcel_inputs, by = "parcel_ID")
  
  total_add_manure_inputs <- total_add_manure_table %>% filter(quantity_t_ha != 0) %>% group_by(scenario,manure_source) %>% summarise(total_add_manure = sum(quantity_t_ha*area/sum(area)))
  total_add_manure_area <- total_add_manure_table %>% filter(quantity_t_ha != 0) %>% group_by(scenario,manure_source) %>% summarise(total_area = sum(area))
  #total_add_manure_inputs <- total_add_manure_inputs %>% mutate(quantity_parcel = quantity_t_ha*area) %>% group_by(scenario,manure_source) %>% summarise(total_add_manure = sum(quantity_t_ha))
  total_add_manure_import_frac <- total_add_manure_table %>% filter(quantity_t_ha != 0) %>% group_by(scenario,manure_source) %>% summarise(average_imported_fraction= mean(imported_frac))
  
  total_add_manure_inputs <- pivot_wider(total_add_manure_inputs, names_from = manure_source, values_from = total_add_manure)
  total_add_manure_area <- pivot_wider(total_add_manure_area, names_from = manure_source, values_from = total_area)
  total_add_manure_import_frac <- pivot_wider(total_add_manure_import_frac, names_from = manure_source, values_from = average_imported_fraction)
  
  #total_add_manure_area <- pivot_wider(total_add_manure_area,names_from = manure_source, values_from = total_area)
  total_add_manure_table <- left_join(total_add_manure_inputs,total_add_manure_area, by = "scenario")
  total_add_manure_table <- left_join(total_add_manure_table,total_add_manure_import_frac, by = "scenario")
  return(total_add_manure_table)

}

#Project activity: Cover crops
get_cc_data <- function(crop_inputs, parcel_inputs) {
  total_cc_input <- left_join(crop_inputs, parcel_inputs, by = "parcel_ID")
  total_cc_input <- total_cc_input %>% filter(crop == "Non-N-fixing dry forages") %>% mutate(cc_parcel = dry_yield*area) %>% group_by(scenario) %>% summarise(total_cc = sum(cc_parcel))
}

get_cc_yield_list <- function(farmId_list){
  cc_yield_list = c()
  connection_string = init_file$connection_string_prod
  farms_collection = mongo(collection="farms", db="carbonplus_production_db", url=connection_string)
  for(farmId in farmId_list){
    farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""))
    landUseSummaryOrPractices = farms_everything$landUse$landUseSummaryOrPractices
    crop_inputs = data.frame(scenario = c(), parcel_ID = c(), crop = c(), dry_yield = c(), 
                             fresh_yield = c(), dry_grazing_yield = c(), fresh_grazing_yield = c(),
                             dry_residue = c(), fresh_residue = c(), 
                             dry_agb_peak = c(), fresh_agb_peak = c() )
    for (j in c(0:1)){ #years
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year',j,sep="")]]
      for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
        if (year_chosen$landUseType[i]=="Arablecrops"){
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
            if(is.na(crop_chosen)==TRUE){ # crop_chosen = NA, meaning no cash crop
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
    if(nrow(crop_inputs)>0){
      crop_inputs = crop_inputs %>% mutate(cc_prod=dry_yield+dry_grazing_yield+dry_residue)
      cc_yield_list_farm = crop_inputs %>% filter(cc_prod>0)
      if(nrow(cc_yield_list_farm)>0){
        cc_yield_list = c(cc_yield_list,mean(na.omit(cc_yield_list_farm$cc_prod))) 
      }
    }
  }
  return(cc_yield_list)
}