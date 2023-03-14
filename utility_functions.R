## UTILITY FUNCTIONS


#Functions for extracting LB2 Data from soil model results
#Project activity: Residue management
get_residues_per_year <- function(pasture_inputs,crop_inputs, parcel_inputs) {
  
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

#Project activity: Add manures
get_add_manures_per_year <- function(add_manure_inputs, parcel_inputs) {
  
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
