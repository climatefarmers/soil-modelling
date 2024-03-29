library(tidyr)

### Calculation of added manure input: Carbon input due to manure/compost/hay daily spreading over a grazed field
# YEARLY
get_monthly_Cinputs_add_manure <- function (add_manure_inputs, manure_factors, scenario_chosen, parcel){
  if(nrow(add_manure_inputs)==0){
    return(0)}
  add_manure_inputs = filter(add_manure_inputs,scenario==scenario_chosen & parcel_ID==parcel)
  add_manure = merge(x = add_manure_inputs, y = manure_factors, by = "manure_source", all.x = TRUE) %>% 
    mutate (tC_inputs_add_manure= quantity_t_ha*remaining_frac*carbon_content)
  tC_inputs_add_manure = sum(add_manure$tC_inputs_add_manure)
  return(tC_inputs_add_manure)
}

### Calculation of animal input: Carbon input due to manure daily spreading over a grazed field
# YEARLY
get_monthly_Cinputs_animals <- function (animal_inputs, animal_factors, scenario_chosen, parcel){
  if(nrow(animal_inputs)==0){
    return(0)}
  animal_inputs = filter(animal_inputs,scenario==scenario_chosen & parcel_ID==parcel & n_animals>0)
  animals = merge(x = animal_inputs, y = animal_factors, by = "species", all.x = TRUE) %>% 
    mutate (C_inputs_manure_kg_per_ha_per_year= n_animals*c_kg_per_year_per_animal/area*grazing_days/365)
  tC_inputs_per_ha_per_year = sum(animals$C_inputs_manure_kg_per_ha_per_year)*1e-3
  return(tC_inputs_per_ha_per_year)
}

### Calculation of agroforestry input
# YERLY
get_monthly_Cinputs_agroforestry <- function (agroforestry_inputs, agroforestry_factors, scenario_chosen, parcel, lat_farmer){
  if(nrow(agroforestry_inputs)==0){
    return(0)}
  agroforestry_inputs = filter(agroforestry_inputs,scenario==scenario_chosen & parcel_ID==parcel)
  # Difference in factors depending on climatic zone
  zone=ifelse(lat_farmer<57,"Temperate","Boreal")
  trees = merge(x = agroforestry_inputs, 
                y = filter(agroforestry_factors,climatic_zone==zone), by = "tree_species", all.x = TRUE) %>% 
    # n_trees removed from input template # mutate (tree_density=ifelse(is.na(tree_density)==FALSE,tree_density,ifelse(is.na(n_trees)==FALSE,n_trees/area,typical_tree_density))) %>%
    # Calculation of c input depending on data availability and dbh
    mutate (tC_inputs_tree_per_ha_per_year=ifelse(is.na(tree_density)==FALSE & is.na(dbh)==FALSE & is.na(a_bg_over30)==FALSE & is.na(b_bg_over30)==FALSE & is.na(b_bg_below30)==FALSE, 
                                                  ifelse(dbh>29,tree_density*(a_bg_over30+b_bg_over30*dbh)*C_frac_dry*root_turnover_rate,
                                                         tree_density*(b_bg_below30*dbh**2.5)*C_frac_dry*root_turnover_rate),
                                                  ifelse(is.na(tree_density)==FALSE & is.na(forest_biomass_kg)==FALSE & is.na(rs_ratio)==FALSE, 
                                                         tree_density*forest_biomass_kg*rs_ratio*C_frac_dry*root_turnover_rate*1e-3,
                                                         paste("Insufficient input data for",tree_species))))
  tC_inputs_per_ha_per_year = sum(as.numeric(trees$tC_inputs_tree_per_ha_per_year))
  return(tC_inputs_per_ha_per_year)
}

### Calculation of pasture input: Carbon input from pasture biomass turnover
# YEARLY
get_monthly_Cinputs_pasture <- function (pasture_inputs, pasture_data, scenario_chosen, parcel){
  if(nrow(pasture_inputs)==0){
    return(0)}
  pasture_inputs <- pasture_inputs %>% filter(scenario==scenario_chosen & parcel_ID==parcel) %>%
    mutate(dry_residual = ifelse(is.na(dry_residual)==FALSE, dry_residual, 
                                 ifelse(is.na(fresh_residual)==FALSE,fresh_residual*dry,
                                        NA))) %>%
    mutate(dry_yield = ifelse(is.na(dry_yield)==FALSE, dry_yield, 
                              ifelse(is.na(fresh_yield)==FALSE,fresh_yield*dry,
                                     NA))) %>%
    mutate(dry_agb_peak = ifelse(is.na(dry_agb_peak)==FALSE, dry_agb_peak, 
                                 ifelse(is.na(fresh_agb_peak)==FALSE,fresh_agb_peak*dry,
                                        NA)))
  annual_pastures <- merge(x = pasture_inputs, 
                           y = filter(pasture_data, pasture_type=="annual"), by = "grass", all.x = TRUE) %>% 
    mutate(c_input_shoot = (dry_residual+dry_yield*0.15)*pasture_efficiency*dry_c) %>%
    mutate(c_input_root = pasture_efficiency*dry_agb_peak*r_s_ratio*dry_c*bg_turnover) %>%
    mutate(c_inputs = c_input_shoot + c_input_root)
  perennial_pastures <- merge(x = pasture_inputs,
                              y = filter(pasture_data, pasture_type=="perennial"), by = "grass", all.x = TRUE) %>% 
    mutate(c_input_shoot= (dry_yield*0.15+pasture_efficiency*dry_agb_peak*ag_turnover)*dry_c) %>%
    mutate(c_input_root= pasture_efficiency*dry_agb_peak*r_s_ratio*dry_c*bg_turnover) %>%
    mutate(c_inputs = c_input_shoot + c_input_root)
  tC_inputs_per_ha_per_year = sum(perennial_pastures$c_inputs*perennial_pastures$perennial_frac,na.rm=T)+
    sum(annual_pastures$c_inputs*(1-annual_pastures$perennial_frac),na.rm=T)
  
  return(tC_inputs_per_ha_per_year)
}

### Calculation of crop input: Carbon input from cash crops and cover crops biomass turnover rates
# YEARLY
get_monthly_Cinputs_crop <- function (crop_inputs, crop_data, scenario_chosen, parcel, farm_EnZ){
  if(nrow(crop_inputs)==0){
    return(0)}
  crops <- merge(x = filter(crop_inputs,scenario==scenario_chosen & parcel_ID==parcel), 
                 y = filter(crop_data,pedo_climatic_area==farm_EnZ | is.na(pedo_climatic_area)==TRUE), by = "crop", all.x = TRUE)
  crops <- crops %>%
    mutate(c_shoot= ifelse(is.na(dry_residue)==FALSE, dry_residue*dry_c,
                           ifelse(is.na(fresh_residue)==FALSE, fresh_residue*dry*dry_c,
                                  ifelse(is.na(dry_residue)==TRUE & is.na(fresh_residue)==TRUE & is.na(residue_frac)==TRUE,NA,
                                         ifelse(is.na(dry_yield)==FALSE, dry_yield*dry_c*residue_frac,
                                                ifelse(is.na(fresh_yield)==FALSE, fresh_yield*dry*dry_c*residue_frac,
                                                       ifelse(is.na(ag_dm_peak)==FALSE, ag_dm_peak*dry_c*residue_frac, 
                                                              NA))))))) %>% # Warning should be implemented
    mutate(c_root= ifelse(is.na(dry_agb_peak)==FALSE, dry_agb_peak*dry_c*r_s_ratio,
                          ifelse(is.na(fresh_agb_peak)==FALSE, fresh_agb_peak*dry*dry_c*r_s_ratio,
                                 ifelse(is.na(ag_dm_peak)==FALSE, ag_dm_peak*dry_c*r_s_ratio,
                                        NA)))) %>% # Warning should be implemented
    mutate(c_inputs= c_shoot*s_turnover + c_root*r_turnover)
  
  # if (is.na(crops$dry_residue)==TRUE & is.na(crops$fresh_residue)==TRUE & is.na(crops$residue_frac)==TRUE){
  #   print("No residue data")
  # }
  # if (is.na(crops$dry_yield)==TRUE & is.na(crops$fresh_yield)==TRUE & is.na(crops$ag_dm_peak)==FALSE){
  #   print("No yield data")
  # }
  tC_inputs_per_ha_per_year = sum(crops$c_inputs)
  
  return(tC_inputs_per_ha_per_year)
}