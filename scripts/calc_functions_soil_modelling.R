library(tidyr)

get_monthly_Cinputs_animals <- function (animal_inputs, animal_factors, scenario_chosen, parcel){
  animals = merge(x = filter(animal_inputs,scenario==scenario_chosen & parcel_ID==parcel), y = animal_factors, by = "species", all.x = TRUE) %>% 
    mutate (C_inputs_manure_kg_per_ha_per_year= n_animals*c_kg_per_year_per_animal/area*grazing_days/365)
  tC_inputs_per_ha_per_year = sum(animals$C_inputs_manure_kg_per_ha_per_year)*1e-3
  return(tC_inputs_per_ha_per_year)
}


get_monthly_Cinputs_agroforestry <- function (agroforestry_inputs, agroforestry_factors, scenario_chosen, parcel){
  zone=ifelse(lat_farmer<57,"Temperate","Boreal")
  trees = merge(x = filter(agroforestry_inputs,scenario==scenario_chosen & parcel_ID==parcel), y = filter(agroforestry_factors,climatic_zone==zone), by = "tree_species", all.x = TRUE) %>% 
    mutate (tree_density=ifelse(is.na(n_trees)==FALSE,n_trees/area,typical_tree_density)) %>%
    mutate (tC_inputs_tree_per_ha_per_year=ifelse(is.na(tree_density)==FALSE & is.na(dbh)==FALSE & is.na(a_bg)==FALSE & is.na(b_bg)==FALSE, tree_density*(a_bg+b_bg*dbh)*C_frac_dry*root_turnover_rate,
                                           ifelse(is.na(tree_density)==FALSE & is.na(forest_biomass_kg)==FALSE & is.na(rs_ratio)==FALSE, tree_density*forest_biomass_kg*rs_ratio*C_frac_dry*root_turnover_rate*1e-3,
                                           paste("Insufficient input data for",tree_species))))
  tC_inputs_per_ha_per_year = sum(as.numeric(trees$tC_inputs_tree_per_ha_per_year))
  return(tC_inputs_per_ha_per_year)
}


get_monthly_Cinputs_pasture <- function (pasture_inputs, pasture_data, scenario_chosen, parcel){
  annual_pastures <- merge(x = filter(pasture_inputs,scenario==scenario_chosen & parcel_ID==parcel), y = filter(pasture_data, pasture_type=="annual"), by = "grass", all.x = TRUE) %>% 
    mutate(c_input_shoot= pasture_efficiency*ag_dm_peak*dry_c*(1-perc_agb_exported+ag_turnover)) %>%
    mutate(c_input_root= pasture_efficiency*ag_dm_peak*r_s_ratio*dry_c*bg_turnover) %>%
    mutate(c_inputs = c_input_shoot + c_input_root)
  perennial_pastures <- merge(x = filter(pasture_inputs,scenario==scenario_chosen & parcel_ID==parcel), y = filter(pasture_data, pasture_type=="perennial"), by = "grass", all.x = TRUE) %>% 
    mutate(c_input_shoot= pasture_efficiency*ag_dm_peak*dry_c*ag_turnover) %>%
    mutate(c_input_root= pasture_efficiency*ag_dm_peak*r_s_ratio*dry_c*bg_turnover) %>%
    mutate(c_inputs = c_input_shoot + c_input_root)
  tC_inputs_per_ha_per_year = sum(perennial_pastures$c_inputs*perennial_pastures$perennial_frac,na.rm=T)+
    sum(annual_pastures$c_inputs*(1-annual_pastures$perennial_frac),na.rm=T)
  return(tC_inputs_per_ha_per_year)
}

get_monthly_Cinputs_crop <- function (crop_inputs, crop_data, scenario_chosen, parcel){
  crops <- merge(x = filter(crop_inputs,scenario==scenario_chosen & parcel_ID==parcel), y = crop_data, by = "crop", all.x = TRUE)
  crops <- crops %>%
    mutate(c_input_shoot= ifelse(is.na(dry_residue)==FALSE, dry_residue*dry_c,
                                 ifelse(is.na(fresh_residue)==FALSE, fresh_residue*dry*dry_c,
                                        ifelse(is.na(dry_residue)==TRUE & is.na(fresh_residue)==TRUE & is.na(residue_frac)==TRUE,NA,
                                               ifelse(is.na(dry_yield)==FALSE, dry_yield*dry_c*residue_frac,
                                                      ifelse(is.na(fresh_yield)==FALSE, fresh_yield*dry*dry_c*residue_frac,
                                                             ifelse(is.na(ag_dm_peak)==FALSE, ag_dm_peak*dry_c*residue_frac,
                                                                    NA))))))) %>%
    mutate(c_input_root= ifelse(is.na(dry_residue)==FALSE & is.na(dry_yield)==FALSE, (dry_yield+dry_residue)*dry_c*r_s_ratio,
                                ifelse(is.na(fresh_residue)==FALSE & is.na(fresh_yield)==FALSE, (fresh_yield+fresh_residue)*dry*dry_c*r_s_ratio,
                                       ifelse(is.na(residue_frac)==TRUE,NA,
                                              ifelse(is.na(dry_yield)==FALSE, dry_yield/(1-residue_frac)*dry_c*r_s_ratio,
                                                     ifelse(is.na(fresh_yield)==FALSE, fresh_yield/(1-residue_frac)*dry*dry_c*residue_frac*r_s_ratio,
                                                            ifelse(is.na(ag_dm_peak)==FALSE, ag_dm_peak*dry_c*r_s_ratio,
                                                                   NA))))))) %>%
    mutate(c_inputs= c_input_shoot + c_input_root)
  
  # if (is.na(crops$dry_residue)==TRUE & is.na(crops$fresh_residue)==TRUE & is.na(crops$residue_frac)==TRUE){
  #   print("No residue data")
  # }
  # if (is.na(crops$dry_yield)==TRUE & is.na(crops$fresh_yield)==TRUE & is.na(crops$ag_dm_peak)==FALSE){
  #   print("No yield data")
  # }
  tC_inputs_per_ha_per_year = sum(crops$c_inputs)
  return(tC_inputs_per_ha_per_year)
}