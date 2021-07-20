# harvest_index.R

# Estimate the soil carbon input through the harvest index

# HI = mass of harvest (t) / mass of above-ground biomass (t)


# Yield (t/ha/yr)
# Carbon concentration (default 0.45)
# Harvest index
# Shoot-to-root-ratio
# Rhizodeposition ratio (default 0.65 after Bolinder)
# Ss (-)
# Sr (default 1)
clean_crop_variable_data <- function(
  crop_data = read.csv("data/crop_values.csv")
  ){
  
  # check for duplicates in crop name
  # check for missing values
  # check whether maximums are necessary
  
}



get_crop_variable <- function(
  crop = "Cereal",
  variable_name = "harvest_index",
  crop_data = read.csv("data/crop_values.csv")
){
  
  if(crop %in% crop_data$crop_type){
    
    if(!variable_name %in% colnames(crop_data))(stop("Variable name not in data."))
    
    crop_variable <- crop_data %>% 
      filter(crop_type == crop) %>% 
      pull(variable_name)
    
  }else{
    
    stop("Crop not present in data file. Get additional data.")
    
  }
  
  return(crop_variable)
  
}




calculate_carbon_input <- function(
  dry_yield = 1000,         # t/ha/year
  harvest_index = 0.5,
  root_to_shoot_ratio = 0.18,
  rhizo_ratio = 0.65,       # Bolinder number
  residue_remaining = 0.25, # 0 - 1 with 1 meaning all residue left on ground
  root_decomposition = 1    # default. 0 implies all roots are removed from the soil
){
  
  yield_bm <- (dry_yield*0.45)
  above_ground_bm <- yield_bm *(1-harvest_index)/harvest_index
  total_bm <- yield_bm + above_ground_bm
  roots_bm <- total_bm * root_to_shoot_ratio
  extra_roots_bm <- roots_bm * rhizo_ratio
  
  
  total_c_input <- above_ground_bm + roots_bm + extra_roots_bm
  # kg C / ha
  total_c_input_tc <- total_c_input /1000
  
  return(total_c_input_tc)
  
}
