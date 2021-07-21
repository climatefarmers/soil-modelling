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
  crop_data = read_csv("data/crop_values.csv", col_types =  "cdddd")
  ){
  
  
  crop_data$crop_type <- tolower(crop_data$crop_type)
  
  crop_data <- crop_data %>% distinct()
  
  if(any(duplicated(crop_data$crop_type))){stop("Duplicated crop data. Remove multiple entries.")}
  
  if(any(is.na(crop_data))){stop("Missing data")}
  
  if(any(crop_data$rhizodeposition > 1.0)){stop("Rhiodeposition values exceed 1")}
  
  if(any(crop_data$carbon_content > 1.0)){stop("Carbon Content values exceed 1")}
  # check for duplicates in crop name
  # check for missing values
  # check whether maximums are necessary
  
  return(crop_data)
  
}

# TO DO: enable this to calculate biomass automatically for a specific crop or rotation

clear_carbon_input_data <- function(carbon_input_data, crop_data){
  
  carbon_input_data$crop <- tolower(carbon_input_data$crop)
  
  crops <- carbon_input_data %>% 
    filter(crop != "manure") %>% 
    distinct(crop) 
  
  missing_data <- setdiff(crops$crop, crop_data$crop_type)
  
  if(length(missing_data) > 0){stop(paste("Missing crop data for", paste(missing_data, collapse = ", ")))}
  
  return(carbon_input_data)
  
}

get_crop_variable <- function(
  crop = "Cereal",
  variable_name = "harvest_index",
  crop_data = read.csv("data/crop_values.csv")
){
  
  crop <- tolower(crop)
  
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


check_field_differences <- function(
  field_parameters, 
  carbon_input_data
){
  
  fields_unique_fp <- unique(field_parameters$field_id)
  fields_unique_ci <- unique(carbon_input_data$field_id)
  
  ci_missing <- setdiff(fields_unique_fp, fields_unique_ci)
  fp_missing <- setdiff(fields_unique_ci, fields_unique_fp)
  
  if(length(ci_missing) > 0){stop(paste("Missing Field data for fields", ci_missing))}
  if(length(fp_missing) > 0){stop(paste("Missing Carbon Input data for fields", fp_missing))}
  
  
}
