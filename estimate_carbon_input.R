# harvest_index.R

clean_crop_variable_data <- function(
  crop_data
){
  
  
  crop_data$crop <- tolower(crop_data$crop)
  
  crop_data <- crop_data %>% distinct()
  
  if(any(duplicated(crop_data$crop))){stop("Duplicated crop data. Remove multiple entries.")}
  
  if(any(is.na(crop_data))){stop("Missing data")}
  
  if(any(crop_data$rhizodeposition > 1.0)){stop("Rhiodeposition values exceed 1")}
  
  if(any(crop_data$carbon_content > 1.0)){stop("Carbon Content values exceed 1")}
  # check for duplicates in crop name
  # check for missing values
  # check whether maximums are necessary
  
  return(crop_data)
  
}

clean_carbon_input_data <- function(
  carbon_input_data, 
  crop_data
){
  
  carbon_input_data$crop <- tolower(carbon_input_data$crop)
  carbon_input_data$case <- tolower(carbon_input_data$case)
  
  
  carbon_input_data <- carbon_input_data %>% 
    mutate(is_crop = ifelse(crop != "manure", "crop", "manure"))
  
  crops <- carbon_input_data %>% 
    filter(crop != "manure") %>% 
    distinct(crop) 
  
  missing_data <- setdiff(crops$crop, crop_data$crop)
  
  if(length(missing_data) > 0){stop(paste("Missing crop data for", paste(missing_data, collapse = ", ")))}
  
  # split into just base and regen
  case_types <- carbon_input_data %>% distinct(case)
  if(length(setdiff(case_types$case, c("base", "base + regen", "regen"))) > 0){
    stop("Case Type can only be base, base + regen or regen. Check text matches exactly.")
  }
  
  carbon_input_data_br <- carbon_input_data %>% 
    filter(case == "base + regen")
  
  carbon_input_data_base <- carbon_input_data_br %>% mutate(case = "base")  
  carbon_input_data_regen <- carbon_input_data_br %>% mutate(case = "regen")  
  
  carbon_input_data_all <- carbon_input_data %>% 
    filter(case != "base + regen") %>% 
    rbind(carbon_input_data_base,
          carbon_input_data_regen) 
  
  return(carbon_input_data_all)
  
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

summarise_carbon_inputs <- function(
  carbon_input_data,
  crop_data
){
  
  field_crop_data <- carbon_input_data %>% 
    left_join(crop_data, by = "crop") %>% 
    pivot_longer(contains("yield"), values_to = "yield") %>% 
    mutate(yield_bm = yield * 0.45,
           above_ground_bm = yield_bm *(1-harvest_index)/harvest_index,
           total_bm = yield_bm + above_ground_bm,
           roots_bm = total_bm * root_shoot_ratio,
           extra_roots_bm = roots_bm * rhizodeposition,
           total_c_input_tc = (above_ground_bm*residue + roots_bm + extra_roots_bm)/1000) %>% 
    mutate(across(contains("year"),
                  ~ case_when(is_crop == "manure" ~ annual_quantity,
                              is_crop == "crop" ~ .x * total_c_input_tc)))
  
  carbon_input_summary <- field_crop_data %>% 
    pivot_longer(cols = contains("year"), 
                 names_to = "year",
                 values_to = "carbon_input") %>% 
    group_by(field_id, case, is_crop, name, year) %>%
    summarise(carbon_input = sum(carbon_input, na.rm = TRUE), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(year = as.numeric(gsub("year_", "", year))) %>% 
    arrange(field_id, case, name, is_crop, year)
  
  carbon_inputs <- combine_crops_fym(carbon_input_summary)
  
  return(carbon_inputs)
}

combine_crops_fym <- function(
  carbon_input_summary, 
  dr_ratio_crops = 1.44,
  dr_ratio_fym = 1
){
  
  field_carbon_inputs <- carbon_input_summary %>% 
    mutate(c_in = 
             case_when(
               is_crop == "crop" ~ carbon_input * dr_ratio_crops,
               is_crop == "manure" ~ carbon_input * dr_ratio_fym
             )) %>% 
    group_by(field_id, case,name, year) %>% 
    summarise(carbon_inputs = sum(carbon_input, na.rm = T), 
              dr_ratio = sum(c_in, na.rm = T)/sum(carbon_input, na.rm = T), .groups = "drop") 
  
  return(field_carbon_inputs)
}

