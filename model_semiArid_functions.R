# SoilR functions

convert_to_tonnes <- function(value, 
                              conversion_factor = 3.67
){
  
  tonnes = value * conversion_factor
  
  return(tonnes)
  
}

get_monthly_dataframe <- function(time_horizon = 10, 
                                  add_month = T){
  
  years <- seq(1/12, time_horizon+1/12, by = 1/12)
  
  if (add_month == F){  years <- seq(1/12, time_horizon, by = 1/12)}
  
}

prep_soil_moisture_factor <- function(
  time_horizon, 
  bare = TRUE,   # This can be a logical, or a string of 12 logicals
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  silt=silt,
  bulk_density=bulk_density,
  SOC=SOC,
  pE=pE,
  tilling_factor=1,
  version=version
  ){
  
  # Monthly data frame
  years <- get_monthly_dataframe(time_horizon, add_month = F)
  
  # Calculate monthly temperature effects
  fT <- fT.RothC(temp) 
  
  # Calculate effect of coverage
  
  fCov = ifelse(bare == TRUE, 1, 0.6)
  
  if(length(bare) == 1 & version=="Normal"){
    # Calculate monthly moisture effects
    fW <- fW.RothC(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pE = pE, 
      bare = bare
    )$b 
  }else if(length(bare) == 12 & version=="Normal"){ 
    # Use the modified version if there is monthly variation in coverage
    fW <- fW.RothC.Modified(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pE = pE, 
      bare_profile = bare
    )$b 
  }
  if(version=="Semi-arid"){ 
    # Use the modified version if there is monthly variation in coverage
    fW <- fW.RothC.Modified_semiArid(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pSilt = silt,
      bulkDensity = bulk_density,
      SOC=SOC,
      pE = pE
    )$b 
  }

  # Moisture factors over time
  xi_frame <- data.frame(years, moisture_factor = rep(fT * fW * fCov * tilling_factor, length.out = length(years)))
  return(xi_frame)
  
}

calc_soil_carbon <- function(
  time_horizon = 10,
  xi_frame,
  c_inputs = c_inputs,
  dr_ratio = 1.44,
  fym_inputs = 0,
  pE = 1.0,
  clay = 20,
  PS = c(DPM=0,RPM=0,BIO=0,HUM=0,IOM=0)
){
  # Monthly data frame
  years <- get_monthly_dataframe(time_horizon, add_month = F)
  
  # Loads the model
  Model <- RothCModel(
    t = years, 
    ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, k.ION = 0),
    C0 = c(DPM = PS),
    In = c_inputs, 
    DR = dr_ratio,
    clay = clay, 
    xi = xi_frame
  ) 
  
  # Calculates stocks for each pool per month  
  c_t <- getC(Model) 
  
  c_t <- as_tibble(c_t)
  names(c_t) <- c("DPM", "RPM", "BIO", "HUM", "IOM")
  
  return(c_t)
  
}

normalise_c_inputs <- function(
  c_in = 0, 
  fym_in = 0,
  dr_ratio_crops = 1.44,
  dr_ratio_fym = 1){
  
  if(c_in + fym_in != 0){
    dr_ratio = (dr_ratio_crops*c_in + dr_ratio_fym*fym_in) / (c_in + fym_in)
  }else{
    dr_ratio = 1
  }  
  return(dr_ratio)
  
}

get_total_C <- function(c_df){
  
  final_row <- as.numeric(tail(c_df, 1))
  
  tot_value <- sum(final_row)
  
  return(tot_value)
  
}

get_initial_C <- function(c_df){
  
  initial_row <- as.numeric(head(c_df,1))
  
  init_value <- sum(initial_row)
  
  return(init_value)
  
}

estimate_starting_soil_content <- function(
  SOC = 1,
  clay = 1
){
  
  RPM = (0.1847 * SOC + 0.1555)*(clay + 1.2750)^-0.1158 
  HUM = (0.7148 * SOC + 0.5069)*(clay + 0.3421)^0.0184 
  BIO = (0.0140 * SOC + 0.0075)*(clay + 8.8473)^0.0567 
  
  FallIOM <- 0.049 * SOC^(1.139)
  
  starting_soc = c(0,RPM, BIO, HUM, FallIOM)
  
  return(starting_soc)
  
}


calc_tilling_factor <- function(
  climate_zone = "temperate moist",
  practice = "no till",
  tilling_factors = tilling_factors
){
  
  climate_zone <- tolower(climate_zone)
  
  if(!climate_zone %in% unique(tilling_factors$climate)){stop("Check climate zone in tilling factors")}
  if(!practice %in% unique(tilling_factors$new_practice)){stop("Check practice in tilling factors")}
  
  
  tilling_factor <- tilling_factors %>% 
    filter(climate == !!climate_zone,
           previous_practice == "conventional till",
           new_practice == !!practice) %>% 
    pull(factor)
  
  if(length(tilling_factor) > 1){stop("Tilling factor not unique")}
  
  return(tilling_factor)
  
}


calc_carbon_over_time <- function(time_horizon = 10, 
                                  field_carbon_in = c(10,10,10,10,10,10,10,10,10,10), # annual input
                                  dr_ratios = c(1,1,1,1,1,1,1,1,1), # annual dr ratio
                                  bare_profile = c(T,T,T,T,T,T,T,T,T,T,T,T), # monthly coverage value
                                  temp = temp,
                                  precip = precip,
                                  evap = evap,
                                  soil_thick = soil_thick,
                                  clay = clay,
                                  pE = pE,
                                  silt=30,
                                  bulk_density=1.3,
                                  PS = starting_soil_content,
                                  tilling_factor = 1,
                                  version="Normal"){
  
  if(length(dr_ratios) != length(field_carbon_in)){stop("Field_carbon_in and dr_ratios should have same length, 1 entry per year")}
  
  SOC = sum(PS[c(1:5)])
  
  xi_frame <- prep_soil_moisture_factor(
    time_horizon = 1, 
    bare = bare_profile,   # This can be a logical, or a string of 12 logicals
    temp = temp,
    precip = precip,
    evap = evap,
    soil_thick = soil_thick,
    clay = clay,
    silt=silt,
    bulk_density=bulk_density,
    SOC=SOC,
    pE=pE,
    tilling_factor = tilling_factor,
    version=version)
  
  for (t in 1: time_horizon){
    
    c_in <- field_carbon_in[t]
    dr_in <- dr_ratios[t]
    
    # Runs the model for a single year, taking the inputs from the previous year as the SOC
    c_df <- calc_soil_carbon(
      time_horizon = 1,
      xi_frame,
      c_inputs = c_in,
      dr_ratio = dr_in,
      clay=clay,
      pE = pE,
      PS = PS
    )
    
    # Sets new starting_soil_content
    PS <- as.numeric(tail(c_df, 1))
    
    if(t == 1){
      all_c = c_df
    }else{
      all_c <- rbind(all_c, c_df)
    }
  }
  
  all_c <- all_c %>% rowwise() %>% mutate(TOT = sum(DPM, RPM, BIO, HUM, IOM))
  
  return(all_c)
}


get_bare_profile_single <- function(field_parameters){
  
  # input_parameters should be a single line of the input_parameter file
  
  ip0 <- field_parameters %>% select(contains("bare_profile"))
  
  if(ncol(ip0) != 12){stop("Missing information about the bare profile months. ")}
  
  bare_profile <- as.data.frame(t(ip0))$V1
  
  return(bare_profile)    
}

get_bare_profile_df <- function(field_parameters){
  
  ip <- field_parameters %>% 
    select(contains("bare_profile")) 
  
  bare_profile <- do.call(paste, c(ip[], sep = ", ")) 
  
  field_parameters <- field_parameters %>% 
    select(!contains("bare_profile")) %>% 
    cbind(bare_profile)
  
  return(field_parameters)
}


