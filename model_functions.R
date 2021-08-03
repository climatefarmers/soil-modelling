# SoilR functions

convert_to_tonnes <- function(value, 
                              conversion_factor = 3.67
){
  
  tonnes = value * conversion_factor
  
  return(tonnes)
  
}

get_monthly_dataframe <- function(time_horizon = 10, add_month = T){
  
  years <- seq(1/12, time_horizon+1/12, by = 1/12)
  
  if (add_month == F){  years <- seq(1/12, time_horizon, by = 1/12)}
  
}

calc_soil_carbon <- function(
  time_horizon = 10,
  bare = TRUE,   # This can be a logical, or a string of 12 logicals
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs,
  dr_ratio = 1.44,
  fym_inputs = 0,
  pE = 1.0,
  PS = c(DPM=0,RPM=0,BIO=0,HUM=0,IOM=0),
  description = "Base Case",
  project_name = "test"
){
  
  # Monthly data frame
  years <- get_monthly_dataframe(time_horizon, add_month = F)
  
  # Calculate monthly temperature effects
  fT <- fT.RothC(temp) 
  
  if(length(bare) == 1){
    # Calculate monthly moisture effects
    fW <- fW.RothC(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pE = pE, 
      bare = bare
    )$b 
  }else if(length(bare) == 12){ 
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
  
  # Moisture factors over time
  xi_frame <- data.frame(years, moisture_factor = rep(fT * fW, length.out = length(years)))
  
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



solve_for_c_0 <- function(
  SOC_target = 39,
  time_horizon = 10,
  bare = FALSE,   
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  dr_ratio = 1.44,
  fym_inputs = 0,
  pE = 1.0,
  PS = c(DPM=0,RPM=0,BIO=0,HUM=0,IOM=0)
){
  
  PS["IOM"] = 0.049 * SOC_target^(1.139) 
  
  # Use a solve function to solve for Cin
  
  # Monthly data frame
  years <- get_monthly_dataframe(time_horizon, add_month = F)
  
  # Calculate monthly temperature effects
  fT <- fT.RothC(temp) 
  
  if(length(bare) == 1){
    # Calculate monthly moisture effects
    fW <- fW.RothC(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pE = pE, 
      bare = bare
    )$b 
  }else if(length(bare) == 12){ 
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
  
  # Moisture factors over time
  xi_frame <- data.frame(years, moisture_factor = rep(fT * fW, length.out = length(years)))
  
  # Solves the difference between modeled SOC and measured SOC by varying c_inputs
  c_inputs <- bisect(calc_diff, 0, 100, 
                     years = years,
                     PS = PS,
                     SOC_target = SOC_target,
                     dr_ratio = dr_ratio,
                     clay = clay,
                     xi_frame = xi_frame)$root
  
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
  
  output_SOC <- tail(c_t,1)
  
  return(output_SOC)
  
}

calc_diff <- function(
  c_inputs,
  years = years,
  PS = PS,
  SOC_target = SOC_target,
  dr_ratio = dr_ratio,
  clay = clay,
  xi_frame = xi_frame){
  
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
  
  output_SOC <- get_total_C(tail(c_t,1))
  
  diff_SOC <- SOC_target - output_SOC
  
  if(abs(diff_SOC) < 0.1){diff_SOC = 0}
  
  return(diff_SOC)
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
    group_by(field_id, case, year) %>% 
    summarise(carbon_inputs = sum(carbon_input, na.rm = T), 
              dr_ratio = sum(c_in, na.rm = T)/sum(carbon_input, na.rm = T), .groups = "drop") 
  
  return(field_carbon_inputs)
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
  SOC = 1
){
  
  factors = c(0.0065, 0.1500, 0.0212, 0.8224)
  FallIOM <- 0.049 * SOC^(1.139)
  rest_soc = SOC - FallIOM
  
  starting_soc = c(factors * rest_soc, FallIOM)
  
  return(starting_soc)
  
}


apply_tilling_factors <- function(
  starting_soil_content,
  climate_zone = "temperate moist",
  previous_practice = "conventional till",
  new_practice = "no till",
  tilling_factors = tilling_factors
){
  
  climate_zone <- tolower(climate_zone)
  
  if(!climate_zone %in% unique(tilling_factors$climate)){stop("Check climate zone in tilling factors")}
  if(!previous_practice %in% unique(tilling_factors$previous_practice)){stop("Check previous practice in tilling factors")}
  if(!new_practice %in% unique(tilling_factors$new_practice)){stop("Check new practice in tilling factors")}
  
  
  tilling_factor <- tilling_factors %>% 
    filter(climate == !!climate_zone,
          previous_practice == !!previous_practice,
           new_practice == !!new_practice) %>% 
    pull(factor)
  
  if(length(tilling_factor) > 1){stop("Tilling factor not unique")}
  
  starting_soil_content <- starting_soil_content * c(rep(tilling_factor,4),1)
  
  return(starting_soil_content)
  
}


plot_c_stocks <- function(years, 
                          df,
                          plot_title = "",
                          project_name = "test"){
  
  df$year <- years
  df <- df %>% 
    pivot_longer(cols = c("DPM", "RPM", "BIO", "HUM", "IOM"),
                 names_to = "soil_comp")
  
  
  plot_title <- paste("Carbon Distribution - ", plot_title)
  plot <- ggplot(data = df, aes(x = year, y = value, group = soil_comp, colour = soil_comp))+
    geom_line()+
    labs(title = plot_title, x = "Time (years)", y = "C Stocks (t/ha)", color = "Soil\nComposition")+
    theme_classic()+
    theme(legend.position = "right")
  
  
  if(!dir.exists(file.path("plots", project_name))){dir.create(file.path("plots", project_name))}
  ggsave(plot, filename = file.path("plots", project_name, paste0(plot_title, ".png")), height = 4.62, width = 5.98)
  
}

plot_total_c <- function(years, 
                         df,
                         plot_title = "",
                         project_name = "test"){
  
  df <- df %>% rowwise() %>% summarise(c_tot = sum(DPM, RPM, BIO, HUM, IOM), .groups = "drop")
  df$year <- years
  
  plot <- ggplot(data = df, aes(x = year, y = c_tot))+
    geom_line()+
    labs(title = plot_title, x = "Time (years)", y = "C Stocks (t/ha)")+
    theme_classic()
  
  plot_title <- paste("Total Carbon - ", plot_title)
  if(!dir.exists(file.path("plots", project_name))){dir.create(file.path("plots", project_name))}
  ggsave(plot, filename = file.path("plots", project_name, paste0(plot_title, ".png")), height = 4.62, width = 5.98)
  
  
}


plot_monthly_c <- function(month = 1, 
                           time_horizon, 
                           df, 
                           plot_title = "", 
                           project_name = "test"){
  
  if(month == 1){th = time_horizon +1}else{th = time_horizon}
  
  df <- df %>% 
    mutate(months = c(rep(1:12, time_horizon), 1)) %>% 
    filter(months == month) %>% 
    rowwise() %>% 
    summarise(c_tot = sum(DPM, RPM, BIO, HUM, IOM), .groups = "drop") %>% 
    mutate(year = 1:th)
  
  plot <- ggplot(data = df, aes(x = year, y = c_tot))+
    geom_line()+
    labs(title = plot_title, x = "Time (years)", y = paste("C Stocks in Month", month,"(t/ha)"))+
    theme_classic()
  
  plot_title <- paste("Carbon month", month, "-", plot_title)
  if(!dir.exists(file.path("plots", project_name))){dir.create(file.path("plots", project_name))}
  ggsave(plot, filename = file.path("plots", project_name, paste0(plot_title, ".png")), height = 4.62, width = 5.98)
  
  
  
  
}


plot_monthly_histogram <- function(time_horizon, 
                                   df,
                                   plot_title = "", 
                                   project_name = "test"){
  
  df <- df %>% 
    rowwise() %>% 
    summarise(c_tot = sum(DPM, RPM, BIO, HUM, IOM), .groups = "drop") %>% 
    mutate(months = c(rep(1:12, time_horizon), 1))
  
  
  plot <- ggplot(data = df, aes(x = factor(months), y = c_tot))+ 
    geom_boxplot()+ 
    labs(title = plot_title, x = "Month", y = paste("Distribution within a month (t/ha)"))+
    theme_classic()
  
  
  plot_title <- paste("Monthly Distribution -", plot_title)
  if(!dir.exists(file.path("plots", project_name))){dir.create(file.path("plots", project_name))}
  ggsave(plot, filename = file.path("plots", project_name, paste0(plot_title, ".png")), height = 4.62, width = 5.98)
  
} 

plot_c_diff <- function(years, 
                        df,
                        plot_title = "", 
                        project_name = "test"){
  
  df <- df %>% 
    rowwise() %>% 
    summarise(c_tot = sum(DPM, RPM, BIO, HUM, IOM), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(c_tot_1 = lag(c_tot, 1),
           c_tot_diff = c_tot-c_tot_1, 
           year = years)
  
  
  
  plot <- ggplot(data = df, aes(x = year, y = c_tot_diff))+
    geom_line()+
    labs(title = plot_title, x = "Time (years)", y = "C difference (t/ha)")+
    theme_classic()
  
  plot_title <- paste("Difference to previous month - ", plot_title)
  if(!dir.exists(file.path("plots", project_name))){dir.create(file.path("plots", project_name))}
  ggsave(plot, filename = file.path("plots", project_name, paste0(plot_title, ".png")), height = 4.62, width = 5.98)
  
}

get_bare_profile <- function(field_parameters){
  
  # input_parameters should be a single line of the input_parameter file
  
  ip0 <- field_parameters[i,] %>% select(contains("bare_profile"))
  
  if(ncol(ip0) != 12){stop("Missing information about the bare profile months. ")}
  
  bare_profile <- as.data.frame(t(ip0))$V1
  
  return(bare_profile)    
}