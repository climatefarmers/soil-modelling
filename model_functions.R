# SoilR functions



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
  description = "Base Case"
){
  
  # Monthly data frame
  years <- seq(1/12, time_horizon, by = 1/12)
  
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
  
  # Generates and saves output plot
  plot_c_stocks(years, c_t, description)
  
  write.csv(fW,paste0(description,"_fW.csv"))
  
  return(c_t)
  
}


get_total_C <- function(C_df){
  
  final_row <- as.numeric(tail(C_df, 1))
  
  tot_value <- sum(final_row)
  
  return(tot_value)
  
}


plot_c_stocks <- function(years, 
                          df,
                          plot_title){
  
  # df <- as.data.frame(df)
  # names(df) <- c("DPM", "RPM", "BIO", "HUM", "IOM")
  df$year <- years
  df <- df %>% 
    pivot_longer(cols = c("DPM", "RPM", "BIO", "HUM", "IOM"),
                 names_to = "soil_comp")
  
  plot <- ggplot(data = df, aes(x = year, y = value, group = soil_comp, colour = soil_comp))+
    geom_line()+
    labs(title = plot_title, x = "Time (years)", y = "C Stocks (Mg/ha)", color = "Soil\nComposition")+
    theme_classic()+
    theme(legend.position = "right")
  
  
  if(!dir.exists("plots")){dir.create("plots")}
  ggsave(plot, filename = paste0("plots/",plot_title, ".png"), height = 4.62, width = 5.98)
  
}


get_bare_profile <- function(input_parameters_0){
  
  # input_parameters should be a single line of the input_parameter file
  
  input_parameters_0 <- input_parameters[i,]
  
  ip0 <- input_parameters_0 %>% select(contains("bare_profile"))
  
  if(ncol(ip0) != 12){stop("Missing information about the bare profile months. ")}
  
  bare_profile <- as.data.frame(t(ip0))$V1
  
  return(bare_profile)    
}