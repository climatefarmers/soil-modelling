# plots


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
