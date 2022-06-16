
extract_weather_ranges <- function(weather_data){
  
  weather <- as.data.frame(sapply(weather_data, t.test, conf.level = 0.9))
  weather <- as.data.frame(lapply(weather, function (x) x[c('conf.int')][1]))
  mean_weather <- sapply(weather_data, mean, na.rm=T)
  
  lapply(weather, `[`, c('conf.int', "estimate"))
  
  weather <- t(weather)
  rownames(weather) <- 1:12
  
  colnames(weather) <- c("min_weather", "max_weather")
  
  all_weather <- as.data.frame(cbind(weather, mean_weather))
  
  
  return(all_weather)
  
}



calculate_confidence_interval <- function(df, confidence_interval = 0.9){
  
  
  if(length(df)< 30){
    conf = t.test(df, conf.level = confidence_interval)$conf.int[1:2]
  }else{
    stop("For more than 30 samples a z distribution should be implemented rather than the t test. This is a to do still!")
    
  }  
  
  return(conf)
}