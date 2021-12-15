# Running the model

# This file allows you to test multiple soil configurations based on input ranges
# defined directly in the script. The results and plots will be created
# in folders. The loop recreates the starting soil carbon content for each run. 

# To run: 
# 1. Ensure your working directory is the same as the repository. 
# 2. Check model_functions.R and modified_functions.R scripts path.
# 3. Run the code and review the results. Results are saved within the "results and plots"graphs/" folder 


# TODO: 
# - Ensure that input ranges fits the possibles to encountered from conventional to regAg


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr, pracma, jsonlite, kableExtra)

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))

## CODING TEST

#j <- array(c( rep(c(1:3), each = 1, times = ncol(uncertainty_list)) ), dim = c(3,nrow(uncertainty_list)))


min=c(0,1,4/12,10,1,1,10,3,0.75,1)
mean=c(15,1.44,9/12,15,25,20,30,20,0.75,1.07)
max=c(60,2,12/12,20,100,70,50,55,1,1.2)

colnames_ranges=c("field_carbon_in","dr_ratios","perc_cover","temp","precip","evap","soil_thick","clay","pE","tilling_factor")

n=15 #number of values tested within the range of possibles for each variable

for (i in c(1:length(colnames_ranges))){
  if (i==1){
    field_carbon_in <- c(mean[i],linspace(min[i],max[i],n))
    uncertainty_list=data.frame(field_carbon_in)
  } else {uncertainty_list[colnames_ranges[i]]<-c(mean[i],linspace(min[i],max[i],n))}
}

starting_soil_content <- estimate_starting_soil_content(SOC=50,clay=uncertainty_list$clay[1])
time_horizon = 500
C0_df <- calc_carbon_over_time(time_horizon,
                               field_carbon_in = rep(uncertainty_list$field_carbon_in[1],time_horizon),
                               dr_ratios = rep(uncertainty_list$dr_ratios[1],time_horizon),
                               bare = as.factor(c(logical(round(12*uncertainty_list$perc_cover[1])),!logical(round(12*(1-uncertainty_list$perc_cover[1]))))),
                               temp = rep(uncertainty_list$temp[1],12),
                               precip = rep(uncertainty_list$precip[1],12),
                               evap = rep(uncertainty_list$evap[1],12),
                               soil_thick = uncertainty_list$soil_thick[1],
                               clay = uncertainty_list$clay[1],
                               pE = uncertainty_list$pE[1],
                               PS = starting_soil_content,
                               tilling_factor = uncertainty_list$tilling_factor[1])
print(tail(C0_df,1))

starting_soil_content <- as.numeric(tail(C0_df, 1))[1:5]
time_horizon = 10
for (i in colnames_ranges){
  indices = data.frame(array(rep(1, times = ncol(uncertainty_list)), c(1,ncol(uncertainty_list))))
  colnames(indices) <- colnames_ranges
  if (i==colnames_ranges[1]){
    all_results <- data.frame(uncertainty_list[i])
  } else {all_results[i] <- uncertainty_list[i]}
  mean_all_c=c()
  for (k in c(0:n+1)){
    indices[i]= k
    # Set parameters
    
    all_c <- calc_carbon_over_time(time_horizon,
                                   field_carbon_in = rep(uncertainty_list$field_carbon_in[indices$field_carbon_in],time_horizon),
                                   dr_ratios = rep(uncertainty_list$dr_ratios[indices$dr_ratios],time_horizon),
                                   bare = c(logical(round(12*uncertainty_list$perc_cover[indices$perc_cover])),!logical(round(12*(1-uncertainty_list$perc_cover[indices$perc_cover])))),
                                   temp = rep(uncertainty_list$temp[indices$temp],12),
                                   precip = rep(uncertainty_list$precip[indices$precip],12),
                                   evap = rep(uncertainty_list$evap[indices$evap],12),
                                   soil_thick = uncertainty_list$soil_thick[indices$soil_thick],
                                   clay = uncertainty_list$clay[indices$clay],
                                   pE = uncertainty_list$pE[indices$pE],
                                   PS = starting_soil_content,
                                   tilling_factor = uncertainty_list$tilling_factor[indices$tilling_factor])
    # 
    # all_c <- calc_carbon_over_time(time_horizon,
    #                                PS = starting_soil_content,
    #                                field_carbon_in = uncertainty_list$field_carbon_in[indices$field_carbon_in],
    #                                dr_ratios = uncertainty_list$dr_ratios[indices$dr_ratios],
    #                                bare_profile = uncertainty_list$bare_profile[indices$bare_profile],
    #                                temp = uncertainty_list$temp[indices$temp],
    #                                precip = uncertainty_list$precip[indices$precip],
    #                                evap = uncertainty_list$evap[indices$evap],
    #                                soil_thick = uncertainty_list$soil_thick[indices$soil_thick],
    #                                clay = uncertainty_list$clay[indices$clay],
    #                                pE = uncertainty_list$clay[indices$pE],
    #                                tilling_factor = uncertainty_list$tilling_factor[indices$tilling_factor])
    mean_all_c <- append(mean_all_c, mean(all_c$TOT))
  }
  all_results[paste("C_response_to",i,sep='_')] <- mean_all_c

  name<-paste(paste("Sensitivity_to",i,sep="_"),".png",sep="")
  graph <- ggplot(all_results, aes(x = .data[[i]], y = .data[[paste("C_response_to",i,sep='_')]])) + 
    geom_line()+ 
    theme_classic()+
    theme(legend.position = "none")+
    labs(title = paste("C_response_to",i,sep='_'))
  print(graph)
  png(paste("graphs/",name,sep=""))
  print(graph)
  dev.off()
}

