# carbon_inputs

# This script is an initial attempt to calculate estimated carbon inputs for different
# crop types based on some specific properties.
# This is not yet being used- nor is it clear that we need it, but it does convert yield
# to carbon input which seems to me to be relevant. 

# Grain corn
hi <- 0.5 # Harvest Index = proportion of the total carbon harvested
rs <- 0.18 # root shoot ratio
extra_root_c <- 0.65 
# carbon delivered to the soil during growing season 
# as root turnover and exudate, as a proportion of root biomass 

# TODO: put into table
# # Soybeans	
# hi <-	0.40
# rs <- 	0.19
# extra_root_c <- 0.65
# 
# # sorghum
# hi <- 0.44
# rs <- 0.24
# extra_root_c <- 0.65


# Input
wet_yield <- 7000 # kg / ha
moisture_content <- 0.2
dry_yield <- wet_yield * (1-moisture_content) # kg DM /ha (dry matter)
# or
# dry_yield <- 6000 # kg DM /ha

yield_bm <- (dry_yield*0.45)
above_ground_bm <- yield_bm *(1-hi)/hi
total_bm <- yield_bm + above_ground_bm
roots_bm <- total_bm * rs
extra_roots_bm <- roots_bm * extra_root_c


total_c_input <- above_ground_bm + roots_bm + extra_roots_bm
# kg C / ha

total_c_input_tc <- total_c_input /1000

