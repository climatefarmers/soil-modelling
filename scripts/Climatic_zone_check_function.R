# Climatic zone check function

# Still to be done: Save map file in an appropriate folder (CFs folder)
# Automatize coordinate input format. Coordinates should be input as a table.
# That can easily be changed. 


# Read input data ----
library(sf)

clime.zone.check<-function(climatic_zone_loc, lon_farmer, lat_farmer){
  # Function to extract the Environmental zone at a given coordinate ----
  # Inputs are the local modelling-data repo folder path and longitude and latitude of the location considered. 
  clim.zone.map <- st_read(file.path(climatic_zone_loc,"EnSv8/ens_v8.shp"), stringsAsFactors=FALSE)
  enz.name<-c("Alpine north", "Boreal", "Nemoral", "Atlantic north", "Alpine south", "Continental", "Atlantic central", 
              "Pannonian", "Lusitanian", "Anatolian", "Mediterranean mountains", "Mediterranean north", "Mediterranean south")
  # The map requires transformations to prevent error messages. 
  clim.zone_ll <- sf::st_transform(clim.zone.map, crs = 3857) 
  clim.zone_ll <- sf::st_transform(clim.zone_ll, crs = 4326) 
  # points are transformed to the same projection
  coord <- data.frame(x=lon_farmer, y=lat_farmer)
  coords_t1 <- st_as_sf(coord, coords = c("x", "y"), crs= 4326)
  
  # st assumes planar, so one last transformation to planar
  coords_planar <-st_transform(coords_t1, 2163) 
  clim.zone_planar <- sf::st_transform(clim.zone_ll, crs = 2163) 
  
  EnZ.class <- st_join(coords_planar, clim.zone_planar)$EnZ 
  enz.name[EnZ.class] 
  
}


# Function to Overlap env zone with WoSIS ----


# Function to overlap env zone with Catch C ----



# input has to be a map, and a table with coordinates, with column names x and y. 
