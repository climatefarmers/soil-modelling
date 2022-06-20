# Climatic zone check function

# Still to be done: Save map file in an appropriate folder (CFs folder)
# Automatize coordinate input format. Coordinates should be input as a table.
# That can easily be changed. 


# Read input data ----
library(sf)

clim.zone <- st_read("G:/Shared drives/Climate Farmers/07_Tech/Modelling/Climatic_zone/EnSv8/ens_v8.shp", stringsAsFactors=FALSE)
enz.name<-c("Alpine north", "Boreal", "Nemoral", "Atlantic north", "Alpine south", "Continental", "Atlantic central", "Pannonian", "Lusitanian", "Anatolian", "Mediterranean mountains", "Mediterranean north", "Mediterranean south")

# Function to extract the Environmental zone at a given coordinate ----
# input has to be a map, and a table with coordinates, with column names x and y. 

clime.zone.check<-function(clim.zone.map, coord, enz.name){
  # The map requires transformations to prevent error messages. 
  clim.zone_ll <- sf::st_transform(clim.zone.map, crs = 3857) 
  clim.zone_ll <- sf::st_transform(clim.zone_ll, crs = 4326) 
  # points are transformed to the same projection
  coords_t1 <- st_as_sf(coord, coords = c("x", "y"), crs= 4326)
  
  # st assumes planar, so one last transformation to planar
  coords_planar <-st_transform(coords_t1, 2163) 
  clim.zone_planar <- sf::st_transform(clim.zone_ll, crs = 2163) 
  
  EnZ.class <- st_join(coords_planar, clim.zone_planar)$EnZ 
  enz.name[EnZ.class] 
  
}


# Run function ----
coords <- data.frame(x=-7.332416805518278, y=39.822072494111474)
clime.zone.check(clim.zone, coords)


# Function to Overlap env zone with WoSIS ----


# Function to overlap env zone with Catch C ----



# input has to be a map, and a table with coordinates, with column names x and y. 
