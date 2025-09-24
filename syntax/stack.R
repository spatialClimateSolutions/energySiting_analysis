processed_layer_path <- "/home/energysiting/data/processed_data/variables"

#####################################
# plot(raster(file.path(processed_layer_path, "community.tif")))

### predictors 
cov.filePaths <- c(file.path(processed_layer_path, "transmission_110_qgis.tif"),
                   file.path(processed_layer_path, "land_acquisition.tif"),
                   file.path(processed_layer_path, "roads_qgis.tif"), 
                   file.path(processed_layer_path, "substations_110_qgis.tif"),
                   file.path(processed_layer_path, "slope.tif"),
                   file.path(processed_layer_path, "pop_density.tif"),
                   file.path(processed_layer_path, "rps.tif"),
                   
                   file.path(processed_layer_path, "svi_unemp.tif"),
                   file.path(processed_layer_path, "svi_minrty.tif"),
                   file.path(processed_layer_path, "svi_pov.tif"),

                   file.path(processed_layer_path, "lulc_forest.tif"),
                   file.path(processed_layer_path, "lulc_grassland.tif"),
                   file.path(processed_layer_path, "lulc_shrubland.tif"),
                   file.path(processed_layer_path, "lulc_riparian.tif"),
                   file.path(processed_layer_path, "lulc_sparselyvegetated.tif"),
                   file.path(processed_layer_path, "lulc_agricultural.tif"),
                   file.path(processed_layer_path, "lulc_developed.tif"),
                   file.path(processed_layer_path, "lulc_other.tif"),
                   
                   file.path(processed_layer_path, "region_ne.tif"),
                   file.path(processed_layer_path, "region_mw.tif"),
                   file.path(processed_layer_path, "region_w.tif"),
                   file.path(processed_layer_path, "region_s.tif"),
                   file.path(processed_layer_path, "region_tex.tif"),
                   file.path(processed_layer_path, "region_mtw.tif")
)


cov.names <- c("Transmission dist",
               "Land acquisition",
               "Road dist", 
               "Substation dist",
               "Slope",
               "Population density", 
               "RPS",
               
               "Unemployment",
               "Minority", 
               "Poverty", 
               
               "Forest", 
               "Grassland", 
               "Shrubland", 
               "Riparian", 
               "Vegetated", 
               "Agriculture", 
               "Developed", 
               "Ohter lands",
               
               "Northeast",
               "Midwest",
               "West",
               "South",
               "Texas",
               "Mtwest")

raster_list <- list()
for (i in 1:length(cov.names)) {
  if(i < 11){
    raster_list[[cov.names[[i]]]] <- raster(cov.filePaths[[i]]) %>% 
      raster::scale()
  }else{
    raster_list[[cov.names[[i]]]] <- raster(cov.filePaths[[i]]) 
  }
}

# Create a raster stack from the list of coefficient rasters
coeff_stack <- stack(raster_list)
raster::writeRaster(coeff_stack, "./derived/covStack.tif", 
                    overwrite=TRUE)



## solar
cov.tech <- c(file.path(processed_layer_path, "env_impactScore_solar.tif"),
              file.path(processed_layer_path, "solar_capacity.tif"),
              file.path(processed_layer_path, "solar_lag.tif"))

cov.names <- c("Environmental score",
               "Capacity factor",
               "Spatial lag")

raster_list <- list()
# Loop through each coefficient and convert to raster
for (i in 1:length(cov.names)) {
  # Add raster to the list
  raster_list[[cov.names[[i]]]] <- raster(cov.tech[[i]]) %>% 
    raster::scale()
}

# Create a raster stack from the list of coefficient rasters
solar_stack <- stack(raster_list)
raster::writeRaster(solar_stack, "./derived/solar_covStack.tif", 
                    overwrite=TRUE)


## wind
cov.tech <- c(file.path(processed_layer_path, "env_impactScore_wind.tif"),
                   file.path(processed_layer_path, "wind_capacity.tif"),
                   file.path(processed_layer_path, "wind_lag.tif"))

cov.names <- c("Environmental score",
               "Capacity factor",
               "Spatial lag")

raster_list <- list()
# Loop through each coefficient and convert to raster
for (i in 1:length(cov.names)) {
  # Add raster to the list
  raster_list[[cov.names[[i]]]] <- raster(cov.tech[[i]]) %>% 
    raster::scale()
}

# Create a raster stack from the list of coefficient rasters
wind_stack <- stack(raster_list)
raster::writeRaster(wind_stack, "./derived/wind_covStack.tif", 
                    overwrite=TRUE)


## future 
cov.tech <- c(file.path(processed_layer_path, "transmission_110_wPlanned.tif"),
              file.path(processed_layer_path, "substations_110_wPlanned.tif")
)

cov.names <- c( "Transmission dist",
                "Substation dist")

raster_list <- list()
# Loop through each coefficient and convert to raster
for (i in 1:length(cov.names)) {
  # Add raster to the list
  raster_list[[cov.names[[i]]]] <- raster(cov.tech[[i]]) %>% 
    raster::scale()
}

# Create a raster stack from the list of coefficient rasters
future_stack <- stack(raster_list)
raster::writeRaster(future_stack, "./derived/future.tif", 
                    overwrite=TRUE)
