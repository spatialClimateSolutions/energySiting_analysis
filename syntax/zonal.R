base_ras <- terra::rast("./processed_data/masks/US_base_raster.tif")
load("./derived/absence.RData") # w_ab_sf, wind_absence_roi,s_ab_sf, solar_absence_roi,

solar_stack <- stack("./derived/solar_covStack.tif")
wind_stack <- stack("./derived/wind_covStack.tif")
coeff_stack <- stack("./derived/covStack.tif")
coeff_future <- stack("./derived/future.tif")


solar_location_rast <- rast("./derived/solar_hull_existing.tif")
wind_location_rast <- rast("./derived/wind_hull_existing.tif")


## stack
names(coeff_stack) <- c("tx", "landAcq", "roads", "sub", "slope", "pop","rps",
                        "unemploy", "minority", "poverty", "lulc_forest", "lulc_grassland", 
                        "lulc_shrubland", "lulc_riparian", "lulc_sparse", 
                        "lulc_agriculture", "lulc_developed", "lulc_other",
                        "region_ne", "region_mw", "region_w", "region_s", "region_tex", 
                        "region_mtw")
names(coeff_future) <- c("tx", "sub")
names(solar_stack) <- names(wind_stack) <- c("env", "cf", "lag")

wind_IV <- stack(list(coeff_stack,wind_stack))
solar_IV <- stack(list(coeff_stack,solar_stack))
future_stack <- stack(list(coeff_stack[[c(-1,-4)]],coeff_future))
wind_future <- stack(list(future_stack,wind_stack))
solar_future <- stack(list(future_stack,solar_stack))


## DV
s_ex <- raster(solar_location_rast)
s_ab <- fasterize::fasterize(sf = s_ab_sf, 
                             raster = raster(base_ras), 
                             field = "id") # Rasterize points

## zonal statistics 
### existing projects 
solar.cov.bg <- as.data.frame(zonal(x = solar_IV, z = s_ab, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 0)
solar.cov.existing <- as.data.frame(zonal(x = solar_IV, z = s_ex, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 1)
write.csv(solar.cov.bg, "./derived/bg_cov_solar.csv", row.names = FALSE)
write.csv(solar.cov.existing, "./derived/existing_cov_solar.csv", row.names = FALSE)


## DV
w_ex <- raster(wind_location_rast)
w_ab <- fasterize::fasterize(sf = w_ab_sf, 
                             raster = raster(base_ras), 
                             field = "id") # Rasterize points

## zonal statistics 
### existing projects 
wind.cov.bg <- as.data.frame(zonal(x = wind_IV, z = w_ab, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 0)
wind.cov.existing <- as.data.frame(zonal(x = wind_IV, z = w_ex, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 1)
write.csv(wind.cov.bg, "./derived/bg_cov_wind.csv", row.names = FALSE)
write.csv(wind.cov.existing, "./derived/existing_cov_wind.csv", row.names = FALSE)



