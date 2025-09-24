base_ras <- terra::rast("./data/processed_data/masks/US_base_raster.tif")

# read in new data from the USGS database
wind_location_path <- '../data/raw_data/uswtdb_v7_2_20241120.geojson'
solar_location_path <- '../data/raw_data/uspvdb_v2_0_20240801.geojson'


# solar
solar_mask <- st_read(solar_location_path) %>% # read in the file
  filter(!p_state %in% c("HI","AK")) %>% 
  mutate(unique_id = row_number()) %>% 
  st_transform(crs = 5070) %>% # transform crs to ours
  st_buffer(dist = 500) # apply buffer calculated above
# plot(solar_mask$geometry)

solar_locations <- st_read(solar_location_path) %>% # read in the file
  st_transform(crs = 5070) %>% # reproject crs into projection of base raster
  filter(p_year >= 2017) %>% #projects after 2017
  filter(p_cap_ac >= 1) %>% 
  filter(!p_state %in% c("HI","AK")) %>% 
  mutate(unique_id = row_number()) %>% 
  rename(state_code = p_state) %>% 
  
  mutate(area_m2 = st_area(geometry),
         radius_m = sqrt(area_m2/pi) %>% 
           as.numeric()) 


# rasterize mask
solar_mask_rast <- solar_mask %>% # vectorize to rasterize
  terra::vect() %>%
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% 
  terra::mask(base_ras)
# plot(solar_mask_rast)

# rasterize locations
solar_location_rast <- solar_locations %>% 
  terra::vect() %>% # vectorize to rasterize
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% # any polygon that touches will be converted
  terra::mask(base_ras)


# save
writeRaster(x = solar_mask_rast,
            filename = "./derived/solar_hull_existing_mask.tif",
            overwrite = TRUE)

writeRaster(x = solar_location_rast,
            filename = "./derived/solar_hull_existing.tif",
            overwrite = TRUE)


write_csv(solar_locations, path = "./derived/solar_hull_existing.csv",
          append = FALSE)

# # append = FALSE allows this command to overwrite previous results
# st_write(solar_locations %>% 
#            mutate(point = st_centroid(geometry)),
#          "./derived/solar_hull_existing_pts.shp",
#          append = FALSE)




# wind
wind_mask <- st_read(wind_location_path) %>% # read in the file
  filter(!t_state %in% c("HI","AK")) %>% 
  mutate(unique_id = row_number()) %>% 
  st_transform(crs = 5070) %>% # transform crs to ours
  st_buffer(dist = 500) # apply buffer calculated above
# plot(wind_mask$geometry)


wind_locations <- st_read(wind_location_path) %>% # read in the file
  st_transform(crs = 5070) %>% # reproject crs into projection of base raster
  # st_crop(y = aoi_state) %>% # crop the data to the aoi
  filter(p_year >= 2017) %>% #projects after 2017
  rename(state_code = t_state) %>% 
  group_by(p_name, 
           state_code) %>% 
  summarise(group_size = n(), # make count column of points
            p_cap = sum(t_cap, na.rm = T)) %>% 
  ungroup() %>% 
  
  filter(p_cap >= 10) %>% 
  st_buffer(dist = 500,
            endCapStyle = 'ROUND')  %>% # Buffer points by 500m for locations
  group_by(p_name, # grab all of the other data for each p_name
           state_code,
           group_size,
           p_cap) %>% 
  summarise(geometry = st_convex_hull(st_union(geometry))) %>% # make a convex hull of each p_name geometry column
  ungroup() %>% 
  filter(state_code != "NA") %>% 
  filter(!state_code %in% c("HI","AK")) %>% 
  mutate(unique_id = row_number()) %>% 
  
  mutate(area_m2 = st_area(geometry),
         radius_m = sqrt(area_m2/pi) %>% 
           as.numeric()) 


# rasterize mask
wind_mask_rast <- wind_mask %>% # vectorize to rasterize
  terra::vect() %>%
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% 
  terra::mask(base_ras)
# plot(wind_mask_rast)

# rasterize locations
wind_location_rast <- wind_locations %>% 
  terra::vect() %>% # vectorize to rasterize
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% # any polygon that touches will be converted
  terra::mask(base_ras)




# save
writeRaster(x = wind_mask_rast,
            filename = "./derived/wind_hull_existing_mask.tif",
            overwrite = TRUE)

writeRaster(x = wind_location_rast,
            filename = "./derived/wind_hull_existing.tif",
            overwrite = TRUE)


write_csv(wind_locations, path = "./derived/wind_hull_existing.csv",
          append = FALSE)

# # append = FALSE allows this command to overwrite previous results
# st_write(wind_locations %>% 
#            mutate(point = st_centroid(geometry)),
#          "./derived/wind_hull_existing_pts.shp",
#          append = FALSE)


save(solar_locations,
     wind_locations,
     file = "./derived/location.RData")


