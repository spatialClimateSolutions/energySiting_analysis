projection <- (raster::crs("+init=epsg:5070"))
state_boundaries <- st_read("../data/raw_data/us_state_outlines/tl_2018_us_state.shp") %>% #read state boundaries
  st_transform(crs = projection)

# plot(state_boundaries$geometry)

# Define the function
randomPts <- function(raster_mask, folder, seeds, tech, location_csv){
  
  ### absent points
  Pts <- dim(location_csv)[1] # bgPt.multiplier
  
  state_proportions <- location_csv %>% # Record proportions of total plants for each state
    filter(!state_code == 'HI') %>% 
    group_by(state_code) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n / sum(n)) %>%
    select(-n) 
  
  state_proportions$count <- round(state_proportions$freq*Pts,0) # Calculate counts per state based on total points needed
  
  state_codes <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID', 'IL', 'IN',
                   'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT',
                   'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA',
                   'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
  
  # Add two points for each state that wasn't accounted for initially
  for (state in setdiff(state_codes, state_proportions$state_code)){
    state_proportions <- state_proportions %>% add_row(state_code = state, freq = 0, count = 10)
  }
  
  # Create radii for each point, and set standard radius for states that previously had no projects
  radii <- location_csv$radius_m
  # mean(radii)
  
  radii <- append(radii, rep(mean(radii), (sum(state_proportions$count) - length(radii)))) 
  
  absence_locations <- data.frame() # prepare a dataframe for sampled points
  for (seed in 1:length(seeds)){ # Iterate through each seed
    
    set.seed(seeds[seed]) # Set seed for current iteration
    for (state in state_proportions$state_code){ # Iterate through each state we want to sample from
      current_boundary <- state_boundaries[state_boundaries$STUSPS == state, ] # Set current state boundary
      raster_data <- terra::crop(raster_mask, current_boundary, mask= T) # clip the current_boundary from raster_master
      raster_data <- mask(raster_data, current_boundary) # remove outsiders from current_boundary
      cell_indices <- which(raster_data[] == 1)
      coordinates <- raster::xyFromCell(raster_data, 
                                        sample(cell_indices, 
                                               state_proportions[state_proportions$state_code == state, ]$count)) %>% 
        as.data.frame()
      
      absence_locations <- coordinates %>% 
        mutate(iteration = seed) %>% 
        rbind(absence_locations)
    }
  }
  
  absence_buff <- st_as_sf(absence_locations, coords = c('x', 'y'), crs = projection) %>%  # Create geometry for sampled lat/longs
    mutate(id = seq(1,nrow(absence_locations),1)) %>%
    st_buffer(sample(radii, nrow(absence_locations), replace = T)) # Buffer points
  
  return(absence_buff)
}

# read in the data
## solar
### suitable areas
solar_mask_rast <- terra::rast("./derived/solar_hull_existing_mask.tif")
# plot(solar_mask_rast)
solar_suit <- terra::rast("../data/processed_data/masks/solar_suitability_SL1.tif")
# plot(solar_suit)

solar_location_csv <- read_csv(file = "./derived/solar_hull_existing.csv")

# masking
solar_absence_roi <- solar_suit %>% 
  terra::mask(mask = solar_mask_rast, inverse = TRUE) %>% # we use this to remove the current solar locations from possible absence point locations
  terra::project(projection) %>% 
  raster()
# plot(solar_absence_roi)

# save, suitable area after removing suitability criteria and existing solar project buffer areas
writeRaster(x = solar_absence_roi,
            filename = "./data/derived/solar_absence_roi.tif",
            overwrite = TRUE)

solar_absence_roi <- raster("./data/derived/solar_absence_roi.tif")

s_ab_sf <- randomPts(raster_mask = solar_absence_roi,
                     seeds = c(1:10), tech = "solar", location_csv = solar_location_csv) # Can add more seeds to list to get more sampling iterations


## wind
### suitable areas
wind_mask_rast <- terra::rast("./derived/wind_hull_existing_mask.tif")
# plot(wind_mask_rast)
wind_suit <- terra::rast("../data/processed_data/masks/wind_suitability_SL1.tif")
# plot(wind_suit)

wind_location_csv <- read_csv(file = "./derived/wind_hull_existing.csv")

# masking
wind_absence_roi <- wind_suit %>% 
  terra::mask(mask = wind_mask_rast, inverse = TRUE) %>% # we use this to remove the current wind locations from possible absence point locations
  terra::project(projection) %>% 
  raster()
# plot(wind_absence_roi)

# save, suitable area after removing suitability criteria and existing wind project buffer areas
writeRaster(x = wind_absence_roi,
            filename = "./derived/wind_absence_roi.tif",
            overwrite = TRUE)

wind_absence_roi <- raster("./derived/wind_absence_roi.tif")


w_ab_sf <- randomPts(raster_mask = wind_absence_roi,
                   seeds = c(1:10), tech = "wind", location_csv = wind_location_csv) # Can add more seeds to list to get more sampling iterations


save(s_ab_sf, solar_absence_roi,
     w_ab_sf, wind_absence_roi,
     file = "./derived/absence.RData")
load("./derived/absence.RData")
