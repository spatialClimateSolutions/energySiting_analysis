source("./syntax/function.R")

### data
load("./derived/absence.RData") # s_ab_sf, w_ab_sf
load("./derived/location.RData") # solar_locations, solar_locations,

# suitable areas
s_roi <- rast("./derived/solar_absence_roi.tif") 
w_roi <- rast("./derived/wind_absence_roi.tif")

# predictor analysis
load("./derived/model_results.RData") # imp, solar_resamps, wind_resamps, rst_s, rst_w
s_dat <- read_csv("./derived/solar.csv") # s_dat
w_dat <- read_csv("./derived/wind.csv")


# prediction
solar.pg.glm <- rast("./derived/pred_logReg_s.tif")
solar.pg.lasso <- rast("./derived/pred_lasso_s.tif")
solar.pg.rf <- rast("./derived/pred_randomForest_s.tif")
solar.pg.xg <- rast("./derived/pred_xg_s.tif")
wind.pg.glm <- rast("./derived/pred_logReg_w.tif")
wind.pg.lasso <- rast("./derived/pred_lasso_w.tif")
wind.pg.rf <- rast("./derived/pred_randomForest_w.tif")
wind.pg.xg <- rast("./derived/pred_xg_w.tif")
results <- list(solar.pg.glm, solar.pg.lasso, solar.pg.rf, solar.pg.xg,
                wind.pg.glm, wind.pg.lasso, wind.pg.rf, wind.pg.xg)

### mask prediction surface
results_masked <- list()

for(i in 1:4){
  results_masked[[i]] <- mask(results[[i]], s_roi)
}
for(i in 5:8){
  results_masked[[i]] <- mask(results[[i]], w_roi)
}

terra::writeRaster(rast(results_masked), "./derived/results_masked.tif", overwrite=TRUE)


### regions by DAC
load("./derived/region.RData") # rgn 

rgn <- rgn %>% 
  mutate(region = recode(region, 
                         "mtwest" = "Mtwest",
                         "midwest" = "Midwest",
                         "northeast" = "Northeast",
                         "south" = "South",
                         "texas" = "Texas",
                         "west" = "West"))

# change row order
rgn <- rgn[c(6,2,1,5,4,3),]

rgn_vect <- rgn %>% 
  mutate(id = 1:6) %>% 
  st_transform(crs = projection) %>% 
  terra::vect()

models <- c("GLM","Lasso","RF","XGBoost")

### mean probability by region
rgn_results <- data.frame()
for(j in 1:8){
  
  data <- results_masked[[j]]
  
  res <- numeric()
  for (i in 1:length(rgn_vect)) {
    cropped_raster <- terra::crop(data, rgn_vect[i])
    masked_raster <- mask(cropped_raster, rgn_vect[i])

    res[i] <-  mean(values(masked_raster), na.rm = T)
  }
  
  if(j %in% c(1:4)){
    tp <- rgn %>% 
      st_drop_geometry() %>% 
      mutate(mean = res,
             model = models[j],
             tech = "Solar")
  }else{
    tp <- rgn %>% 
      st_drop_geometry() %>% 
      mutate(mean = res,
             model = models[j-4],
             tech = "Wind")
  }
  
  rgn_results <- tp %>% 
    rbind(rgn_results)
}


### DAC
DAC <- read_csv("https://github.com/reconjohn/disadvantaged_communities/raw/main/results/DAC_s.csv") %>% 
  dplyr::select(GEOID, disadvantaged)

load("../../min25/R/adoption/data/census.RData") # tr.sf
tracts <- tr.sf

# DAC data
dac <- tracts %>% 
  rename(GEOID = FIPS,
         geometry = Shape) %>% 
  dplyr::select(GEOID) %>% 
  left_join(DAC, by = "GEOID") %>% 
  group_by(disadvantaged) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()


# DAC by regions
r_dac <- rgn %>% 
  st_transform(st_crs(dac)) %>% 
  st_intersection(dac) 


polygons_vect <- r_dac %>% 
  mutate(id = 1:12) %>% 
  st_transform(crs = projection) %>% 
  terra::vect()

dac_vect <- dac %>% 
  mutate(id = 1:2) %>% 
  st_transform(crs = projection) %>% 
  terra::vect()


### mean probability by DAC
dac_results <- data.frame()
for(j in 1:8){
  
  data <- results_masked[[j]]
  res <- numeric()
  
  for (i in 1:length(polygons_vect)) {
    cropped_raster <- terra::crop(data, polygons_vect[i])
    masked_raster <- mask(cropped_raster, polygons_vect[i])
    res[i] <-  mean(values(masked_raster), na.rm = T)
  }
  
  if(j %in% c(1:4)){
    tp <- r_dac %>% 
      st_drop_geometry() %>% 
      mutate(mean = res,
             model = models[j],
             tech = "Solar")
  }else{
    tp <- r_dac %>% 
      st_drop_geometry() %>% 
      mutate(mean = res,
             model = models[j-4],
             tech = "Wind")
  }
  
  dac_results <- tp %>% 
    rbind(dac_results)
}



library(nlme)
# 1. Project geometries up front (Ensure polygons_vect has 'region' and 'disadvantaged' columns)
polygons_vect_proj <- project(polygons_vect, crs(results_masked[[1]]))

# Identify unique regions
unique_regions <- unique(polygons_vect_proj$region)
final_results <- list() # Using a list is faster than rbind in a loop
pts_per_cat <- 1000 

# We sample the points ONCE for all 8 rasters to save time
region_points <- list()

for (reg in unique_regions) {
  # Get DAC and Non-DAC polygons for this specific region
  poly_dac <- polygons_vect_proj[polygons_vect_proj$region == reg & polygons_vect_proj$disadvantaged == TRUE]
  poly_nondac <- polygons_vect_proj[polygons_vect_proj$region == reg & polygons_vect_proj$disadvantaged == FALSE]
  
  # Sample points directly from the vector (much faster than masking rasters)
  set.seed(42)
  p_dac <- spatSample(poly_dac, size = pts_per_cat, method = "random")
  p_nondac <- spatSample(poly_nondac, size = pts_per_cat, method = "random")
  
  if(nrow(p_dac) > 0 && nrow(p_nondac) > 0) {
    p_dac$status <- "DAC"
    p_nondac$status <- "NonDAC"
    
    # Store coordinates and status
    combined_pts <- rbind(p_dac, p_nondac)
    region_points[[reg]] <- combined_pts
  }
}

# region_points$West %>% 
#   as.data.frame(geom = "XY") %>% 
#   ggplot() +
#   geom_point(aes(x = x, y = y, color = disadvantaged), size = 1.5, alpha = 0.8) +
#   scale_color_manual(
#     values = c("FALSE" = "steelblue", "TRUE" = "firebrick"),
#     labels = c("Non‑DAC", "DAC"),
#     name = "Disadvantaged"
#   ) +
#   coord_equal() +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Region: West — Disadvantaged vs Non‑Disadvantaged Points",
#     x = "", y = ""
#   )


library(purrr)
# Your raster with 8 layers
r <- results_masked   # SpatRaster (8 layers)
val_all <- data.frame()
for (reg in names(region_points)) {
  
  # If DAC info is in a SpatVector (e.g., region_points$West), rasterize it:
  dac_mask <- rasterize(region_points[[reg]], r, field = "disadvantaged")
  
  # Extract values from all layers + DAC mask
  vals <- c(r, dac_mask) |> 
    as.data.frame(na.rm = FALSE)
  
  # Rename DAC column
  names(vals)[ncol(vals)] <- "DAC"
  
  # Drop rows where DAC is NA
  vals <- vals |> filter(!is.na(DAC)) %>% 
    mutate(region = reg)
  
  val_all <- rbind(vals, val_all)
}


# Function to run t-test for one layer
run_ttest <- function(layer_name) {
  x <- val_all[[layer_name]]
  
  # Remove NA values
  df <- data.frame(value = x, DAC = val_all$DAC, region = val_all$region) |> 
    filter(!is.na(value))
  
  tb <- data.frame()
  for(reg in r_dac$region %>% unique()){
    
    df1 <- df %>% 
      filter(region == reg)
    
    df2 <- df1 %>% 
      group_by(DAC) %>%
      summarise(
        mean = mean(value, na.rm = TRUE),
        sd   = sd(value, na.rm = TRUE),
        n    = n(),
        se   = sd / sqrt(n)
      )
    
    
    # t-test
    tt <- t.test(value ~ DAC, data = df1)
    
    t <- tibble(
      layer = layer_name,
      mean_DAC = df2$mean[2],
      sd_DAC = df2$sd[2],
      se_DAC = df2$se[2],
      mean_nonDAC = df2$mean[1],
      sd_nonDAC = df2$sd[1],
      se_nonDAC = df2$se[1],
      
      t_stat = tt$statistic,
      p_value = tt$p.value,
      region = reg
    )
    
    tb <- rbind(tb, t)
    
  }
  return(tb)

}

# Apply to all 8 layers
layer_names <- names(r)
ttest_results <- map_dfr(layer_names, run_ttest) %>% 
  mutate(tech = if_else(grepl("_s$", layer), "Solar", "Wind"),
         model = if_else(str_detect(layer, "logReg"), "GLM", 
                         if_else(str_detect(layer, "lasso"), "Lasso", 
                                 if_else(str_detect(layer, "random"), "RF", "XGBoost")))) %>% 
  dplyr::select(-layer)


### DAC split mapping
dac_masked_s <- list()
rda <- results_masked[[1]]
for(i in 1:2){
  dac_masked_s[[i]] <- mask(rda, dac_vect[i])
}

dac_masked_w <- list()
rda <- results_masked[[5]]
for(i in 1:2){
  dac_masked_w[[i]] <- mask(rda, dac_vect[i])
  
}

dac_masked <- c(dac_masked_s, dac_masked_w)
terra::writeRaster(rast(dac_masked), "./derived/dac_masked.tif", overwrite=TRUE)


### slope
sol <- s_dat %>% # pop, tx, env
  mutate(region = names(s_dat[20:25])[max.col(s_dat[20:25])]) %>% # revert one-hot incode
  dplyr::select(-zone,-region_ne:-region_mtw)

win <- w_dat %>% # cf, tx, lag
  mutate(region = names(w_dat[20:25])[max.col(w_dat[20:25])]) %>% # revert one-hot incode
  dplyr::select(-zone,-region_ne:-region_mtw)


RE <- function(tech){
  if(tech == "solar"){
    da <- sol
    model1vars <- setdiff(names(da)[c(1:21)], c("pop","tx","env"))
    fvar <- as.formula(paste("treat ~", paste(model1vars, collapse = " + "), "+ (pop+tx+env|region)"))
  }else{
    da <- win
    model1vars <- setdiff(names(da)[c(1:21)], c("cf","tx","lag"))
    fvar <- as.formula(paste("treat ~", paste(model1vars, collapse = " + "), "+ (cf+tx+lag|region)"))
  }
  
  fit <- glmer(fvar, family = "binomial", data = da)
  # summary(fit)
  
  tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96
  
  d <- as.data.frame(ranef(fit)$region) %>% 
    tibble::rownames_to_column("region") %>% 
    mutate(region = recode(region, 
                           "region_mtw" = "Mtwest",
                           "region_mw" = "Midwest",
                           "region_ne" = "Northeast",
                           "region_s" = "South",
                           "region_tex" = "Texas",
                           "region_w" = "West")) %>% 
    dplyr::select(-`(Intercept)`) %>% 
    gather(variable, R_effect, -region) %>% 
    mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,])) 
  
  return(d)
}

s_d <- RE("solar") %>% 
  mutate(variable = factor(variable, levels = c("pop", "tx", "env"))) %>% 
  mutate(variable = recode(variable, "pop" = "Population density",
                           "tx" = "Transmission dist",
                           "env" = "Environmental score"))
w_d <- RE("wind") %>% 
  mutate(variable = factor(variable, levels = c("cf", "lag", "tx"))) %>% 
  mutate(variable = recode(variable, "cf" = "Capacity factor",
                           "tx" = "Transmission dist",
                           "lag" = "Spatial lag")) 

save(solar_locations, wind_locations, s_ab_sf, w_ab_sf, imp,solar_resamps, wind_resamps, rst_s, rst_w, 
     s_dat, w_dat, rgn_results, dac_results, s_d, w_d, state_boundaries,rgn,
     file = "./derived/total.RData")
