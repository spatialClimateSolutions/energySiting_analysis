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

models <- c("GLM","Lasso","RandomForest","XGBoost")

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

load("../../min25/R/Zillow/data/census.RData") # tr.sf
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
    model1vars <- names(da)[c(1:21)]
    fvar <- as.formula(paste("treat ~", paste(model1vars, collapse = " + "), "+ (pop+tx+env|region)"))
  }else{
    da <- win
    model1vars <- names(da)[c(1:21)]
    fvar <- as.formula(paste("treat ~", paste(model1vars, collapse = " + "), "+ (cf+tx+lag|region)"))
  }
  
  fit <- lmer(fvar, data = da)
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
