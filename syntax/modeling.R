## modeling 
#### solar
solar.cov.existing <- read_csv("./derived/existing_cov_solar.csv") 
s.bg <- read_csv("./derived/bg_cov_solar.csv") 
solar.cov.bg <- s.bg %>% 
  mutate(group = c(rep(seq(1,10,1), each = floor(nrow(s.bg)/10)),rep(10,8)))

v_name <- c("(Intercept)",
            "Transmission dist",
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
            # "Ohter lands",
            
            "Northeast",
            "Midwest",
            "West",
            "South",
            "Texas",
            # "Mtwest",
            
            "Environmental score",
            "Capacity factor",
            "Spatial lag")


### combined
rst_s <- data.frame()
aic <- c()
for(i in 1:10){
  s_dat <- solar.cov.bg %>% 
    filter(group == i) %>% 
    dplyr::select(-group) %>% 
    rbind(solar.cov.existing) %>% 
    mutate(treat = factor(treat)) %>% 
    na.omit()
  
  glm1_solar <- glm(treat ~ tx + landAcq + roads + sub + slope + pop + rps + unemploy + minority + poverty + 
                      lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                      region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                      env + cf + lag, 
                    data=s_dat, family = binomial(link="logit"))
  
  tp <- summary(glm1_solar)$coefficients %>% 
    as.data.frame() %>% 
    mutate(var = v_name) %>% 
    mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"z value",-"Pr(>|z|)") %>% 
    # tibble::rownames_to_column("var") %>% 
    filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate) %>% 
    mutate(Group = i) 
  
  rst_s <- tp %>% 
    rbind(rst_s)
  
  aic[i] <- glm1_solar$aic
  
}


s_dat <- solar.cov.bg %>% 
  filter(group == which(aic == sort(aic)[5])) %>% 
  dplyr::select(-group) %>% 
  rbind(solar.cov.existing) %>% 
  mutate(treat = factor(treat)) %>% 
  na.omit()

write.csv(s_dat, "./derived/solar.csv", row.names = FALSE)


glm1_solar <- glm(treat ~ tx + landAcq + roads + sub + slope + pop + rps + unemploy + minority + poverty + 
                    lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                    region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                    env + cf + lag, 
                  data=s_dat, family = binomial(link="logit"))


solar.pg <- predict(object = solar_future, model = glm1_solar, type = "response", ext=extent(solar_future))
writeRaster(solar.pg, "./derived/pred_logReg_s.tif", overwrite=TRUE)


### ML
tc_kfold <- trainControl(method = "cv", number = 10, returnResamp="all",
                         savePredictions = TRUE, classProbs = TRUE,
                         summaryFunction = twoClassSummary)

region_model <- make.names(treat) ~ tx + landAcq + roads + sub + slope + pop + rps + unemploy + minority + poverty + 
  lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
  region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
  env + cf + lag


# ### GLM
mdl_solarglm <- train(form = region_model, data=s_dat, method = "glm",
                      trControl = tc_kfold,
                      na.action=na.omit,
                      family = 'binomial')

varimp_solarglm <- varImp(mdl_solarglm)
# solar.pg.glm <- predict(object = solar_future, mdl_solarglm, type = "prob", ext=extent(solar_future), progress='text', index=2)
# writeRaster(solar.pg.glm, "./derived/pred_logReg_ML_s.tif", overwrite=TRUE)


### lasso
lambda <- 10^seq(-3, 3, length = 100)
mdl_solarlasso <- train(form = region_model, data=s_dat, method = "glmnet",
                        trControl = tc_kfold,
                        tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                        na.action=na.omit)

varimp_solarlasso <- varImp(mdl_solarlasso)
solar.pg.lasso <- predict(object = solar_future, mdl_solarlasso, type = "prob", ext=extent(solar_future), progress='text', index=2)
writeRaster(solar.pg.lasso, "./derived/pred_lasso_s.tif", overwrite=TRUE)

#### xgbTree
library(xgboost)
tune_grid <- expand.grid(
  nrounds = c(50),       # Number of boosting iterations
  max_depth = c(3, 6),           # Maximum depth of trees
  eta = c(0.01, 0.1),         # Learning rate
  gamma = c(1),               # Minimum loss reduction required
  colsample_bytree = c(0.6),# Fraction of features used per tree
  min_child_weight = c(1, 5),
  subsample = c(0.6)        # Fraction of samples used per tree
)

mdl_solarxg = train(form = region_model, data=s_dat,
                    method="xgbTree", trControl = tc_kfold,
                    tuneGrid = tune_grid,
                    na.action = na.omit)

varimp_solarxg <- varImp(mdl_solarxg)
solar.pg.xg <- predict(object = solar_future, mdl_solarxg, type = "prob", ext=extent(solar_future), progress='text', index=2)
writeRaster(solar.pg.xg, "./derived/pred_xg_s.tif", overwrite=TRUE)  


#### RF
mdl_solarrf = train(form = region_model, data=s_dat,
                    metric = "ROC",
                    method = "rf", trControl = tc_kfold,
                    na.action = na.omit,
                    importance = TRUE)

varimp_solarrf <- varImp(mdl_solarrf)
solar.pg.rf <- predict(object = solar_future, mdl_solarrf, type = "prob", ext=extent(solar_future), progress='text', index=2)
writeRaster(solar.pg.rf, "./derived/pred_randomForest_s.tif", overwrite=TRUE)


### summary
solarmodel_list <- list(
  glm = mdl_solarglm,
  lasso = mdl_solarlasso,
  xg = mdl_solarxg,
  rf = mdl_solarrf
)

solar_resamps <- resamples(solarmodel_list)


### wind
wind.cov.existing <- read_csv("./derived/existing_cov_wind.csv") 
w.bg <- read_csv("./derived/bg_cov_wind.csv") 
wind.cov.bg <- w.bg %>% 
  mutate(group = c(rep(seq(1,10,1), each = floor(nrow(w.bg)/10)),rep(10,0)))


### combined
rst_w <- data.frame()
aic <- c()
for(i in 1:10){
  w_dat <- wind.cov.bg %>% 
    filter(group == i) %>% 
    dplyr::select(-group) %>% 
    rbind(wind.cov.existing) %>% 
    mutate(treat = factor(treat)) %>% 
    na.omit()
  
  glm1_wind <- glm(treat ~ tx + landAcq + roads + sub + slope + pop + rps + unemploy + minority + poverty + 
                      lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                      region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                      env + cf + lag, 
                    data=w_dat, family = binomial(link="logit"))
  
  tp <- summary(glm1_wind)$coefficients %>% 
    as.data.frame() %>% 
    mutate(var = v_name) %>% 
    mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"z value",-"Pr(>|z|)") %>% 
    # tibble::rownames_to_column("var") %>% 
    filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate) %>% 
    mutate(Group = i) 
  
  rst_w <- tp %>% 
    rbind(rst_w)
  
  aic[i] <- glm1_wind$aic
  
}


w_dat <- wind.cov.bg %>% 
  filter(group == which(aic == sort(aic)[5])) %>% 
  dplyr::select(-group) %>% 
  rbind(wind.cov.existing) %>% 
  mutate(treat = factor(treat)) %>% 
  na.omit()

write.csv(w_dat, "./derived/wind.csv", row.names = FALSE)


glm1_wind <- glm(treat ~ tx + landAcq + roads + sub + slope + pop + rps + unemploy + minority + poverty + 
                    lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                    region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                    env + cf + lag, 
                  data=w_dat, family = binomial(link="logit"))


wind.pg <- predict(object = wind_future, model = glm1_wind, type = "response", ext=extent(wind_future))
writeRaster(wind.pg, "./derived/pred_logReg_w.tif", overwrite=TRUE)


# ### GLM
mdl_windglm <- train(form = region_model, data=w_dat, method = "glm",
                      trControl = tc_kfold,
                      na.action=na.omit,
                      family = 'binomial')

varimp_windglm <- varImp(mdl_windglm)
# wind.pg.glm <- predict(object = wind_future, mdl_windglm, type = "prob", ext=extent(wind_future), progress='text', index=2)
# writeRaster(wind.pg.glm, "./derived/pred_logReg_ML_s.tif", overwrite=TRUE)


### lasso
lambda <- 10^seq(-3, 3, length = 100)
mdl_windlasso <- train(form = region_model, data=w_dat, method = "glmnet",
                        trControl = tc_kfold,
                        tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                        na.action=na.omit)

varimp_windlasso <- varImp(mdl_windlasso)
wind.pg.lasso <- predict(object = wind_future, mdl_windlasso, type = "prob", ext=extent(wind_future), progress='text', index=2)
writeRaster(wind.pg.lasso, "./derived/pred_lasso_w.tif", overwrite=TRUE)

#### xgbTree
library(xgboost)
tune_grid <- expand.grid(
  nrounds = c(50),       # Number of boosting iterations
  max_depth = c(3, 6),           # Maximum depth of trees
  eta = c(0.01, 0.1),         # Learning rate
  gamma = c(1),               # Minimum loss reduction required
  colsample_bytree = c(0.6),# Fraction of features used per tree
  min_child_weight = c(1, 5),
  subsample = c(0.6)        # Fraction of samples used per tree
)

mdl_windxg = train(form = region_model, data=w_dat,
                    method="xgbTree", trControl = tc_kfold,
                    tuneGrid = tune_grid,
                    na.action = na.omit)

varimp_windxg <- varImp(mdl_windxg)
wind.pg.xg <- predict(object = wind_future, mdl_windxg, type = "prob", ext=extent(wind_future), progress='text', index=2)
writeRaster(wind.pg.xg, "./derived/pred_xg_w.tif", overwrite=TRUE)  


#### RF
mdl_windrf = train(form = region_model, data=w_dat,
                    metric = "ROC",
                    method = "rf", trControl = tc_kfold,
                    na.action = na.omit,
                    importance = TRUE)

varimp_windrf <- varImp(mdl_windrf)
wind.pg.rf <- predict(object = wind_future, mdl_windrf, type = "prob", ext=extent(wind_future), progress='text', index=2)
writeRaster(wind.pg.rf, "./derived/pred_randomForest_w.tif", overwrite=TRUE)


### summary
windmodel_list <- list(
  glm = mdl_windglm,
  lasso = mdl_windlasso,
  xg = mdl_windxg,
  rf = mdl_windrf
)

wind_resamps <- resamples(windmodel_list)



imp <- rbind(
  varimp_windrf[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    dplyr::select(-X0) %>% 
    mutate(model = "RF",
           tech = "Wind") %>% 
    rename(Overall = X1),
  varimp_windxg[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(model = "XGBoost",
           tech = "Wind"),
  varimp_windlasso[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(model = "Lasso",
           tech = "Wind"),
  varimp_windglm[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(model = "GLM",
           tech = "Wind"),
  
  varimp_solarrf[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    dplyr::select(-X0) %>% 
    mutate(model = "RF",
           tech = "Solar") %>% 
    rename(Overall = X1),
  varimp_solarxg[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(model = "XGBoost",
           tech = "Solar"),
  varimp_solarlasso[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(model = "Lasso",
           tech = "Solar"),
  varimp_solarglm[[1]] %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(model = "GLM",
           tech = "Solar")
) %>% 
  mutate(rowname = recode(
    rowname,
    "tx" = "Transmission dist",
    "landAcq" = "Land acquisition",
    "roads" = "Road dist",
    "sub" = "Substation dist",
    "slope" = "Slope",
    "pop" = "Population density",
    "rps" = "RPS target",
    "unemploy" = "Unemployment",
    "minority" = "Minority",
    "poverty" = "Poverty",
    "lulc_forest" = "Forest",
    "lulc_grassland" = "Grassland",
    "lulc_shrubland" = "Shrubland",
    "lulc_riparian" = "Riparian",
    "lulc_sparse" = "Sparsely vegetated",
    "lulc_agriculture" = "Agriculture",
    "lulc_developed" = "Developed",
    "lulc_other" = "Otherland",
    "region_ne" = "Northeast",
    "region_mw" = "Midwest",
    "region_w" = "West",
    "region_s" = "South",
    "region_tex" = "Texas",
    "region_mtw" = "Mountain West",
    "env" = "Environmental score",
    "cf" = "Capacity factor",
    "lag" = "Spatial lag"
  ))

save(imp, solar_resamps, wind_resamps, rst_s, rst_w, file = "./derived/model_results.RData")
load("./derived/model_results.RData")
