if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

librarian::shelf("raster", "sf", "tidyverse", "terra", "here", "tictoc", "foreach", "doParallel", "foreign", "dplyr","tigris",
                 "stargazer", "caret", "tidycensus", "ggpubr", "tidyterra", "gridExtra", "rasterVis", "RColorBrewer", "grid",
                 "lme4", "lmerTest", "ggstance", "cowplot", "mapview")
sf_use_s2(FALSE)
library(ggnewscale)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

projection <- (raster::crs("+init=epsg:5070"))
state_boundaries <- st_read("../data/raw_data/us_state_outlines/tl_2018_us_state.shp") %>% #read state boundaries
  st_transform(crs = projection)

