# =============================== Packages ===============================================================================
library(FNN)
library(corrr)
library(tidyverse)
library(geosphere)
library(goftest)
library(pals)
library(raster)
library(FNN)
library(corrr)
library(tidyverse)
library(geosphere)
library(goftest)

library(SpatialExtremes)

library(patchwork)
# library(magrittr)
# # options(rgl.useNULL = TRUE)
# library(stringr)
# library(rgl)
library(boot)  # envelope(), for confidence intervals for qqplots
library(evgam)

library(rayshader)  # raster_to_matrix(), 
# library(magick)
# library(ggplot2)
library(sf)  # st_bbox(), 
# require(maps)
# require(raster)
# require(marmap)
# library(leaflet)
library(geojsonio)  # geojson_sf(), 
# library(sp)
library(scales)  # rescale(), 
library(plotly)  # theme(), 
library(shinythemes) # shinytheme(), 

library(forecast) # fourier(), 

library(ismev) # gev.fit(), 

library(lubridate) # ymd(), 
library(xts)
library(viridis)
# 

library(ggrepel)


library(cowplot)
library(gridExtra)

library(evd)

# library(shiny)
# library(shinyjs)
# library(shinythemes)
# library(shinyWidgets)
# library(shinyBS)
# library(shinyalert)
# library(shinydashboard)
# library(dashboardthemes)
# library(shinydashboardPlus)
# library(shinycssloaders)
# library(waiter)
# library(rintrojs)
# library(shinydisconnect)

# library(xtable)
# 
# library(ggmap)
# library(leaflet.extras)

library(tidyverse)
library(data.table)

# =============================== Packages ==============================================================================
using<-function(...) {
  # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages could not be found: ", libsmsg, collapse="")
    print(libsmsg)
    if(readline(prompt="Install missing packages? (YES/NO)") == "YES"){
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
}
using("raster", "sf", "secr", "geojsonio", "data.table")

# =============================== DATA ==================================================================================
setwd("/Users/Antoine/Desktop/Master Thesis Extremes Costa Rica ")

# ------------------------------------------- 1) Map of Costa Rica -------------------------------------------
border_costa_rica <- raster::getData('GADM', country='Costa Rica', level=0)
# if it is not working: border_costa_rica <- readRDS("data/borders/border_costa_rica_level_0.rds")
pt.lim = data.frame(xlim=c(-86, -82.5), ylim=c(8, 11.2))

extractCoords <- function(sp.df){
  results <- list()
  for(i in 1:length(sp.df@polygons[[1]]@Polygons)){ results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords }
  return(Reduce(rbind, results))
}
path_border <- extractCoords(raster::crop(border_costa_rica, as(raster::extent(pt.lim$xlim[1], pt.lim$xlim[2], pt.lim$ylim[1], pt.lim$ylim[2]), 'SpatialPolygons')))

# we will focus only on Costa Rica's territories that are located between lat. [8, 11.2] and long. [-86, -82.5]:
limits_map <- sf::st_bbox(c(xmin=pt.lim$xlim[1],
                        xmax=pt.lim$xlim[2],
                        ymin=pt.lim$ylim[1],
                        ymax=pt.lim$ylim[2]))

# we then crop the map, after converting it to sf object:
border_costa_rica <- sf::st_as_sf(border_costa_rica) %>%      # Make it sf to facilitate and cut
  st_crop(limits_map)


provinces_costa_rica <- raster::getData('GADM', country='Costa Rica', level=1)
# if it is not working: provinces_costa_rica <- readRDS("data/borders/border_costa_rica_level_1.rds")

# plot(provinces_costa_rica[provinces_costa_rica$NAME_1 == "Alajuela",])
provinces_costa_rica <- sf::st_as_sf(provinces_costa_rica) %>%      # Make it sf to facilitate and cut
  st_crop(limits_map)



# ------------------------------------------- 2) relief and barymetric data -------------------------------------------
# we use data from GEBCO: https://download.gebco.net
costa_rica_tif <- raster::raster("data/relief/costa_rica_low_res.tif")

# https://www.eorc.jaxa.jp/ALOS/en/aw3d30/data/index.htm
# costa_rica_tif_high_resolution <- raster::raster("data/relief/costa_rica_high_res.tif")
# elevation_costa_rica_high_resolution_df <- as.data.frame(costa_rica_tif_high_resolution, xy = TRUE) 
# colnames(elevation_costa_rica_high_resolution_df) <- c("long", "lat", "alt")

elevation_costa_rica_df <- as.data.frame(costa_rica_tif, xy = TRUE) 
contours_sf <- rasterToContour(costa_rica_tif, levels = seq(0, max(elevation_costa_rica_df), by = 1000)) %>% st_as_sf

elevation_costa_rica_df$inside_costa_rica <- secr::pointsInPolygon(elevation_costa_rica_df[, c("x", "y")], 
                                                             raster::getData('GADM', country='Costa Rica', level=0))
# or, if not working:
# elevation_costa_rica_df$inside_costa_rica <- secr::pointsInPolygon(elevation_costa_rica_df[, c("x", "y")],
#                                                              readRDS("data/borders/border_costa_rica_level_0.rds"))

colnames(elevation_costa_rica_df) <- c("long", "lat", "alt", "inside_costa_rica")

# ------------------------------------------- 3) position of rivers and lakes -------------------------------------------
# sf_rivers <- geojson_sf("data/rivers/costaRicaRivers.geojson")
# sf_large_rivers <- geojson_sf("data/rivers/costaRicaLargeRivers.geojson")
# sf_lakes <- geojson_sf("data/rivers/costaRicaLakes.geojson")

reduced_rivers <- st_crop(geojson_sf("data/rivers/costaRicaRivers.geojson"), 
                          xmin = pt.lim$xlim[1], ymin = pt.lim$ylim[1], xmax = pt.lim$xlim[2], ymax = pt.lim$ylim[2])   # first, we select only rivers inside the zone of interest
reduced_large_rivers <- st_crop(geojson_sf("data/rivers/costaRicaLargeRivers.geojson"), 
                                xmin = pt.lim$xlim[1], ymin = pt.lim$ylim[1], xmax = pt.lim$xlim[2], ymax = pt.lim$ylim[2])   # same for the large rivers
reduced_lakes <- st_crop(geojson_sf("data/rivers/costaRicaLakes.geojson"), 
                         xmin = pt.lim$xlim[1], ymin = pt.lim$ylim[1], xmax = pt.lim$xlim[2], ymax = pt.lim$ylim[2])   # same for the large rivers


# ------------------------------------------- 4) Rainfall Data -------------------------------------------

if (TRUE){
  rbindlist_fread_fct <- function(path, pattern = "*.csv") {
    files = list.files(path, pattern, full.names = TRUE)
    print(paste("Number of files: ", length(files)))
    rbindlist(lapply(files, function(x) fread(x)))
  }
  
  rainfall_data = rbindlist_fread_fct("data/rainfall/099 datos_antoine/")

  # need some preprocessing:
  # dim(rainfall_data[rainfall_data$date > "2021-05-01", ]) # removing all observations after this date
  rainfall_data <- rainfall_data[rainfall_data$date < "2021-05-01", ]
  
  rainfall_data$id <- rainfall_data$basin*1000 + rainfall_data$station
  
  list_of_stations <- unique(rainfall_data$id)
  list_stations_data <- list()
  
  pb <- txtProgressBar(min = 0, max = length(list_of_stations), style = 3)
  for (j in seq_along(list_of_stations)){
    df_s <- rainfall_data[rainfall_data$id == list_of_stations[j], ]
    # df_s <- df_s[df_s$source %in% c("M", "D", "T", "O", "L", "TRUE"), ]
    df_s <- df_s[df_s$source %in% c("M", "D", "T"), ]
    df_s$source<-factor(df_s$source, levels= c("M", "D", "T", "O", "L", "TRUE"))
    df_s$unique_id <- paste(df_s$date, df_s$dur)
    df_s <- df_s[order(df_s$unique_id, df_s$source),]
    df_s <- df_s[!duplicated(df_s$unique_id),]
    
    list_stations_data[[j]] <- df_s
    
    setTxtProgressBar(pb, j)
  }
  close(pb)
  
  rainfall_data <- do.call("rbind", list_stations_data)
  write.csv(rainfall_data, "data/rainfall/stations_data.csv", row.names=TRUE, quote=FALSE)
  
  rm(pb, df_s, list_stations_data, j)

}else{
  rainfall_data <- read.csv("data/rainfall/stations_data.csv", header = TRUE, row.names = 1)
  rainfall_data$date <- as.Date(rainfall_data$date)
}

list_of_stations <- unique(rainfall_data$id)
list_of_basins <- unique(rainfall_data$basin)
list_of_sources <- unique(rainfall_data$source)
# cat(sprintf("Number of stations: %s \n    id: %s", length(list_of_stations), paste(list_of_stations, collapse = ", ")))  # 162 stations
# cat(sprintf("Number of basins: %s \n    id: %s", length(list_of_basins), paste(list_of_basins, collapse = ", ")))   # 13 basins

min(rainfall_data$date) # "1990-01-01"
max(rainfall_data$date) # "2021-04-30"

rainfall_data[rainfall_data$int < 0, ]$int <- 0

stations_with_data <- as.numeric(unique(rainfall_data$id))

# ------------------------------------------- 5) Coordinates of the stations -------------------------------------------
station_coordinates <- read.delim("data/stations/coordinates.txt")
# +proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

station_coordinates <- station_coordinates[!duplicated(station_coordinates$id, fromLast = TRUE), ] # removing duplicates (taking only the first occurence)
station_coordinates$obs <- 0
station_coordinates[station_coordinates$id %in% stations_with_data,]$obs <- 1
station_coordinates$obs <- as.factor(station_coordinates$obs)
station_coordinates$basin <- floor(station_coordinates$id/1000)
station_coordinates$basin <- as.factor(station_coordinates$basin)

# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projcrs <- "+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

list_of_station_coordinates_df <- st_as_sf(x = station_coordinates,                         
                                           coords = c("x", "y"),
                                           crs = projcrs) %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

list_of_station_coordinates_df <- list_of_station_coordinates_df$geometry %>%
  st_coordinates() %>%
  as.data.frame

list_of_station_coordinates_df$id <- station_coordinates$id
list_of_station_coordinates_df$obs <- station_coordinates$obs
list_of_station_coordinates_df$basin <- station_coordinates$basin

list_of_station_coordinates_df <- list_of_station_coordinates_df[complete.cases(list_of_station_coordinates_df),]

duplicated_stations <- list_of_station_coordinates_df[duplicated(list_of_station_coordinates_df[,c("X", "Y")]), ]
for (j in c(1:dim(duplicated_stations)[1])){
  if (duplicated_stations$obs[j] == 1){
    indices <- list_of_station_coordinates_df$X == duplicated_stations[j, ]$X & list_of_station_coordinates_df$Y == duplicated_stations[j, ]$Y
    list_of_station_coordinates_df[indices,]$X <- rnorm(n = dim(list_of_station_coordinates_df[indices,])[1], mean = list_of_station_coordinates_df[indices,]$X[1], sd = 0.005)  # because 0.005 * 111 * 2 = 1.11 (so we move the station in a square of roughly 2km side center at the original data)
    list_of_station_coordinates_df[indices,]$Y <- rnorm(n = dim(list_of_station_coordinates_df[indices,])[1], mean = list_of_station_coordinates_df[indices,]$Y[1], sd = 0.005)  # because 0.005 * 111 * 2 = 1.11 (so we move the station in a square of roughly 2km side center at the original data)
  }
}
rm(indices, projcrs, duplicated_stations, j)

# list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% stations_with_data, ]$Y  # ok, all stations have known coordinates

if (FALSE){
  list_of_station_coordinates_df$alt <- 0
  
  pb <- txtProgressBar(min = 0, max = length(list_of_station_coordinates_df[, 1]), style = 3)
  for (j in c(1:length(list_of_station_coordinates_df[, 1]))){
    x_diff <- elevation_costa_rica_df$long - list_of_station_coordinates_df[j, ]$X
    y_diff <- elevation_costa_rica_df$lat - list_of_station_coordinates_df[j, ]$Y
    if (is.na(x_diff[1])){
      list_of_station_coordinates_df[j, ]$alt <- NA
    }else{
      diff_all <- abs(x_diff) == min(abs(x_diff)) & abs(y_diff) == min(abs(y_diff))
      list_of_station_coordinates_df[j, ]$alt <- as.numeric(elevation_costa_rica_df$alt[diff_all])
    }
    setTxtProgressBar(pb, j)
  }
  close(pb)
  
  # list_of_station_coordinates$alt <- list_of_station_coordinates_df$alt
  write.csv(list_of_station_coordinates_df, "data/stations/coordinates.csv", row.names=TRUE, quote=FALSE)
  
  rm(pb, x_diff, y_diff, diff_all, j)
  
}else{
  list_of_station_coordinates_df <- read.csv("data/stations/coordinates.csv", header = TRUE, row.names = 1)
  # list_of_station_coordinates$alt <- list_of_station_coordinates_df$alt
}

# stations at the same locations (but for all of them, we don't have the data, as obs = 0)
list_of_station_coordinates_df[duplicated(list_of_station_coordinates_df[,c("X", "Y")]), ]




# ------------------------------------------- 6) main cities -------------------------------------------
cities_coordinates <- data.frame("name" = c("Liberia", "Puntarenas", "Alajuela", "Heredia", "San Jose", "Cartago", "Puerto Limon"),
                                 "lat" = c(10.6333401, 9.9780814, 10.0165162, 9.9983493, 9.9325427, 9.8642435, 9.99524),
                                 "long" = c(-85.4362722, -84.8303637, -84.2138234, -84.1167945, -84.0795782, -83.9204377, -83.0293585))


# ------------------------------------------- 7) Basin ------------------------------------------------------------------
unzip("data/basins/basins_costa_rica.zip", exdir = "data/basins")
sf_basins <- read_sf('data/basins/Cuencas_Hidrologicas_Nacionales.shp')

# ------------------------------------------- 8) Station training/testing -----------------------------------------------
# create training stations
list_of_stations = unique(rainfall_data$id)

# Include stations 69507, 77001, 76026, 84118, 98006 and 75022 in testing (six stations of study)
extra_stations_to_include_in_testing = c(69507, 77001, 76026, 84118, 98006, 75022)
# stations that are typically far from others (this step could be automated...)
extra_stations_to_include_in_training = c(69578, 87010, 94014, 78020)

indices = sample(c(1:length(list_of_stations)), size = floor(length(list_of_stations) * 0.8), replace = FALSE)
list_stations_training = c()
for (j in indices){
  if (!(list_of_stations[j] %in% extra_stations_to_include_in_testing)){
    list_stations_training <- c(list_stations_training, list_of_stations[j])
  }
}
list_stations_training <- unique(c(list_stations_training, extra_stations_to_include_in_training))

list_stations_testing <- c()
for (j in seq_along(list_of_stations)){
  if (!(list_of_stations[j] %in% list_stations_training)){
    list_stations_testing <- c(list_stations_testing, list_of_stations[j])
  }
}

# check:
# extra_stations_to_include_in_training %in% list_stations_training
# extra_stations_to_include_in_testing %in% list_stations_testing
# extra_stations_to_include_in_training %in% list_stations_testing
# extra_stations_to_include_in_testing %in% list_stations_training

df_training <- data.frame(id = list_stations_training)
df_training$train <- "TRAINING"
df_testing <- data.frame(id = list_stations_testing)
df_testing$train <- "TESTING"
df <- rbind(df_training, df_testing)
# write.csv(df,"data/stations/data_training_testing_NEW.csv", row.names = TRUE)

# df <- merge(list_of_station_coordinates_df, df, by = "id")

# =============================== Save environment ======================================================================
save.image(file = "data/Env_intro.RData")

