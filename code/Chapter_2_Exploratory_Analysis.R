# =============================== Packages ==============================================================================
load("data/Env_intro.RData")
using("raster", "sf", "secr", "geojsonio", "data.table")
using("ggstar", "ggplot2", "ggrepel", "scales", "patchwork", "plotly", "viridis", "tidyverse")

setwd("/Users/Antoine/Desktop/Master Thesis Extremes Costa Rica ")

# =============================== DATA ==================================================================================
font_color_plot <- "white"
points_color <- "black"
theme_for_the_plots <- theme(plot.background = element_rect(fill = font_color_plot),
                             panel.background = element_rect(fill = "white", colour = "black"),
                             panel.grid.major = element_line(colour = "grey85"),
                             panel.grid.minor = element_line(colour = "grey90"),
                             legend.position = c(.95, .95),
                             legend.justification = c("right", "top"),
                             legend.box.just = "right",
                             legend.margin = margin(6, 6, 6, 6),
                             plot.margin = unit(c(1,1,1,1), "cm"),
                             legend.key = element_rect(colour = "transparent", fill = "transparent"))
# theme_shiny <- shinytheme('paper')

# - - - - - - - - - - - - - - - - - - - - - - - - - Provinces -------------

map_provinces <- ggplot() +
  coord_quickmap() + 
  geom_sf(data = provinces_costa_rica, aes(fill = NAME_1), colour = "black") + 
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent", size = 0.8) + 
  # scale_fill_manual(name = "Province", values = c("#006D77", "#83C5BE", "#B8DEDC", "#EDF6F9", "#F6EAE6", "#FFDDD2", "#E29578")) + 
  scale_fill_manual(name = "Province", values = c("#6484A3", "#F3DFCE", "#C66D45", "#E8BF53", "#8F0030", "#87976B", "#D18168")) + 
  # scale_fill_viridis(name = "Province", discrete = T, option = "plasma") + 
  theme_for_the_plots  + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),) + 
  labs(title = "Provinces of Costa Rica") + 
  theme(legend.position = c(.25, .25)) + 
  theme(legend.justification = c(-0.05, -0.05), legend.position = c(0, 0)) + 
  geom_star(data = cities_coordinates, mapping = aes(x = long, y = lat, text=paste('</br>', name)), 
            colour = "black", fill = "white", size = 4) + 
  geom_label_repel(data = cities_coordinates, mapping = aes(x = long, y = lat, label = name, text=paste('</br>', name)),
                   point.padding = 0.5,
                   segment.color = 'black')

map_provinces = map_provinces + theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) + labs(x = "longitude", y = "latitude")

# - - - - - - - - - - - - - - - - - - - - - - - - - Relief -------------

custom_colors_3 <- c("#32731E", "#76AA32", "#A9CA3F", "#D8DB51", "#D1A943", "#DA7640", "#D8382F", "#D8382F", "#FFFBFC") # "#489458", "#5CB679"
min_alt <- min(elevation_costa_rica_df$alt, na.rm = TRUE)
max_alt <- max(elevation_costa_rica_df$alt, na.rm = TRUE)
rescaled_values <- scales::rescale(c(min_alt, min_alt/2, -0.1, seq(0, max_alt, length.out = length(custom_colors_3))), 
                           to = c(0, 1), 
                           from = c(min_alt, max_alt))
cols <- c("#0E63A6", "#4E8DC1", "#DCEEFD", custom_colors_3)

# custom_colors_3 <- c("#B8D359", "#9EB452", "#84954B", "#BCBB8D", "#F3E1CE", "#DDA15E", "#BC6C25")
rescaled_values <- scales::rescale(seq(0, max_alt, length.out = length(custom_colors_3)), 
                           to = c(0, 1), 
                           from = c(0, max_alt))
cols <- custom_colors_3

plotCostaRica_relief <- ggplot() + 
  coord_quickmap() + 
  # geom_sf(data = contours_sf, linetype = 1, color = "gray") +
  geom_raster(data = elevation_costa_rica_df[elevation_costa_rica_df$alt > 0, ] , 
              aes(x = long, y = lat, fill = alt)) + 
  scale_fill_gradientn("altitude [m]", colours = cols, values = rescaled_values, na.value = "transparent") + 
  
  geom_sf(data = sf::st_cast(reduced_lakes$geometry, "MULTIPOLYGON"), colour = "transparent", fill = "#0E63A6") + 
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent", size = 0.8) + 
  
  labs(title="Elevation map of Costa Rica", x="longitude", y="latitude") + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "vertical") + 
  theme(legend.justification = c(-0.05, -0.05), 
        legend.position = c(0, 0),
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"))

plotCostaRica_relief = plotCostaRica_relief + theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) + labs(x = "longitude", y = "latitude")

# - - - - - - - - - - - - - - - - - - - - - - - - - Training/Testing set -------------

df_train_test <- read.csv("data/stations/data_training_testing.csv")[,c("id", "train")]
df_train_test <- merge(list_of_station_coordinates_df, df_train_test, by = "id")
df_train_test[df_train_test$train == TRUE,]$train <- "TRAIN"
df_train_test[df_train_test$train == FALSE,]$train <- "TEST"
contours_sf <- rasterToContour(costa_rica_tif, levels = seq(0, max(elevation_costa_rica_df), by = 1000)) %>% st_as_sf

ids_station_of_interest <- c(69507, 77001, 75022, 84118, 76026, 98006)
position_text <- data.frame(id = ids_station_of_interest, 
                            X_text = c(-84.4, -83.1, -83.6, -84, -85.5, -84), 
                            # Y_text = c(10.5, 10.2, 10.3, 10.3, 10.5, 9)
                            Y_text = c(10.6, 10.8, 10.5, 10.4, 9.4, 8.3)
)
position_text <- merge(position_text, list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% ids_station_of_interest, c("X", "Y", "id")], by = "id")
position_text$X_text = position_text$X

plot_stations <- ggplot() + coord_quickmap() + 
  geom_sf(data = contours_sf, linetype = 1, color = "black", size = 0.3) +
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent", size = 0.8) + 
  geom_point(data = df_train_test, 
             mapping = aes(x = X, y = Y, colour = train, text=paste('</br>ID: ', id,'</br>long: ', round(X, digits = 2),'</br>lat: ',round(Y, digits = 2))), 
             size = 2) + 
  scale_colour_discrete(name = "Station") + 
  geom_segment(aes(x = X_text, y = Y_text, xend = X, yend = Y), data = position_text, colour ="black") + 
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% ids_station_of_interest, ],
             mapping = aes(x = X, y = Y, text=paste('</br>ID: ', id, '</br>long: ', round(X, digits = 2),  '</br>lat: ',round(Y, digits = 2))),
             size = 3, colour = "red") +
  geom_label(data = position_text, aes(x=X_text, y=Y_text, label = id), colour = "black", fontface = "bold") + 
  
  labs(title="Training and testing set", x="longitude", y="latitude") + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  theme(#legend.position="right", 
    legend.direction = "vertical", legend.box = "horizontal",
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent")) +
  theme(legend.justification = c(-0.05, -0.05), legend.position = c(0, 0))


plot_stations = plot_stations + theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) + labs(x = "longitude", y = "latitude")

# - - - - - - - - - - - - - - - - - - - - - - - - - Drainage basin -------------

# position_text <- data.frame(basin = c(69, 73, 74, 75, 76, 77, 78, 79, 84, 88, 87, 94, 98), 
#                             X_text = -c(84.3, 83.4, 85.4, 83.3, 85.5, 83.2, 84.8, 83.1, 84.6, 84.2, 82.7, 83.9, 82.7),
#                             # Y_text = c(10.5, 10.2, 10.3, 10.3, 10.5, 9)
#                             Y_text = c(11.2, 10.5, 11.2, 10.4, 9.7, 10.3, 9.5, 10.2, 9.4, 9.3, 9.4, 9.2, 9.1)
# )
position_text <- data.frame(basin = c(69, 73, 74, 75, 76, 77, 78, 79, 84, 88, 87, 94, 98), 
                            X_text = -c(84.3, 83.6, 85.5, 83.2, 85.1, 83.1, 84.9, 83.3, 84.2, 84.2, 83.2, 84.1, 83.3),
                            # Y_text = c(10.5, 10.2, 10.3, 10.3, 10.5, 9)
                            Y_text = c(10.5, 10.2, 10.7, 10.3, 10.3, 10.2, 10.2, 9.9, 10, 9.7, 9.5, 9.3, 9.2)
)
# position_text <- merge(position_text, list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% ids_station_of_interest, c("X", "Y", "id")], by = "id")
# position_text$X_text = position_text$X

plot_basins <- ggplot(data = NULL) + 
  coord_quickmap() + 
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent", size = 0.8) + 
  # geom_sf(data = sf_basins, aes(fill = NOMBRE_C), colour = "black") + 
  geom_sf(data = sf_basins, colour = "black", fill = "transparent") + 
  scale_fill_discrete(name = "Basin") + 
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data$id), ], 
             aes(x = X, y = Y, colour = factor(basin),
                 text=paste('</br>ID: ', id,
                            '</br>long: ', round(X, digits = 2), 
                            '</br>lat: ',round(Y, digits = 2)
                 )), size = 2) + 
  geom_label(data = position_text, aes(x=X_text, y=Y_text, label = basin, colour = factor(basin)), fontface = "bold") + 
  scale_colour_discrete(name = "Basin") + 
  theme_for_the_plots + 
  labs(title = "Basins") + 
  theme(legend.justification = c(-0.05, -0.05), legend.position = c(0, 0)) + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        legend.position = "none") 

# ggplotly(pl, tooltip = c("text"))

plot_basins = plot_basins + theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) + labs(x = "longitude", y = "latitude")


# - - - - - - - - - - - - - - - - - - - - - - - - - Final plot -------------
p <- plotCostaRica_relief + plot_stations + map_provinces + plot_basins + plot_layout(nrow = 2, ncol = 2)
ggsave("results/Figures/chapter_2_exploratory_analysis/Figure_1.pdf", width = 35, height = 35, units = "cm", plot = p)

# ggplotly(plotCostaRica_relief, tooltip = c("text"))
# ggplotly(plot_stations, tooltip = c("text"))
# ggplotly(map_provinces, tooltip = c("text"))
# ggplotly(plot_basins, tooltip = c("text"))


# =============================== PART 1: exploratory data analysis =======================================================
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1) Map of costa rica ----------------------------------------

# Options:
high_resolution <- FALSE
elevation_contour <- FALSE
show_rivers <- TRUE
show_lakes <- TRUE
show_major_cities <- FALSE
show_stations <- TRUE
known_stations <- FALSE
terrain_color <- "custom3"
custom_colors_1 <- c("#D8D8D8", "#6B8E65", "#1E302F", "#ACBE94", "#5D4947", "#D2BEAA", "#9E634D")
custom_colors_2 <- c("#2B645E", "#51968E", "#93CAC0", "#CDE9E4", "#F5F5F5", "#F3E8C7", "#D9C386", "#B6843E", "#83531E")
custom_colors_3 <- c("#32731E", "#76AA32", "#A9CA3F", "#D8DB51", "#D1A943", "#DA7640", "#D8382F", "#D8382F", "#FFFBFC") # "#489458", "#5CB679"

plotCostaRica <- ggplot() + coord_quickmap()

# resolution of the elevation map
if (high_resolution){ 
  df_elevation <- elevation_costa_rica_high_resolution_df 
  tif_elevation <- costa_rica_tif_high_resolution
}else{ 
  df_elevation <- elevation_costa_rica_df 
  tif_elevation <- costa_rica_tif
}
df_elevation[is.na(df_elevation)] <- 0


# plotting contours or the elevations values
if (elevation_contour){
  contours_sf <- rasterToContour(tif_elevation, 
                                 levels = seq(0, max(df_elevation), by = 1000)) %>% st_as_sf
  plotCostaRica <- plotCostaRica + 
    geom_sf(data = contours_sf, linetype = 1, color = "gray") +
    geom_sf(data = border_costa_rica, colour = "black", fill = "transparent")
}else{
  if (terrain_color == "terrain"){
    color_terrain <- terrain.colors(10)
  }else if (terrain_color == "custom1"){
    color_terrain <- custom_colors_1
  }else if (terrain_color == "custom2"){
    color_terrain <- custom_colors_2
  }else if (terrain_color == "custom3"){
    color_terrain <- custom_colors_3
  }else{
    color_terrain <- custom_colors_2
  }
  
  min_alt <- min(df_elevation$alt, na.rm = TRUE)
  max_alt <- max(df_elevation$alt, na.rm = TRUE)
  rescaled_values <- rescale(c(min_alt, min_alt/2, -0.1, seq(0, max_alt, length.out = length(color_terrain))), 
                             to = c(0, 1), 
                             from = c(min_alt, max_alt))
  
  plotCostaRica <- plotCostaRica + 
    geom_raster(data = df_elevation , 
                aes(x = long, y = lat, fill = alt)) + 
    scale_fill_gradientn("altitude [m]", colours = c("#0E63A6", "#4E8DC1", "#DCEEFD", color_terrain), values = rescaled_values, na.value = "transparent") + 
    geom_sf(data = border_costa_rica, colour = "black", fill = "transparent")
}

# showing rivers
if (show_rivers){
  plotCostaRica <- plotCostaRica +
    geom_sf(data = sf::st_cast(reduced_rivers$geometry, "MULTILINESTRING"), colour = "#DCEEFD") # #4E8DC1
}
if (show_lakes){
  plotCostaRica <- plotCostaRica +
    geom_sf(data = sf::st_cast(reduced_lakes$geometry, "MULTIPOLYGON"), colour = "transparent", fill = "#0E63A6")
}

if (show_stations){
  list_of_station_coordinates_df$basin <- as.factor(list_of_station_coordinates_df$basin)
  stations <- unique(list_of_station_coordinates_df$id) 
  if (known_stations){
    stations <- unique(rbindlist_fread$id)
  }
  if (!elevation_contour){
    plotCostaRica <- plotCostaRica +
      geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% stations, ],
                 mapping = aes(x = X, y = Y), 
                 shape = 1, colour = "black",
                 size = 2)
  }
  plotCostaRica <- plotCostaRica + 
    geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% stations, ], 
               mapping = aes(x = X, y = Y, colour = basin,
                             text=paste('</br>ID: ', id,
                                        '</br>long: ', round(X, digits = 2), 
                                        '</br>lat: ',round(Y, digits = 2)
                             )), 
               size = 1.5) + 
    scale_colour_discrete(name = "basin") # add guide=F to remove legend
  # geom_sf(data = list_of_station_coordinates, 
  #         mapping = aes(colour = basin, text=paste('</br>ID: ', id,
  #                                                  '</br>long: ', round(X, digits = 2), 
  #                                                  '</br>lat: ',round(Y, digits = 2),
  #                                                  '</br>value: ',round(mean_int, 3))), size = 1)
}


if (show_major_cities){
  plotCostaRica <- plotCostaRica + 
    geom_point(data = cities_coordinates, mapping = aes(x = long, y = lat, text=paste('</br>', name)), colour = "black", fill = "#0E63A6") + 
    geom_label_repel(data = cities_coordinates, mapping = aes(x = long, y = lat, label = name, text=paste('</br>', name)),
                     point.padding = 0.5,
                     segment.color = 'black')
}



plotCostaRica_relief <- plotCostaRica + 
  # scale_alpha(range = c(0.15, 0.65), guide = "none") +
  labs(title="Elevation map of Costa Rica", x="longitude", y="latitude") + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "vertical") + 
  theme(legend.justification = c(-0.05, -0.05), legend.position = c(0, 0))

plotCostaRica_relief
# ggplotly(plotCostaRica_relief, tooltip = c("text"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 2) Stations characteristics ---------------------------------
show_elevation_contour <- TRUE
show_rivers <- TRUE
show_lakes <- TRUE
station_color <- "elevation" # "elevation", "basin" or "observed"
show_only_observed <- TRUE
plotCostaRica <- ggplot() + coord_quickmap()

if (show_elevation_contour){
  contours_sf <- rasterToContour(costa_rica_tif, 
                                 levels = seq(0, max(elevation_costa_rica_df), by = 1000)) %>% st_as_sf
  # contours_sf <- st_cast(contours_sf, to = "MULTIPOLYGON")
  plotCostaRica <- plotCostaRica + 
    geom_sf(data = contours_sf, linetype = 1, color = "black") +
    geom_sf(data = border_costa_rica, colour = "black", fill = "transparent")
}

if (show_rivers){
  plotCostaRica <- plotCostaRica +
    geom_sf(data = sf::st_cast(reduced_rivers$geometry, "MULTILINESTRING"), colour = "#DCEEFD") # #4E8DC1
}
if (show_lakes){
  plotCostaRica <- plotCostaRica +
    geom_sf(data = sf::st_cast(reduced_lakes$geometry, "MULTIPOLYGON"), colour = "transparent", fill = "#0E63A6")
}
if (show_only_observed){ stations_to_be_displayed <- c(1) }else{ stations_to_be_displayed <- c(0, 1)}

if (station_color == "elevation"){
  plotCostaRica <- plotCostaRica + 
    geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$obs %in% stations_to_be_displayed, ], 
               mapping = aes(x = X, y = Y, colour = alt, 
                             text=paste('</br>ID: ', id,
                                        '</br>longitude: ', round(X, digits = 2), 
                                        '</br>latitude: ',round(Y, digits = 2),
                                        '</br>altitude: ',alt, ' meters'
                             )), 
               size = 1.5) + 
    scale_color_viridis(name = "altitude [m]", limits = c(0, max(list_of_station_coordinates_df$alt)))
  
}else if (station_color == "basin"){
  plotCostaRica <- plotCostaRica + 
    geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$obs %in% stations_to_be_displayed, ], 
               mapping = aes(x = X, y = Y, colour = basin, 
                             text=paste('</br>ID: ', id,
                                        '</br>long: ', round(X, digits = 2), 
                                        '</br>lat: ',round(Y, digits = 2),
                                        '</br>basin: ',basin
                             )), 
               size = 1.5) + 
    scale_colour_discrete(name = "basin")
}else if (station_color == "observed"){
  plotCostaRica <- plotCostaRica + 
    geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$obs %in% stations_to_be_displayed, ], 
               mapping = aes(x = X, y = Y, colour = obs, 
                             text=paste('</br>ID: ', id,
                                        '</br>long: ', round(X, digits = 2), 
                                        '</br>lat: ',round(Y, digits = 2)
                             )), 
               size = 1.5) + 
    scale_colour_discrete(name = "observed")
}

plotCostaRica <- plotCostaRica + 
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent") + 
  # scale_alpha(range = c(0.15, 0.65), guide = "none") +
  ggtitle("Elevation Costa Rica") +
  labs(title="Costa Rica map", x="longitude", y="latitude") + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal")
# plotCostaRica
# ggplotly(plotCostaRica, tooltip = c("text"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 3) Station rainfall data ------------------------------------

is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

is.numeric0 <- function(x){
  is.numeric(x) && length(x) == 0L
}

# dat_all_stations_analysis <- rainfall_data
# dat_all_stations_analysis$month <- month.abb[as.numeric(format(dat_all_stations_analysis$date, "%m"))]
# dat_all_stations_analysis$month <- factor(dat_all_stations_analysis$month, levels = month.abb)
# dim(dat_all_stations_analysis[dat_all_stations_analysis$id == 69702 & dat_all_stations_analysis$dur == 180 & dat_all_stations_analysis$month == "Jun", ])

# t <- dat_all_stations_analysis[dat_all_stations_analysis$id == 69578 & dat_all_stations_analysis$dur == 180, ]
# t$date <- as.Date(t$date)
# plot(t$date, t$int)

show_elevation_contour <- TRUE
quantity_of_interest <- "var" # "mean", "variance"
month <- "ALL" # "Jan", "Feb", ..., "Dec", "ALL"
duration <- "ALL" # 5, 10, ..., 1440, "ALL"
log_scale_colors <- FALSE

plotCostaRica <- ggplot() + coord_quickmap()

if (show_elevation_contour){
  contours_sf <- rasterToContour(costa_rica_tif, 
                                 levels = seq(0, max(elevation_costa_rica_df), by = 1000)) %>% st_as_sf
  # contours_sf <- st_cast(contours_sf, to = "MULTIPOLYGON")
  plotCostaRica <- plotCostaRica + 
    geom_sf(data = contours_sf, linetype = 1, color = "black", size = 0.2)
}

dat_all_stations_analysis <- rainfall_data

if (duration != "ALL"){
  dat_all_stations_analysis <- dat_all_stations_analysis[dat_all_stations_analysis$dur == duration, ]
  legend_month <- month
}else{legend_month <- "All months"}

# dat_all_stations_analysis$id_station <- as.numeric(dat_all_stations_analysis$basin*1000 + dat_all_stations_analysis$station)
dat_all_stations_analysis$month <- month.abb[as.numeric(format(dat_all_stations_analysis$date, "%m"))]
dat_all_stations_analysis$month <- factor(dat_all_stations_analysis$month, levels = month.abb)
dat_all_stations_analysis$int <- as.numeric(dat_all_stations_analysis$int)

stations_with_data <- unique(dat_all_stations_analysis$id)
coordinates_observed_stations <- list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% stations_with_data, ]

months_ <- unique(dat_all_stations_analysis$month)
# dat_all_stations_analysis[dat_all_stations_analysis$month == months_[2], ]
# dat_all_stations_analysis$grouping <- paste(dat_all_stations_analysis$id, dat_all_stations_analysis$month)
# stats::aggregate(dat_all_stations_analysis[, c(6)], list(dat_all_stations_analysis$grouping), mean)
if (month != "ALL"){
  if (quantity_of_interest == "mean"){
    values_of_interest <- stats::aggregate(dat_all_stations_analysis[, c(6)], list(dat_all_stations_analysis$id, dat_all_stations_analysis$month), mean)
    legend_title <- "mean"
  }else if (quantity_of_interest == "var") {
    values_of_interest <- stats::aggregate(dat_all_stations_analysis[, c(6)], list(dat_all_stations_analysis$id, dat_all_stations_analysis$month), var)
    legend_title <- "variance"
  }
  colnames(values_of_interest) <- c("id", "month", "quantity_of_interest")
}else{
  if (quantity_of_interest == "mean"){
    values_of_interest <- stats::aggregate(dat_all_stations_analysis[, c(6)], list(dat_all_stations_analysis$id), mean)
    legend_title <- "mean"
  }else if (quantity_of_interest == "var") {
    values_of_interest <- stats::aggregate(dat_all_stations_analysis[, c(6)], list(dat_all_stations_analysis$id), var)
    legend_title <- "variance"
  }
  colnames(values_of_interest) <- c("id", "quantity_of_interest")
}

if (log_scale_colors){
  values_of_interest$quantity_of_interest <- log(values_of_interest$quantity_of_interest)
  legend_title <- paste(legend_title, "(log scale)")
}

if (month != "ALL"){
  values_of_interest_month <- values_of_interest[values_of_interest$month == month, ]
  
  number_of_observations <- dat_all_stations_analysis %>%
    group_by(id, dur, month) %>%
    summarise(
      int = length(int)) %>%
    ungroup() %>% as.data.frame()
  # number_of_observations <- number_of_observations[,c(1, 3, 4)]
  colnames(number_of_observations) <- c("id", "dur" , "month", "nb")
  
  number_of_observations <- number_of_observations[number_of_observations$month == month, ]
  number_of_observations <- number_of_observations[, c(1, 2, 4)]
}else{
  values_of_interest_month <- values_of_interest
  number_of_observations <- dat_all_stations_analysis %>%
    group_by(id, dur) %>%
    summarise(
      int = length(int)) %>%
    ungroup() %>% as.data.frame()
  # number_of_observations <- number_of_observations[,c(1, 3)]
  colnames(number_of_observations) <- c("id", "dur", "nb")
}

if (duration != "ALL"){
  number_of_observations <- number_of_observations[number_of_observations$dur == duration, c(1, 3)]
}else{
  number_of_observations <- number_of_observations %>%
    group_by(id) %>%
    summarise(
      nb = sum(nb)) %>%
    ungroup() %>% as.data.frame()
}

df <- list_of_station_coordinates_df[list_of_station_coordinates_df$obs == 1, ]
dim(df)
all_stations <- unique(df$id)
df <- merge(df, values_of_interest_month, by = "id")
df <- merge(df, number_of_observations, by = "id")
stations_in_df <- unique(df$id)

missing_stations <- all_stations[!(all_stations %in% stations_in_df)]
if (!is.integer0(missing_stations)){
  plotCostaRica <- plotCostaRica + 
    geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$obs == 1 & list_of_station_coordinates_df$id %in% missing_stations, ], 
               mapping = aes(x = X, y = Y, 
                             text=paste('</br>ID: ', id,
                                        '</br>long: ', round(X, digits = 2), 
                                        '</br>lat: ',round(Y, digits = 2),
                                        '</br>value: ','No observed value')), 
               size = 1.5, shape = 4, colour = "red")
}

cat(sprintf("Number of stations with no observed data for the given settings: %s.", length(missing_stations)))
cat(sprintf("Number of stations with only 1 observed data for the given settings: %s.", dim(df[is.na(df$quantity_of_interest), ])[1]))
cat(sprintf("Number of stations with more than 1 observed data for the given settings: %s.", dim(df[!is.na(df$quantity_of_interest), ])[1]))


plotCostaRica <- plotCostaRica + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  # geom_sf(data = sf_rivers, colour = "#4E8DC1", alpha = 0.5) + 
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent") + 
  # geom_sf(data = list_of_station_coordinates, mapping = aes(colour = basin, shape = obs), size = 1) + 
  geom_point(data = df, 
             mapping = aes(x = X, y = Y, colour = quantity_of_interest, size = nb,
                           text=paste('</br>ID: ', id,
                                      '</br>long: ', round(X, digits = 2), 
                                      '</br>lat: ',round(Y, digits = 2),
                                      '</br>value: ',round(quantity_of_interest, 3),
                                      '</br>nb: ',nb))) +
  scale_size("Number of observations", range = c(1, 3)) + 
  scale_color_viridis(name = legend_title, limits = c(min(df$quantity_of_interest), max(df$quantity_of_interest)), option = "A") + 
  # scale_color_viridis(discrete = TRUE, option = "D") + 
  # scale_colour_discrete("basin") + 
  labs(title = "Costa Rica map \n", subtitle=paste(legend_title, "of intensity values for all stations, for", legend_month, "\nDuration:", duration), x="longitude", y="latitude") + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal")


ggplotly(plotCostaRica, tooltip = c("text")) %>%
  layout(title = list(text = paste0("Costa Rica map \n",
                                    '<br>',
                                    '<sup>',
                                    paste(legend_title, "of intensity values for all stations, for", legend_month, "\nDuration:", duration),'</sup>')))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 4) Data availability ----------------------------------------
#                                                                     i) Data availability per month ----------------------
data_of_interest <- rainfall_data
data_of_interest$month <- month.abb[as.numeric(format(data_of_interest$date, "%m"))]
data_of_interest$month <- factor(data_of_interest$month, levels = month.abb)
data_of_interest$year <- format(data_of_interest$date, "%Y")

# data_monthly_maxima <- aggregate(int ~ year + id + month + dur, data_of_interest, max)
# data_annual_maxima <- aggregate(int ~ year + id + dur, data_of_interest, max)
# 
# data_monthly_maxima_with_exact_date <- data_of_interest %>% group_by(year, month, dur, id) %>%
#   summarize(exact_date = date[which(int == max(int))], int = max(int)) %>%
#   ungroup() %>% as.data.frame()
# 
# data_annual_maxima_with_exact_date <- data_of_interest %>% group_by(year, dur, id) %>%
#   summarize(exact_date = date[which(int == max(int))], int = max(int)) %>%
#   ungroup() %>% as.data.frame()

nb_obs_per_month <- data_of_interest %>%
  group_by(month, dur, id) %>%
  summarise(
    nb = length(int)
  ) %>% ungroup() %>% as.data.frame()
# nb_obs_per_month$dates <- as.Date(paste(nb_obs_per_month$year, as.numeric(nb_obs_per_month$month), "15", sep = "-")) # to center around the month...

# nb_obs_per_year <- data_of_interest %>%
#   group_by(year, dur, id) %>%
#   summarise(
#     nb = length(int)
#   ) %>% ungroup() %>% as.data.frame()
# nb_obs_per_year$dates <- as.Date(paste(nb_obs_per_year$year, "07-01", sep = "-"))   # to center around the year...

durs_for_plots <- c(5, 15, 60, 180, 1440)
list_plots <- list()
size_plot <- c()
for (du in seq_along(durs_for_plots)){
  dd <- nb_obs_per_month[nb_obs_per_month$dur == durs_for_plots[du], ]
  dd$basin <- as.factor(floor(dd$id / 1000))
  
  basins <- unique(dd$basin)
  for (j in seq_along(basins)){
    list_plots[[paste(du, j, sep = ".")]] <- ggplot(data = dd[dd$basin == basins[j], ], aes(x=as.factor(id), y=as.factor(month), fill=nb)) + 
      geom_tile() + 
      scale_fill_viridis(limits = c(0, max(nb_obs_per_month$nb)), # c(min(gev_quantile$fitted), max(gev_quantile$fitted))
                         option = "magma", name = "Number of \nobservations") + 
      theme_for_the_plots + 
      theme(legend.position="right") + 
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
      theme(plot.margin = margin(0.1, 0, 0.1, 0, "cm")) + 
      labs(x = "", y = durs_for_plots[du], title = basins[j]) + 
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) + 
      theme(legend.title.align=0.5)
    
    if (j > 1){
      list_plots[[paste(du, j, sep = ".")]] <- list_plots[[paste(du, j, sep = ".")]] + 
        theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title=element_blank()) +
        theme(plot.margin = margin(0.1, 0, 0.1, 0, "cm")) + labs(y = "", title = basins[j])
    }
    if (j < length(basins)){
      list_plots[[paste(du, j, sep = ".")]] <- list_plots[[paste(du, j, sep = ".")]] + theme(legend.position = "none")
    }
    # size_plot <- c(size_plot, length(unique(dd[dd$basin == basins[j], ]$id)))
  }
}
nb_obs_per_month$basin <- as.factor(floor(nb_obs_per_month$id / 1000))
basins <- unique(nb_obs_per_month$basin)
for (j in seq_along(basins)){
  size_plot <- c(size_plot, length(unique(nb_obs_per_month[nb_obs_per_month$basin == basins[j], ]$id)))
}
size_plot[size_plot<3] <- 3

p <- wrap_plots(list_plots, nrow = length(durs_for_plots), widths = size_plot, guides = "collect")
ggsave("results/Figures/chapter_2_exploratory_analysis/Figure_A.2.pdf", width = 35, height = 25, units = "cm", plot = p) # Figure A.2


#                                                                     ii) number of observations per day  -----------------
data_availability_per_dur <- rainfall_data %>% group_by(date, dur) %>% summarize(nb = length(int)) %>% ungroup() %>% as.data.frame()
data_availability <- rainfall_data %>% group_by(date) %>% summarize(nb = length(int)) %>% ungroup() %>% as.data.frame()
data_availability_nb_different_stations <- rainfall_data %>% group_by(date) %>% summarize(nb = length(unique(id))) %>% ungroup() %>% as.data.frame()

p1 <- ggplot(data = data_availability, aes(x = date, y = nb)) + 
  geom_point() + 
  geom_smooth(method = "loess", span = 0.1) + 
  labs(title = "Number of observations per day, across all stations and durations", x="Date", y="Number of observations") + 
  theme_for_the_plots + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal") + 
  scale_x_date(name = 'time', date_breaks = '2 year',date_labels = '%Y') + 
  theme(plot.margin = margin(1, 1, 0.2, 1, "cm"))

p2 <- ggplot()+
  geom_bar(data = data_availability_nb_different_stations, mapping = aes(x = date, y = nb), stat="identity", fill="black")+
  labs(title = "Number of different stations with measurements", x="Date", y="Number of stations") + 
  theme_for_the_plots + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal") + 
  scale_x_date(name = 'time', date_breaks = '2 year',date_labels = '%Y') + 
  theme(plot.margin = margin(0.1, 1, 0.1, 1, "cm"))

p3 <- ggplot(data = data_availability_per_dur, aes(x = date, y = nb, colour = as.factor(dur))) + 
  geom_smooth(method = "loess", span = 0.1, alpha = 0.5, se = F) + 
  scale_fill_viridis(name = "Duration", discrete = T) + 
  scale_colour_viridis(name = "Duration", discrete = T) + 
  labs(title = "Number of observations per day across all stations, grouped by duration", x="Date", y="Number of observations") + 
  theme_for_the_plots + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal") + 
  scale_x_date(name = 'time', date_breaks = '2 year',date_labels = '%Y') + 
  theme(plot.margin = margin(0.1, 1, 1, 1, "cm"))


p <- p1 + p2 + p3 + plot_layout(nrow = 3, guide = "collect", heights = c(1, 0.2, 1))
ggsave("results/Figures/chapter_2_exploratory_analysis/Figure_2.pdf", width = 35, height = 25, units = "cm", plot = p) # Figure 2


#                                                                     iii) When occured first observation of each station  -----------------
list_dates <- c("1999-01-01", "2002-01-01", "2006-01-01", "2022-01-01")
# check that dates between first and last, and sort them
list_dates_stations <- list()
for (j in c(1:length(list_dates))){
  list_dates_stations[[j]] <- unique(rainfall_data[rainfall_data$date < list_dates[j], c("id")])
}
list_dates_df <- list()
for (j in c(1:length(list_dates))){
  if (j == 1){
    list_dates_df[[j]] <- data.frame(id = list_dates_stations[[j]], period = list_dates[j])
  }else{
    list_dates_df[[j]] <- data.frame(id = setdiff(list_dates_stations[[j]], list_dates_stations[[j-1]]), period = list_dates[j])
  }
}
list_dates_df <- do.call(rbind, list_dates_df)
dat <- merge(list_dates_df, list_of_station_coordinates_df)

ggplot() + coord_quickmap() + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  # geom_sf(data = sf_rivers, colour = "#4E8DC1", alpha = 0.5) + 
  geom_sf(data = contours_sf, colour = "grey", fill = "transparent") + 
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent") + 
  # geom_sf(data = list_of_station_coordinates, mapping = aes(colour = basin, shape = obs), size = 1) + 
  geom_point(data = dat, 
             mapping = aes(x = X, y = Y, colour = period)) +
  scale_size("Number of observations", range = c(1, 3)) + 
  # scale_color_viridis(name = legend_title, limits = c(min(df$quantity_of_interest), max(df$quantity_of_interest)), option = "A") + 
  scale_color_discrete(name = "first observation before...") + 
  # scale_colour_discrete("basin") + 
  labs(title = "Repartition of the stations, by the date of the first observation", subtitle="Costa Rica", x="longitude", y="latitude") + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 5) Elevation of stations vs Costa Rica ----------------------

costa_rica_elevation_map_reduced_tif_border <- crop(costa_rica_tif, border_costa_rica)
costa_rica_elevation_map_reduced_tif_border <- mask(costa_rica_tif, border_costa_rica)

costa_rica_elevation_map_reduced_border <- as.data.frame(costa_rica_elevation_map_reduced_tif_border, xy = TRUE) 
colnames(costa_rica_elevation_map_reduced_border) <- c("long", "lat", "alt")
costa_rica_elevation_map_reduced_border <- costa_rica_elevation_map_reduced_border[!is.na(costa_rica_elevation_map_reduced_border$alt), ]

binwidth <- density(costa_rica_elevation_map_reduced_border$alt)$bw
n_bins <- length(ggplot2:::bin_breaks_width(range(costa_rica_elevation_map_reduced_border$alt), width = binwidth)$breaks) - 1L
p1 <- ggplot(costa_rica_elevation_map_reduced_border, aes(x=alt)) + 
  geom_histogram(aes(y = ..density..), binwidth = binwidth, fill = inferno(n_bins)) +
  # geom_density(fill="red", alpha = 0.2) + 
  labs(title = "Elevation profile of Costa Rica", x = "altitude [m]", "count") + 
  theme_for_the_plots

binwidth <- density(list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data$id), ]$alt)$bw
n_bins <- length(ggplot2:::bin_breaks_width(range(list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data$id), ]$alt), width = binwidth)$breaks) - 1L
p2 <- ggplot(list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data$id), ], aes(x=alt)) + 
  geom_histogram(aes(y = ..density..), binwidth = binwidth, fill = inferno(n_bins)) +
  # geom_density(fill="red", alpha = 0.2) + 
  labs(title = "Elevation profile of the stations Costa Rica", x = "altitude [m]", "count") + 
  theme_for_the_plots

p1 + p2 + plot_layout(ncol = 2)



#  - - - - - - - - 
station_altitudes <- list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data$id), ]

bin_size = 100
station_altitudes$alt_bin <- floor(station_altitudes$alt /bin_size) * bin_size
station_altitudes_bins <- station_altitudes %>% group_by(alt_bin) %>% summarize(nb = length(alt)) %>% ungroup() %>% as.data.frame()

costa_rica_elevation_map_reduced_tif_border <- crop(costa_rica_tif, border_costa_rica)
costa_rica_elevation_map_reduced_tif_border <- mask(costa_rica_tif, border_costa_rica)

costa_rica_elevation_map_reduced_border <- as.data.frame(costa_rica_elevation_map_reduced_tif_border, xy = TRUE) 
colnames(costa_rica_elevation_map_reduced_border) <- c("long", "lat", "alt")
costa_rica_elevation_map_reduced_border <- costa_rica_elevation_map_reduced_border[!is.na(costa_rica_elevation_map_reduced_border$alt), ]
costa_rica_elevation_map_reduced_border$alt_bin <- floor(costa_rica_elevation_map_reduced_border$alt /bin_size) * bin_size

costa_rica_altitudes_bins <- costa_rica_elevation_map_reduced_border %>% group_by(alt_bin) %>% summarize(nb = length(alt)) %>% ungroup() %>% as.data.frame()

station_altitudes_bins$nb <- station_altitudes_bins$nb/sum(station_altitudes_bins$nb)*100
station_altitudes_bins$type <- "Stations"

costa_rica_altitudes_bins$nb <- costa_rica_altitudes_bins$nb/sum(costa_rica_altitudes_bins$nb)*100
costa_rica_altitudes_bins$nb <- -costa_rica_altitudes_bins$nb
costa_rica_altitudes_bins$type <- "Costa Rica"

altitudes_all <- rbind(station_altitudes_bins, costa_rica_altitudes_bins)


# X Axis Breaks and Labels 
brks <- c(-0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3)*100
lbls <- c(0.3, 0.2, 0.1, 0.0, 0.1, 0.2, 0.3)*100

ids_station_of_interest <- c(69507, 77001, 75022, 84118, 76026, 98006)

df_alt_stations <- list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% ids_station_of_interest, ]
position_text <- data.frame(id = df_alt_stations$id, 
                            X_text = c(0.2, 0.3, 0.3, 0.2, 0.2, 0.2), 
                            Y_text = df_alt_stations$alt,
                            X = rep(0, length(ids_station_of_interest)),
                            Y = df_alt_stations$alt)
position_text$Y_text <- as.numeric(position_text$Y_text)
position_text$Y <- as.numeric(position_text$Y)


# Plot
station_elevation_profile <- ggplot() +  # Fill column
  geom_bar(data = altitudes_all, aes(x = alt_bin, y = nb, fill = type), stat = "identity", width = 100) +   # draw the bars
  scale_fill_manual(breaks = c("Costa Rica", "Stations"), values = c("#3A86FF", "#FB5607"), name = "") + 
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls,
                     limits = c(-max(abs(altitudes_all$nb)), max(abs(altitudes_all$nb)))) + # Labels
  geom_hline(yintercept = 0, color = "black", linetype = 1) + 
  coord_flip() +  # Flip axes
  geom_point(data = position_text,
             mapping = aes(y = X, x = Y, text=paste('</br>ID: ', id, '</br>long: ', round(X, digits = 2),  '</br>lat: ',round(Y, digits = 2))),
             size = 2, colour = "black") +
  geom_label_repel(data = position_text, aes(y = X, x = Y, label = id), fill = "white",
                   nudge_x = .15,
                   box.padding = 0.5,
                   nudge_y = 0.23,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 20,
                   fontface = "bold"
  ) + 
  labs(title="Elevation profile of Costa Rica and its stations", y = "Frequency [%]", x = "altitude [m]") +
  theme_for_the_plots + 
  theme(legend.justification = c(-0.05, +1), 
        legend.position = c(0, 1),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = .0),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey95")
  )


ids_station_of_interest <- c(69507, 77001, 75022, 84118, 76026, 98006)
position_text <- data.frame(id = ids_station_of_interest, 
                            X_text = c(-84.4, -83.1, -83.6, -84, -85.5, -84), 
                            # Y_text = c(10.5, 10.2, 10.3, 10.3, 10.5, 9)
                            Y_text = c(10.6, 10.8, 10.5, 10.4, 9.4, 8.3)
)
position_text <- merge(position_text, list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% ids_station_of_interest, c("X", "Y", "id")], by = "id")
position_text$X_text = position_text$X

plot_stations <- ggplot() + coord_quickmap() + 
  geom_sf(data = contours_sf, linetype = 1, color = "black", size = 0.3) +
  geom_sf(data = border_costa_rica, colour = "black", fill = "transparent", size = 1) + 
  # geom_point(data = df_train_test, 
  #            mapping = aes(x = X, y = Y, colour = train, text=paste('</br>ID: ', id,'</br>long: ', round(X, digits = 2),'</br>lat: ',round(Y, digits = 2))), 
  #            size = 1.5) + 
  # scale_colour_discrete(name = "Station") + 
  geom_segment(aes(x = X_text, y = Y_text, xend = X, yend = Y), data = position_text, colour ="black") + 
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% ids_station_of_interest, ],
             mapping = aes(x = X, y = Y, text=paste('</br>ID: ', id, '</br>long: ', round(X, digits = 2),  '</br>lat: ',round(Y, digits = 2))),
             size = 2, colour = "red") +
  geom_label(data = position_text, aes(x=X_text, y=Y_text, label = id), colour = "black", fontface = "bold") + 
  
  labs(title="Stations of interest for the univariate analysis", x="longitude", y="latitude") + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  theme(#legend.position="right", 
    legend.direction = "vertical", legend.box = "horizontal",
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    plot.title = element_text(hjust = .0)) +
  theme(legend.justification = c(-0.05, -0.05), legend.position = c(0, 0))

ggsave("results/Figures/chapter_2_exploratory_analysis/Figure_A.1.pdf", width = 42, height = 20, units = "cm", 
       plot = station_elevation_profile +  plot_stations + plot_layout(nrow = 1)) # Figure A.1



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 6) mean values and variance ---------------------------------

mean_values_plot <- function(DF_mean){
  durs_for_plots <- c(5, 15, 60, 180, 1440)
  list_plots <- list()
  size_plot <- c()
  for (du in seq_along(durs_for_plots)){
    dd <- DF_mean[DF_mean$dur == durs_for_plots[du], ]
    dd$basin <- as.factor(floor(dd$id / 1000))
    
    basins <- unique(dd$basin)
    for (j in seq_along(basins)){
      list_plots[[paste(du, j, sep = ".")]] <- ggplot(data = dd[dd$basin == basins[j], ], aes(x=as.factor(id), y=as.factor(month), fill=mean)) + 
        geom_tile() + 
        scale_fill_viridis(limits = c(min(dd$mean), max(dd$mean)), # c(min(gev_quantile$fitted), max(gev_quantile$fitted))
                           option = "magma", name = "Median", 
                           # trans = "log10"
        ) + 
        theme_for_the_plots + 
        theme(legend.position="right") + 
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
        theme(plot.margin = margin(0.1, 0, 0.1, 0, "cm")) + 
        labs(x = "", y = durs_for_plots[du], title = basins[j]) + 
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) + 
        theme(legend.title.align=0.5)
      
      if (j > 1){
        list_plots[[paste(du, j, sep = ".")]] <- list_plots[[paste(du, j, sep = ".")]] + 
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title=element_blank()) +
          theme(plot.margin = margin(0.1, 0, 0.1, 0, "cm")) + labs(y = "", title = basins[j])
      }
      if (j < length(basins)){
        list_plots[[paste(du, j, sep = ".")]] <- list_plots[[paste(du, j, sep = ".")]] + theme(legend.position = "none")
      }
      # size_plot <- c(size_plot, length(unique(dd[dd$basin == basins[j], ]$id)))
    }
  }
  
  basins <- unique(DF_mean$basin)
  for (j in seq_along(basins)){
    size_plot <- c(size_plot, length(unique(DF_mean[DF_mean$basin == basins[j], ]$id)))
  }
  size_plot[size_plot<3] <- 3
  
  return(wrap_plots(list_plots, nrow = length(durs_for_plots), widths = size_plot))
}

#                                                                     i) Mean daily intensities ---------------------------
dat_all_stations_analysis <- rainfall_data
dat_all_stations_analysis$month <- month.abb[as.numeric(format(dat_all_stations_analysis$date, "%m"))]
dat_all_stations_analysis$month <- factor(dat_all_stations_analysis$month, levels = month.abb)
dat_all_stations_analysis$year <- as.numeric(format(dat_all_stations_analysis$date, "%Y"))

mean_values_df <- dat_all_stations_analysis %>% group_by(id, month, dur) %>% summarise(mean = median(int)) %>%  ungroup() %>% as.data.frame()
mean_values_df$basin <- as.factor(floor(mean_values_df$id / 1000))

# mean_values_df[mean_values_df$dur == 60 & mean_values_df$basin == 69 & mean_values_df$month == "Jul", ]

# remove those observations ???
# plot(dat_all_stations_analysis[dat_all_stations_analysis$id  == 76044 & dat_all_stations_analysis$dur == 5, c("date", "int")], log = 'y')
# plot(dat_all_stations_analysis[dat_all_stations_analysis$id  == 69682 & dat_all_stations_analysis$dur == 60, c("date", "int")], log = 'y')
# plot(dat_all_stations_analysis[dat_all_stations_analysis$id  == 69520 & dat_all_stations_analysis$dur == 60, c("date", "int")], log = 'y')
# plot(dat_all_stations_analysis[dat_all_stations_analysis$id  == 69686 & dat_all_stations_analysis$dur == 60, c("date", "int")], log = 'y')
# 
# plot(dat_all_stations_analysis[dat_all_stations_analysis$id  == 69578 & dat_all_stations_analysis$dur == 1440, c("date", "int")], log = 'y')

mean_daily <- mean_values_plot(mean_values_df) + plot_annotation(title = "Median of daily intensities ", 
                                                                 theme = theme(plot.title = element_text(hjust = 0.5)))
ggsave("results/Figures/chapter_2_exploratory_analysis/Figure_3.a.pdf", width = 35, height = 25, units = "cm", plot = mean_daily)

#                                                                     i) Mean monthly max intensities ---------------------
dat_all_stations_analysis <- rainfall_data
dat_all_stations_analysis$month <- month.abb[as.numeric(format(dat_all_stations_analysis$date, "%m"))]
dat_all_stations_analysis$month <- factor(dat_all_stations_analysis$month, levels = month.abb)
dat_all_stations_analysis$year <- as.numeric(format(dat_all_stations_analysis$date, "%Y"))

dat_all_stations_analysis <- dat_all_stations_analysis %>% group_by(id, month, dur, year) %>% summarize(int = max(int)) %>%  ungroup() %>% as.data.frame()

mean_values_df <- dat_all_stations_analysis %>% group_by(id, month, dur) %>% summarise(mean = median(int)) %>%  ungroup() %>% as.data.frame()
mean_values_df$basin <- as.factor(floor(mean_values_df$id / 1000))

mean_monthly <- mean_values_plot(mean_values_df) + plot_annotation(title = "Median of monthly maximum intensities ", 
                                                                   theme = theme(plot.title = element_text(hjust = 0.5)))
ggsave("results/Figures/chapter_2_exploratory_analysis/Figure_3.b.pdf", width = 35, height = 25, units = "cm", plot = mean_monthly)



# =============================== PART 2: clean dataset ===================================================================
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1) Remove a station -----------------------------------------
# /!\ the station 79020 do not have coordinates... so we remove it from the original data set
stations_with_no_coordinates <- list_of_stations[!(list_of_stations%in%station_coordinates[station_coordinates$obs == 1, ]$id)]

rainfall_data <- rainfall_data[!(rainfall_data$id %in% stations_with_no_coordinates), ]

# removing also station 69702, because too few observations
rainfall_data <- rainfall_data[rainfall_data$id != 69702, ]
unique(rainfall_data$id)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 2) Remove some observations ---------------------------------

dat_all_stations_analysis <- rainfall_data
dat_all_stations_analysis$month <- month.abb[as.numeric(format(dat_all_stations_analysis$date, "%m"))]
dat_all_stations_analysis$month <- factor(dat_all_stations_analysis$month, levels = month.abb)
dat_all_stations_analysis$year <- as.numeric(format(dat_all_stations_analysis$date, "%Y"))

mean_values_df <- dat_all_stations_analysis %>% group_by(id, month, dur) %>% summarise(mean = median(int)) %>%  ungroup() %>% as.data.frame()
mean_values_df$basin <- as.factor(floor(mean_values_df$id / 1000))

# mean_values_df[mean_values_df$dur == 60 & mean_values_df$basin == 69 & mean_values_df$month == "Jul", ]

# remove those observations ???
plot(rainfall_data[rainfall_data$id  == 76044 & rainfall_data$dur == 5, c("date", "int")], log = 'y')
plot(rainfall_data[rainfall_data$id  == 69682 & rainfall_data$dur == 60, c("date", "int")], log = 'y')
plot(rainfall_data[rainfall_data$id  == 69520 & rainfall_data$dur == 60, c("date", "int")], log = 'y')
# plot(rainfall_data[rainfall_data$id  == 69686 & rainfall_data$dur == 60, c("date", "int")], log = 'y')
# plot(rainfall_data[rainfall_data$id  == 69578 & rainfall_data$dur == 1440, c("date", "int")], log = 'y')

rainfall_data <- rainfall_data[-which(rainfall_data$id  == 76044 & rainfall_data$dur == 5 & rainfall_data$int > 1000),]
rainfall_data <- rainfall_data[-which(rainfall_data$id  == 69682 & rainfall_data$dur == 60 & rainfall_data$int > 100),]
rainfall_data <- rainfall_data[-which(rainfall_data$id  == 69520 & rainfall_data$dur == 60 & rainfall_data$int > 500),]


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 3) Save -----------------------------------------------------
rainfall_data_clean <- rainfall_data
rainfall_data_clean$month <- month.abb[as.numeric(format(rainfall_data_clean$date, "%m"))]
rainfall_data_clean$month <- factor(rainfall_data_clean$month, levels = month.abb)
rainfall_data_clean$year <- format(rainfall_data_clean$date, "%Y")

data_monthly_maxima <- aggregate(int ~ year + id + month + dur, rainfall_data_clean, max)
data_monthly_maxima$basin <- as.factor(floor(data_monthly_maxima$id / 1000))

data_annual_maxima <- aggregate(int ~ year + id + dur, rainfall_data_clean, max)
data_annual_maxima$basin <- as.factor(floor(data_annual_maxima$id / 1000))


save(rainfall_data_clean, data_monthly_maxima, data_annual_maxima, file = "data/rainfall_data.RData")

