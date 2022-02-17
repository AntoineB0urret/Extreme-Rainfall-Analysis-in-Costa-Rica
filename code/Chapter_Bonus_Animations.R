
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
load("data/Env_intro.RData")
load("data/rainfall_data.RData")

using("raster", "sf", "secr", "geojsonio", "data.table")
using("ggplot2", "rayshader", "viridis", "ColorPalette")
# using("hexbin")

setwd("/Users/Antoine/Desktop/Master Thesis Extremes Costa Rica ")

# =============================== Data ==================================================================================
costa_rica_elevation_high_res_tif <- raster::raster("data/relief/costa_rica_high_res.tif")
costa_rica_elevation_low_res_extended_tif <- raster::raster("data/relief/costa_rica_low_res_extended.tif")
# costa_rica_elevation_low_res_extended_tif_new_high_res <- disaggregate(costa_rica_elevation_low_res_extended_tif, fact=0.004166667/0.0002777778)

# costa_rica_elevation_high_res_mat = raster_to_matrix(costa_rica_elevation_high_res_tif)
# costa_rica_elevation_high_res_mat[is.na(costa_rica_elevation_high_res_mat)] <- 0
# 
# costa_rica_elevation_low_res_extended_mat = raster_to_matrix(costa_rica_elevation_low_res_extended_tif_new_high_res)
# costa_rica_elevation_low_res_extended_mat[is.na(costa_rica_elevation_low_res_extended_mat)] <- 0
# 
# costa_rica_elevation_high_res_mat[costa_rica_elevation_high_res_mat <= 0] <- costa_rica_elevation_low_res_extended_mat[costa_rica_elevation_high_res_mat <= 0]
# merged_high_res <- raster(t(costa_rica_elevation_high_res_mat))
# 
# extent(merged_high_res) <- extent(costa_rica_elevation_low_res_extended_tif)
# crs(merged_high_res) <- crs(costa_rica_elevation_low_res_extended_tif)
# writeRaster(merged_high_res, "data/relief/costa_rica_high_res_with_ocean_depth.tif", overwrite=TRUE)


# =============================== 3D plots ==============================================================================
# ------------------------------------------- From rayshader directly ---------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 2.1.1) Reducing the map size -------------------------------------------
# The map size might be too big and mequire too much computing time for rendering. Thus, here we reduce its size:
# costa_rica_elevation_high_res_tif <- raster::raster("data/relief/costa_rica_high_res.tif")
costa_rica_elevation_high_res_tif <- raster::raster("data/relief/costa_rica_high_res_with_ocean_depth.tif")

costa_rica_elevation_map_reduced_tif <- resample(costa_rica_elevation_high_res_tif, raster(nrow=1000, ncol=1000, xmn=-86, xmx=-82, ymn=8, ymx=12))


costa_rica_elevation_map_reduced = raster_to_matrix(costa_rica_elevation_map_reduced_tif)
costa_rica_elevation_map_reduced[is.na(costa_rica_elevation_map_reduced)] <- 0
water_pos = costa_rica_elevation_map_reduced
water_pos[water_pos < 0] = 0

costa_rica_elevation_map_reduced %>%
  sphere_shade(texture = "desert", sunangle = 225) %>%
  add_water(detect_water(water_pos), color = "desert") %>%
  add_shadow(ray_shade(costa_rica_elevation_map_reduced, sunangle = 225, sunaltitude = 45), max_darken = 0.7) %>%
  add_shadow(ambient_shade(costa_rica_elevation_map_reduced), max_darken = 0.7) %>%
  plot_map()

# use height_shade(texture = topo.colors(256)) instead of sphere_shade to have a texture depending on the elevation matrix (/!\ shade are not taken into account...)

costa_rica_elevation_map_reduced %>%
  sphere_shade(zscale = 10, texture = "imhof2") %>%   # ('imhof1','imhof2','imhof3','imhof4','desert', 'bw', and 'unicorn').
  add_water(detect_water(water_pos), color = "imhof2") %>%
  add_shadow(ray_shade(costa_rica_elevation_map_reduced), 0.5) %>%
  add_shadow(ambient_shade(costa_rica_elevation_map_reduced),0) %>%
  plot_3d(costa_rica_elevation_map_reduced, zscale = 100, fov = 60, theta = -100, phi = 30, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, waterlinecolor = "white", waterlinealpha = 0.5,
          wateralpha = 0, watercolor = "lightblue")

# adding border points 
render_points(extent = attr(costa_rica_elevation_map_reduced_tif,"extent"), heightmap = costa_rica_elevation_map_reduced,
              lat = unlist(path_border[,2]), long = unlist(path_border[,1]), 
              zscale=100, color="white",offset=100, size=2)
# render_points(clear_previous=TRUE)


costa_rica_elevation_map = raster_to_matrix(costa_rica_tif)
costa_rica_elevation_map[is.na(costa_rica_elevation_map)] <- 0
water_pos = costa_rica_elevation_map
water_pos[water_pos < 0] = 0

costa_rica_elevation_map %>%
  sphere_shade(zscale = 10, texture = "desert", sunangle = 225) %>%   # ('imhof1','imhof2','imhof3','imhof4','desert', 'bw', and 'unicorn').
  add_water(detect_water(water_pos), color = "desert") %>%
  add_shadow(ray_shade(costa_rica_elevation_map, sunangle = 225), 0.5) %>%
  add_shadow(ambient_shade(costa_rica_elevation_map),0) %>%
  plot_3d(costa_rica_elevation_map, zscale = 100, fov = 60, theta = -100, phi = 30, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, waterlinecolor = "white", waterlinealpha = 0.5,
          wateralpha = 0.5, watercolor = "lightblue")



costa_rica_elevation_map_reduced_tif_border <- crop(costa_rica_elevation_map_reduced_tif, border_costa_rica)
costa_rica_elevation_map_reduced_tif_border <- mask(costa_rica_elevation_map_reduced_tif, border_costa_rica)
costa_rica_elevation_map_reduced = raster_to_matrix(costa_rica_elevation_map_reduced_tif_border)
costa_rica_elevation_map_reduced[is.na(costa_rica_elevation_map_reduced)] <- NA
costa_rica_elevation_map_reduced_original <- costa_rica_elevation_map_reduced

costa_rica_elevation_map_reduced_added <- costa_rica_elevation_map_reduced + 5000
costa_rica_elevation_map_reduced %>%
  height_shade(texture = colorRampPalette(c("#E8F9F5", "#948B7E", "#AD7652","#AD7762","#B66C70","#B35E5E","#BC4944"))(256)) %>%
  # sphere_shade(create_texture("white", "black", "grey", "grey", "grey"), zscale=60, sunangle=225) %>%
  # add_overlay(height_shade(costa_rica_elevation_map_reduced, 
  #                          texture = colorRampPalette(c("#E8F9F5", "#948B7E", "#AD7652","#AD7762","#B66C70","#B35E5E","#BC4944"))(256))) %>%
  # add_shadow(ray_shade(costa_rica_elevation_map_reduced, zscale=60, sunaltitude=25, sunangle=225)) %>%
  # sphere_shade(texture = create_texture("white", "black", "grey", "grey", "white"), zscale=60, sunangle=0) %>%
  add_overlay(sphere_shade(costa_rica_elevation_map_reduced_original,
                           texture = create_texture("white", "black", "grey", "grey", "grey"), zscale=60, sunangle=0)) %>%
  add_shadow(ray_shade(costa_rica_elevation_map_reduced, zscale=60, sunaltitude=25, sunangle=0)) %>%
  # sphere_shade(texture = "imhof2") %>%
  # add_shadow(ray_shade(costa_rica_elevation_map_reduced_border), 0.5) %>%
  # add_shadow(ambient_shade(costa_rica_elevation_map_reduced_border), 0) %>%
  # plot_map()
  plot_3d(costa_rica_elevation_map_reduced,
          zscale = 60, fov = 60, theta = 0, phi = 90, windowsize = c(1000, 800), zoom = 0.6,
          # shadowdepth = -20,
          # shadowwidth = 100,
          solid = FALSE,
          baseshape = "circle",
          # watercolor = "lightblue", wateralpha = 0.2, 
          # waterdepth = 0,
          # water = TRUE,
  )


increase_shading <- function(mat, shade_factor = 0.5){
  # mat must be a NxMx3 matrix...
  return(apply(mat, c(1, 2), function(x){return(x * (1-shade_factor))}))
}


height_map[c(500:510), c(100:110), 1]

increase_shading(height_map[c(500:510), c(100:110), ])


costa_rica_elevation_map_reduced %>%
  # height_shade(texture = colorRampPalette(c("#E8F9F5", "#948B7E", "#AD7652","#AD7762","#B66C70","#B35E5E","#BC4944"))(256)) %>%
  sphere_shade(create_texture("white", "black", "grey", "grey", "grey"), zscale=60, sunangle=225) %>%
  add_overlay(height_shade(costa_rica_elevation_map_reduced, 
                           texture = colorRampPalette(c("#E8F9F5", "#948B7E", "#AD7652","#AD7762","#B66C70","#B35E5E","#BC4944"))(256))) %>%
  add_shadow(ray_shade(costa_rica_elevation_map_reduced, zscale=60, sunaltitude=25, sunangle=225)) %>%
  add_overlay(sphere_shade(costa_rica_elevation_map_reduced_original, 
                           texture = create_texture("white", "black", "grey", "grey", "grey"), zscale=60, sunangle=225)) %>%
  # sphere_shade(texture = "imhof2") %>%
  # add_shadow(ray_shade(costa_rica_elevation_map_reduced_border), 0.5) %>%
  # add_shadow(ambient_shade(costa_rica_elevation_map_reduced_border), 0) %>%
  # plot_map()
  plot_3d(costa_rica_elevation_map_reduced_added,
          zscale = 60, fov = 60, theta = 0, phi = 90, windowsize = c(1000, 800), zoom = 0.6,
          # shadowdepth = -20,
          # shadowwidth = 100,
          shadow = FALSE,
          solid = FALSE,
          baseshape = "circle",
          # watercolor = "lightblue", wateralpha = 0.2, 
          # waterdepth = 0,
          # water = TRUE,
          background = "transparent"
  )



# color_pal <- c("#FFFFFF", "#F4FCFA", "#E8F9F5", "#EEF3B8", "#F4ED7B", "#FAE73E", "#FFE100")
# color_pal <- c("#F4FCFA", "#E8F9F5", "#F2C38A", "#FB8D1F", "#D14200", "#7B3312")
# color_pal <- c("#F4FCFA", "#E8F9F5", "#F2C38A", "#FB8D1F", "#DC6B32", "#BC4944")
color_pal <- c("#FFFFFF", "#FEFAF7", "#CDC7C7", "#9D9298", "#8E7073", "#AA716F", "#C3726E", "#E0726C")
# color_pal <- c("white", "white", "#E8F9F5", "#AD7652","#AD7762","#B66C70","#B35E5E","#BC4944")
color_pal <- c("#F4FCFA", "#E8F9F5", "#D6F4C4", "#C3EE92", "#9EE32F", "#72A618", "#466801")
color_pal <- c("white", "white", "grey")
color_pal <- c("#FFFFFF", "#C0CDD5", "#809BAB", "#406981", "#003657")
color_pal <- c("white", "#EEC60E")
# Color based on elevation
costa_rica_elevation_map_reduced[costa_rica_elevation_map_reduced<1] <- 1
height_map <- height_shade(costa_rica_elevation_map_reduced, 
                           texture = colorRampPalette(color_pal)(256),
                           # texture = scales::viridis_pal(option = "plasma")(100)
                           )
# sphere shade: 
# shadow_map <- sphere_shade(costa_rica_elevation_map_reduced, 
#                            texture = create_texture("white", "black", "grey", "grey", "grey"), zscale=100, sunangle=0)

shadow_ray <- ray_shade(costa_rica_elevation_map_reduced, zscale=100, sunaltitude=70, sunangle=0)
shadow_ray2 <- t(shadow_ray[dim(shadow_ray)[1]:1, ])

shadow_ambiant_map <- ambient_shade(costa_rica_elevation_map_reduced,maxsearch = 100, multicore=TRUE)
shadow_ambiant_map2 <- t(shadow_ambiant_map[dim(shadow_ambiant_map)[1]:1, ])

shadow_lamb_map <- lamb_shade(costa_rica_elevation_map_reduced, zscale=100, sunaltitude=70, sunangle = 0)
shadow_lamb_map2 <- t(shadow_lamb_map[dim(shadow_lamb_map)[1]:1, ])


height_map_new <- array(NA,dim = dim(height_map))
height_map_new[,,c(1,2,3)] <- height_map[,,c(1,2,3)]

split_mat <- FALSE
factor <- 0.2
th <- 0.5
if (split_mat){
  # shadow_ray3 <- shadow_ray2
  # shadow_ray3[is.na(shadow_ray3)] <- 0
  # height_map_new[,,1][shadow_ray3 >= th] <- (height_map_new[,,1] + (1- height_map_new[,,1])*(shadow_ray3)*factor)[shadow_ray3 >= th]
  # height_map_new[,,2][shadow_ray3 >= th] <- (height_map_new[,,2] + (1- height_map_new[,,2])*(shadow_ray3)*factor)[shadow_ray3 >= th]
  # height_map_new[,,3][shadow_ray3 >= th] <- (height_map_new[,,3] + (1- height_map_new[,,3])*(shadow_ray3)*factor)[shadow_ray3 >= th]
  # 
  # shadow_ray4 <- shadow_ray2
  # shadow_ray4[is.na(shadow_ray4)] <- 1
  # height_map_new[,,1][shadow_ray4 < th] <- (height_map_new[,,1] *(1-shadow_ray4)*factor)[shadow_ray4 < th]
  # height_map_new[,,2][shadow_ray4 < th] <- (height_map_new[,,2] *(1-shadow_ray4)*factor)[shadow_ray4 < th]
  # height_map_new[,,3][shadow_ray4 < th] <- (height_map_new[,,3] *(1-shadow_ray4)*factor)[shadow_ray4 < th]
  # 
}else{
  height_map_new[,,1] <- height_map_new[,,1] + (1- height_map_new[,,1])*(shadow_ray2)*factor
  height_map_new[,,2] <- height_map_new[,,2] + (1- height_map_new[,,2])*(shadow_ray2)*factor
  height_map_new[,,3] <- height_map_new[,,3] + (1- height_map_new[,,3])*(shadow_ray2)*factor
}

height_map_new[,,1] <- height_map_new[,,1] * (shadow_ambiant_map2**0.2)
height_map_new[,,2] <- height_map_new[,,2] * (shadow_ambiant_map2**0.2)
height_map_new[,,3] <- height_map_new[,,3] * (shadow_ambiant_map2**0.2)

# height_map_new[,,1] <- height_map_new[,,1] * (shadow_map[,,1])
# height_map_new[,,2] <- height_map_new[,,2] * (shadow_map[,,1])
# height_map_new[,,3] <- height_map_new[,,3] * (shadow_map[,,1])

height_map_new[,,1] <- height_map_new[,,1] * (shadow_lamb_map2)
height_map_new[,,2] <- height_map_new[,,2] * (shadow_lamb_map2)
height_map_new[,,3] <- height_map_new[,,3] * (shadow_lamb_map2)


plot_3d(height_map_new, costa_rica_elevation_map_reduced,
        zscale = 100, fov = 60, theta = 0, phi = 90, windowsize = c(1000, 800), zoom = 0.6,
        solid = FALSE,
        background = "black",
        shadow = FALSE
)


img_elev <- raster::raster(t(costa_rica_elevation_map_reduced))
extent(img_elev) <- extent(costa_rica_elevation_low_res_extended_tif)
crs(img_elev) <- crs(costa_rica_elevation_low_res_extended_tif)

costa_rica_elevation_map_reduced_df <- as.data.frame(img_elev, xy=TRUE)

# costa_rica_elevation_map_reduced_df[complete.cases(costa_rica_elevation_map_reduced_df),]

height_map_new_hex_color <- apply(height_map_new, c(1,2), function(x){if(is.na(x[1])){return(NA)}else{rgb(x[1], x[2], x[3])}})
# height_map_new_hex_color <- t(height_map_new_hex_color[dim(height_map_new_hex_color)[1]:1, ])
# height_map_new_hex_color_df <- as.data.frame(raster::raster(height_map_new_hex_color), xy=TRUE)

d1 <- expand.grid(x = c(1:length(unique(costa_rica_elevation_map_reduced_df$x))), y = c(1:length(unique(costa_rica_elevation_map_reduced_df$y)))) 
out <- transform(d1, z = height_map_new_hex_color[as.matrix(d1)])
height_map_new_hex_color_df <- out[order(out$x), ]

costa_rica_elevation_map_reduced_df$color <- height_map_new_hex_color_df$z

# col1 <- rgb2hsv(255, 60, 90)
# col2rgb(hsv(col1[1,], col1[2,], col1[3,]))

p <- ggplot() + 
  coord_quickmap() + 
  geom_raster(data = costa_rica_elevation_map_reduced_df[complete.cases(costa_rica_elevation_map_reduced_df),], aes(x = x, y = y, fill = color, color = color)) + 
  # scale_fill_viridis() + 
  # scale_color_viridis() + 
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% rainfall_data_clean$id,],
             aes(x = X, y = Y), colour = "#003657") + 
  scale_color_identity() + 
  scale_fill_identity() + 
  theme_for_the_plots + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid = element_line(color = "transparent"),
        axis.text = element_text(color = "transparent"),
        axis.title = element_text(color = "transparent"))
ggsave("/Users/Antoine/Desktop/Poster PdM/test_img.png", p, bg = "transparent")


# render_highquality()
min(shadow_ray[,], na.rm = T)


# costa_rica_elevation_map_reduced %>%
#   sphere_shade(zscale = 10, texture = "desert", sunangle = 225) %>%
#   height_shade(texture = c("white")) %>%
#   add_shadow(ray_shade(costa_rica_elevation_map_reduced, sunangle = 225, sunaltitude = 80), max_darken = 0.7) %>%
#   add_shadow(ambient_shade(costa_rica_elevation_map_reduced), max_darken = 0.7) %>%
#   plot_map()
# 
# costa_rica_elevation_map_reduced %>%
#   height_shade(texture = c("white")) %>%
#   add_shadow(ray_shade(costa_rica_elevation_map_reduced, sunangle = 225, sunaltitude = 30, zscale= 200), 0.5) %>%
#   add_shadow(ambient_shade(costa_rica_elevation_map_reduced, zcale = 200),0) %>%
#   plot_map()
# 
# 
# costa_rica_elevation_map_reduced %>%
#   sphere_shade(zscale=200, texture = create_texture("red","red","red","red","white")) %>%
#   add_shadow(ambient_shade(costa_rica_elevation_map_reduced, maxsearch = 100, multicore = TRUE,zscale=200), 0) %>%
#   add_shadow(lamb_shade(costa_rica_elevation_map_reduced), 0.5) %>%
#   plot_map()



# costa_rica_elevation_map_reduced_border = raster_to_matrix(costa_rica_elevation_map_reduced_tif_border)
costa_rica_elevation_map_reduced_border = raster_to_matrix(costa_rica_elevation_map_reduced_tif)
costa_rica_elevation_map_reduced_border[is.na(costa_rica_elevation_map_reduced_border)] <- 0

costa_rica_elevation_map_reduced_border_water <- costa_rica_elevation_map_reduced_border

# costa_rica_elevation_map_reduced_border[costa_rica_elevation_map_reduced_border <= 0] <- NA

costa_rica_elevation_map_reduced_border_water %>%
  height_shade(texture = colorRampPalette(c("#E8F9F5", "#948B7E", "#AD7652","#AD7762","#B66C70","#B35E5E","#BC4944"))(256)) %>%
  add_shadow(ray_shade(costa_rica_elevation_map_reduced_border_water, zscale=60, sunaltitude=25, sunangle=225)) %>%
  # sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(costa_rica_elevation_map_reduced_border_water)) %>%
  # add_shadow(ray_shade(costa_rica_elevation_map_reduced_border), 0.5) %>%
  # add_shadow(ambient_shade(costa_rica_elevation_map_reduced_border), 0) %>%
  # plot_map()
  plot_3d(costa_rica_elevation_map_reduced_border_water,
          zscale = 60, fov = 60, theta = 0, phi = 90, windowsize = c(1000, 800), zoom = 0.6,
          # shadowdepth = -20,
          # shadowwidth = 100,
          solid = TRUE,
          baseshape = "circle",
          watercolor = "lightblue", wateralpha = 0.2, 
          waterdepth = 0,
          water = TRUE,
          )

n_frames <- 100

# theta <- transition_values(from = 0, to = 360, steps = n_frames, 
#                            one_way = TRUE, type = "lin")
# phi <- transition_values(from = 10, to = 70, steps = n_frames, 
#                          one_way = FALSE, type = "cos")
# zoom <- transition_values(from = 0.4, to = 0.8, steps = n_frames, 
#                           one_way = FALSE, type = "cos")

shadowdepth <- transition_values(from = 0, to = -20, steps = n_frames, 
                                     one_way = TRUE, type = "cos")
shadowwidth <- transition_values(from = 0, to = 100, steps = n_frames, 
                                 one_way = TRUE, type = "cos")
zscale <- transition_values(from = 200, to = 60, steps = n_frames, 
                                 one_way = TRUE, type = "lin")
zoom <- transition_values(from = 0.8, to = 0.6, steps = n_frames, 
                          one_way = TRUE, type = "lin")
phi <- transition_values(from = 40, to = 25, steps = n_frames, 
                         one_way = TRUE, type = "lin")
theta <- transition_values(from = 0, to = 0, steps = n_frames,
                           one_way = TRUE, type = "lin")

build_accelerating_rotation <- function(nb_rounds = 5, n_steps_total = 100){
  steps <- cumsum((-cos(seq(0, 2*pi, 2*pi/n_steps_total)) + 1)/2)
  steps <- steps/steps[length(steps)] * 360 * nb_rounds
  return(steps)
}

theta <- build_accelerating_rotation()
zoom <- transition_values(from = 0.6, to = 1, steps = length(theta), 
                          one_way = FALSE, type = "cos")
phi <- transition_values(from = 30, to = 60, steps = length(theta), 
                         one_way = FALSE, type = "cos")

# zscale <- 60
costa_rica_elevation_map_reduced_border_water %>% 
  height_shade(texture = scales::viridis_pal(option = "plasma")(100)) %>%
  add_water(detect_water(costa_rica_elevation_map_reduced_border_water)) %>%
  save_3d_gif(costa_rica_elevation_map_reduced_border_water, file = "/Users/Antoine/Desktop/frames_test/costaRicaAnimation5.gif", duration = 5,
              solid = TRUE, 
              shadow = FALSE, 
              zscale = 60,
              # watercolor = "lightblue", wateralpha = 0.8, 
              # waterlinecolor = "white", waterlinealpha = 0.1,
              # waterdepth = waterdepths, 
              # shadowdepth = 0,
              # shadowwidth = 0,
              # theta = theta, phi = phi, zoom = zoom, 
              theta = theta, 
              phi = phi, zoom = zoom, 
              fov = 60, windowsize = c(1000, 800),
              
              baseshape = "circle",
              watercolor = "lightblue", wateralpha = 0.4, 
              waterdepth = 0,
              water = TRUE,
              )


# ------------------------------------------- from ggplot ---------------------------------------------------------------

cols <- c("#32731E", "#76AA32", "#A9CA3F", "#D8DB51", "#D1A943", "#DA7640", "#D8382F", "#D8382F", "#FFFBFC") # "#489458", "#5CB679"
rescaled_values <- scales::rescale(seq(0, max(elevation_costa_rica_df$alt, na.rm = TRUE), length.out = length(cols)), 
                                   to = c(0, 1), 
                                   from = c(0, max(elevation_costa_rica_df$alt, na.rm = TRUE)))

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


plotCostaRica_relief <- ggplot(data = elevation_costa_rica_df[elevation_costa_rica_df$alt > 0 & elevation_costa_rica_df$inside_costa_rica == TRUE, ] , 
                               aes(x = long, y = lat, fill = alt), alpha = 1) + 
  # geom_hex(bins = 40, size = 0.5, color = "black") +
  coord_quickmap() + 
  # geom_sf(data = contours_sf, linetype = 1, color = "gray") +
  geom_raster() + 
  # scale_fill_gradientn("altitude [m]", colours = cols, values = rescaled_values, na.value = "transparent") + 
  scale_fill_viridis_c(option = "C") + 
  
  # geom_sf(data = sf::st_cast(reduced_lakes$geometry, "MULTIPOLYGON"), colour = "transparent", fill = "#0E63A6") + 
  # geom_sf(data = border_costa_rica, colour = "black", fill = "transparent", size = 0.8) + 
  
  labs(title=NULL, x=NULL, y=NULL) + 
  theme_for_the_plots + 
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "vertical") + 
  theme(panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        axis.ticks = element_line(colour = NA),
        axis.text = element_text(colour = NA)
        )
  
# plotCostaRica_relief = plotCostaRica_relief + theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) + labs(x = "longitude", y = "latitude")

plot_gg(plotCostaRica_relief, multicore = TRUE, raytrace = TRUE, 
        width = 7, height = 7, 
        # scale = 300, 
        # windowsize = c(1400, 1400), 
         #zoom = 0.6, 
        # phi = 30, theta = 30,
        scale = 100, fov = 60, theta = -100, phi = 30, windowsize = c(1000, 800), zoom = 0.6
        )




montereybay %>%
  height_shade() %>%
  plot_map()

#Add a shadow:

montereybay %>%
  height_shade() %>%
  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
  plot_map()


#Change the palette:

montereybay %>%
  height_shade(texture = topo.colors(256)) %>%
  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
  plot_map()


#Really change the palette:

montereybay %>%
  height_shade(texture = rainbow(256)) %>%
  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
  plot_map()


# =============================== Animations ============================================================================

# ------------------------------------------- 4) Animations -------------------------------------------

elev_matrix <- costa_rica_elevation_map
n_frames <- 180
zscale <- 200
# frame transition variables
waterdepthvalues <- min(elev_matrix)/2 - min(elev_matrix)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues <- -90 + 45 * cos(seq(0, 2*pi, length.out = n_frames))
# shadow layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)

# generate .png frame images
img_frames <- paste0("/Users/Antoine/Desktop/PdM 2021/Results/animations/animation1/drain", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  elev_matrix %>%
    sphere_shade(texture = "imhof2") %>%
    add_shadow(ambmat, 0) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elev_matrix, solid = TRUE, shadow = TRUE, zscale = zscale, 
            water = TRUE, watercolor = "imhof3", wateralpha = 0.8, 
            waterlinecolor = "#ffffff", waterlinealpha = 0.5,
            waterdepth = waterdepthvalues[i], 
            theta = thetavalues[i], phi = 45)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# build gif
# install.packages("gifski")
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "/Users/Antoine/Desktop/PdM 2021/Results/animations/animation1/montereybay.gif", 
                        delay = 6/n_frames)


# or instead:
n_frames <- 100
waterdepths <- transition_values(from = 0, to = min(costa_rica_elevation_map), steps = n_frames) 
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
# generate gif
zscale <- 100
costa_rica_elevation_map %>% 
  sphere_shade(texture = "imhof2", zscale = 10) %>%
  add_shadow(ambient_shade(costa_rica_elevation_map, zscale = 10), 0.5) %>%
  add_shadow(ray_shade(costa_rica_elevation_map, zscale = 10, lambert = TRUE), 0.5) %>%
  save_3d_gif(costa_rica_elevation_map, file = "/Users/Antoine/Desktop/PdM 2021/Results/animations/animation1/costaRicaAnimation.gif", duration = 5,
              solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
              watercolor = "lightblue", wateralpha = 0.8, 
              waterlinecolor = "white", waterlinealpha = 0.1,
              waterdepth = waterdepths, 
              theta = thetas, phi = 45, fov = 60, zoom = 0.8, windowsize = c(400, 320))


n_frames <- 100
waterdepths <- transition_values(from = 0, to = min(costa_rica_elevation_map), steps = n_frames) 

theta <- transition_values(from = 0, to = 360, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 10, to = 70, steps = n_frames, 
                         one_way = FALSE, type = "cos")
zoom <- transition_values(from = 0.4, to = 0.8, steps = n_frames, 
                          one_way = FALSE, type = "cos")
zscale <- 100
costa_rica_elevation_map %>% 
  sphere_shade(texture = "imhof2", zscale = 10) %>%
  add_shadow(ambient_shade(costa_rica_elevation_map, zscale = 10), 0.5) %>%
  add_shadow(ray_shade(costa_rica_elevation_map, zscale = 10, lambert = TRUE), 0.5) %>%
  save_3d_gif(costa_rica_elevation_map, file = "/Users/Antoine/Desktop/PdM 2021/Results/animations/animation1/costaRicaAnimation.gif", duration = 5,
              solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
              watercolor = "lightblue", wateralpha = 0.8, 
              waterlinecolor = "white", waterlinealpha = 0.1,
              waterdepth = waterdepths, 
              theta = theta, phi = phi, fov = 60, zoom = zoom, windowsize = c(400, 320))


# ------------------------------------------- X) Some auxiliary functions -------------------------------------------

#' Build a gif of 3D rayshader plots
#'
#' @param hillshade Hillshade/image to be added to 3D surface map.
#' @param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' @param file file path for .gif
#' @param duration gif duration in seconds (framerate will be duration/n_frames)
#' @param ... additional arguments passed to rayshader::plot_3d(). See Details for more info.
#'
#' @details This function is designed to be a pipe-in replacement for rayshader::plot_3d(),
#' but it will generate a 3D animated gif. Any inputs with lengths >1 will 
#' be interpreted as "animation" variables, which will be used to generate 
#' individual animation frames -- e.g. a vector of theta values would produce
#' a rotating gif. Inputs to plot_3d() that are meant to have length >1 
#' (specifically "windowsize") will be excluded from this process.
#'
#' @return file path of .gif file created
#' 
#' @examples
#' # MONTEREREY BAY WATER DRAINING
#' # ------------------------------
#' # define transition variables
#' n_frames <- 180
#' waterdepths <- transition_values(from = 0, to = min(montereybay), steps = n_frames) 
#' thetas <- transition_values(from = -45, to = -135, steps = n_frames)
#' # generate gif
#' zscale <- 50
#' montereybay %>% 
#'   sphere_shade(texture = "imhof1", zscale = zscale) %>%
#'   add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
#'   add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
#'   save_3d_gif(montereybay, file = "montereybay.gif", duration = 6,
#'               solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
#'               watercolor = "imhof3", wateralpha = 0.8, 
#'               waterlinecolor = "#ffffff", waterlinealpha = 0.5,
#'               waterdepth = waterdepths/zscale, 
#'               theta = thetas, phi = 45)
#' 
save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}


#' Create a numeric vector of transition values.
#' @description This function helps generate a sequence 
#' of numeric values to transition "from" a start point
#' "to" some end point. The transition can be "one_way" 
#' (meaning it ends at the "to" point) or "two_way" (meaning
#' we return back to end at the "from" point).
#'
#' @param from starting point for transition values
#' @param to ending point (for one-way transitions) or turn-around point 
#'           (for two-way transitions)
#' @param steps the number of steps to take in the transation (i.e. the length
#'              of the returned vector)
#' @param one_way logical value to determine if we should stop at the "to" value
#'                (TRUE) or turn around and return to the "from" value (FALSE)
#' @param type string defining the transition type - currently suppoerts "cos"
#'             (for a cosine curve) and "lin" (for linear steps)
#'
#' @return a numeric vector of transition values
#' 
transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}
