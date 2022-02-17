# =============================== Packages ==============================================================================
load("data/Env_intro.RData")
# using("raster", "sf", "secr", "geojsonio", "data.table")
using("ggstar", "ggplot2", "ggrepel", "scales", "patchwork", "plotly", "viridis", "tidyverse")

setwd("/Users/Antoine/Desktop/Master Thesis Extremes Costa Rica ")
load("data/rainfall_data.RData")

using("evgam", "plyr", "ggforce")

print("All packages downloaded !")
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

# =============================== Multivariate analysis with max-stable processes =======================================

file_name = "results/estimates/max_stable/pred_model_max_stable_stations.csv"
data_pred <- read.csv(file_name)


# ------------------ Plot station vs other stations ---------------------
station1 = 69507
station2 = 69620

plot_all_durations = FALSE
return_periods = seq(1, 20, 0.05)
return_levels_frechet = 1/(-log(1 - 1/return_periods))

duration_int = 60

full_data_monthly_maxima <- merge(data_monthly_maxima, list_of_station_coordinates_df[, c("id", "X", "Y", "basin", "alt")], by = "id")
durations = sort(unique(full_data_monthly_maxima$dur))
station_ids = sort(unique(full_data_monthly_maxima$id))

full_data_monthly_maxima$int_frechet = NA
for (dur in seq_along(durations)){
  for (id in seq_along(station_ids)){
    mask = full_data_monthly_maxima$dur == durations[dur] & full_data_monthly_maxima$id ==station_ids[id]
    ecdf_ = ecdf(full_data_monthly_maxima[mask, "int"])
    full_data_monthly_maxima[mask, "int_frechet"] = -1/log(ecdf_(full_data_monthly_maxima[mask, "int"]))
  }
}

other_stations_of_interest <- c(69654, 77001, 69620, 98034)
station_main <- 69507

list_plots_pairwise <- list()
for (s in seq_along(other_stations_of_interest)){
  print(s)
  coordinate_station2 <- list_of_station_coordinates_df[list_of_station_coordinates_df$id == other_stations_of_interest[s], ]
  ids_of_interest <- c(station_main, other_stations_of_interest[s])
  if (plot_all_durations){
    dat <- full_data_monthly_maxima[full_data_monthly_maxima$id %in% ids_of_interest, ]
  }else{
    dat <- full_data_monthly_maxima[full_data_monthly_maxima$id %in% ids_of_interest & 
                                      full_data_monthly_maxima$dur == duration_int, ]
  }
  
  dat_split <- split(dat[c("year", "month", "dur", "id", "int_frechet")], list(dat$id))
  for (j in seq_along(names(dat_split))){
    id <- dat_split[[names(dat_split)[j]]]$id[1]
    colnames(dat_split[[names(dat_split)[j]]]) <- c("year", "month", "dur", "id", id)
    dat_split[[names(dat_split)[j]]] <- dat_split[[names(dat_split)[j]]][c("year", "month", "dur", id)]
  }
  
  dat_merged <- merge(dat_split[[as.character(station_main)]], dat_split[[as.character(other_stations_of_interest[s])]], by = c("year", "month", "dur"))
  # dat_merged <- dat_split %>% reduce(full_join, by = c("year", "month", "dur"))     # make sure that values for x and y are occuring for during the same months
  dat_merged <- dat_merged[order(dat_merged$year, dat_merged$month, dat_merged$dur),]
  # dat_merged <- dat_merged[as.character(ids_of_interest)]
  dat_merged <- dat_merged[complete.cases(dat_merged), ]
  
  dat_merged$min_values <- as.numeric(apply(dat_merged[as.character(ids_of_interest)], 1, min))
  empirical_f <- function(x, z){
    return(sum(x > z)/length(x))
  }
  
  
  
  durations = unique(dat_merged$dur)
  df_l <- list()
  for (dur in seq_along(durations)){
    res = c()
    for (j in seq_along(return_levels_frechet)){
      res = c(res, empirical_f(dat_merged[dat_merged$dur==durations[dur], c("min_values")], return_levels_frechet[j]))
    }
    
    # V <- data_pred[which(data_pred[,"X"] == other_stations_of_interest[s]), paste("X", durations[dur], sep = "")]
    V <- data_pred[data_pred$s1 == station_main & data_pred$s2 == other_stations_of_interest[s] & data_pred$dur == durations[dur], "V"]
    se_V <- sqrt(data_pred[data_pred$s1 == station_main & data_pred$s2 == other_stations_of_interest[s] & data_pred$dur == durations[dur], "var_V"])
    V_lower <- V -1.96*se_V
    V_upper <- V +1.96*se_V
    # V_empirical <- data_pred[data_pred$id == other_stations_of_interest[s] & data_pred$dur == durations[dur], "empirical"]
    df_l[[dur]] <- data.frame(return_periods = return_periods, 
                              return_levels = return_levels_frechet,
                              prob = res,
                              duration = durations[dur],
                              fitted = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V/return_levels_frechet),
                              lower = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V_lower/return_levels_frechet),
                              upper = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V_upper/return_levels_frechet)
                              # empirical = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V_empirical/return_levels_frechet)
    )
  }
  df_l <- do.call(rbind, df_l)
  
  df_theoretical <- data.frame(return_periods = return_periods, 
                               return_levels = return_levels_frechet,
                               prob_dep = 1.0 - 1*exp(-1/return_levels_frechet),
                               prob_indep = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-2/return_levels_frechet)
                               
  )
  
  if (length(durations) == 1){
    pos_legend = "none"
    title_ = paste("Stations ", paste(as.character(ids_of_interest), collapse = " and "), sep = "")
    subtitle_ = paste("Duration ", durations, " [mn]", sep = "")
  }else{
    pos_legend = "right"
    title_ = paste("Stations ", paste(as.character(ids_of_interest), collapse = " and "), sep = "")
    subtitle_ = ""
  }
  
  list_plots_pairwise[[s]] <- ggplot() + 
    geom_ribbon(data = df_l, aes(x = return_periods, ymin = lower, ymax = upper, group = factor(duration),
                                 color = factor(duration), fill = factor(duration)), alpha = 0.3, colour = "transparent", fill = "#ffa600") +
    geom_point(data = df_l, aes(x = return_periods, y = prob, group = factor(duration), color = factor(duration)), alpha = 1, colour = "#003f5c") +
    geom_line(data = df_l, aes(x = return_periods, y = fitted, group = factor(duration), color = factor(duration))
              ,colour = "#ffa600"
              ,size = 0.6
    ) + 
    # geom_line(data = df_l, aes(x = return_periods, y = empirical, group = factor(duration), color = factor(duration))
    #           ,colour = "red"
    #           ,size = 0.6
    #           ,linetype = 2
    # ) + 
    scale_color_viridis(name = "Duration", discrete = T) + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10") + 
    geom_line(data = NULL, aes(x =df_theoretical$return_periods, y = df_theoretical$prob_dep), colour = "black", linetype = 2, size = 0.2) + 
    geom_line(data = NULL, aes(x =df_theoretical$return_periods, y = df_theoretical$prob_indep), color = "black", linetype = 2, size = 0.2) + 
    theme_for_the_plots + 
    theme(legend.position = "none") + 
    labs(x = "return period", y = "joint survival probability", 
         title = title_, subtitle = subtitle_) + 
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
  
}

list_plots_pairwise[[2]] <- list_plots_pairwise[[2]] + labs(y = NULL)
list_plots_pairwise[[3]] <- list_plots_pairwise[[3]] + labs(y = NULL)
list_plots_pairwise[[4]] <- list_plots_pairwise[[4]] + labs(y = NULL)

p_joint_prob <- wrap_plots(list_plots_pairwise, nrow = 1, guides = "collect")
ggsave("results/Figures/chapter_5_multivariate_analysis/joint_prob_max_stable_1_60.pdf", width = 40, height = 15, units = "cm", plot = p_joint_prob)

p_joint_prob_bis <- wrap_plots(list_plots_pairwise, nrow = 1, guides = "collect")
ggsave("results/Figures/chapter_5_multivariate_analysis/joint_prob_bivariate_2_60.pdf", width = 40, height = 15, units = "cm", plot = p_joint_prob_bis)


# ------------------ Plot station vs other locations ---------------------

list_stations <- c(75022, 84118, 77001, 69507, 98006, 76026)

pred_list <- list()
for (s in seq_along(list_stations)){
  pred_list[[s]] <- read.csv(paste("results/estimates/max_stable/max_stable_results/max_stable_",list_stations[s],".csv", sep = ""))
  pred_list[[s]]$id <- list_stations[s]
}
pred_list <- do.call(rbind, pred_list)
pt.lim = data.frame(xlim=c(-86, -82.5), ylim=c(8, 11.2))

pred_list <- pred_list[pred_list$X > pt.lim$xlim[1] & 
                         pred_list$X < pt.lim$xlim[2] & 
                         pred_list$Y > pt.lim$ylim[1] & 
                         pred_list$Y < pt.lim$ylim[2], ]
pred_list <- pred_list[pred_list$alt >= 0, ]

duration_int = 1440

plot_map_1 <- function(data_to_plot, name_var = "mean", ...){
  return(
    ggplot() + coord_quickmap() + 
      theme_for_the_plots + 
      geom_raster(data = data_to_plot, aes_string(x = "X", y = "Y", fill = name_var)) + 
      scale_fill_viridis(option = "magma",direction = -1,...
                         # trans = "log10"
      ) + 
      geom_sf(data = contours_sf, linetype = 1, color = "black") +
      scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
      scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
      geom_sf(data = border_costa_rica, colour = "black", fill = "transparent") + 
      geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data_clean$id),],
                 mapping = aes(x = X, y = Y),
                 # size = 1.5, colour = "red", 
                 size = 0.5, colour = "green",
                 # shape = 1
      ) +
      geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main,],
                 mapping = aes(x = X, y = Y),
                 size = 2, colour = "white") +
      labs(title=paste("Extremal coefficient from Husler-Reiss model, with respect to station ", station_main, sep = ""), 
         subtitle = paste("Duration ", duration_int, " [mn]", sep = ""),
         x="longitude", y="latitude") + 
      theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal")
  )
}

# data_pred <- pred_list[pred_list$id == station_main, ]
# 
# plotCostaRica_with_alt_p1 <- plot_map_1(data_pred, name_var = "mean")
# plotCostaRica_with_alt_p2 <- plot_map_1(data_pred, name_var = "upper", limits = c(1,2))
# plotCostaRica_with_alt_p3 <- plot_map_1(data_pred, name_var = "lower", limits = c(1,2))
# plotCostaRica_with_alt_p4 <- plot_map_1(data_pred, name_var = "std")
# 
# plotCostaRica_with_alt_p1 + plotCostaRica_with_alt_p4 + plotCostaRica_with_alt_p2 + plotCostaRica_with_alt_p3 + plot_layout(ncol = 2, nrow = 2)
# 
# 
# plotCostaRica_with_alt_p1 + plotCostaRica_with_alt_p4 + plot_layout(ncol = 2, nrow = 1)

get_point_cov <- function(cov, mean = c(0,0), r = 1.44){
  # https://stackoverflow.com/questions/40300217/obtain-vertices-of-the-ellipse-on-an-ellipse-covariance-plot-created-by-care
  
  ## initial circle
  theta <- seq(0, 2 * pi, by = 0.01 * pi)
  X <- r * cbind(cos(theta), sin(theta))
  
  ## target covariance matrix
  # A <- matrix(c(0.7486687, -0.8804840,-0.8804840, 2.066236), nrow = 2)
  A <- cov
  E <- eigen(A, symmetric = TRUE)  ## symmetric eigen decomposition
  U <- E[[2]]  ## eigen vectors, i.e., rotation matrix
  D <- sqrt(E[[1]])  ## root eigen values, i.e., scaling factor
  
  r <- 1.44  ## radius of original circle
  Z <- rbind(c(r, 0), c(0, r), c(-r, 0), c(0, -r))  ## original vertices on major / minor axes
  
  ## step 1: re-scaling
  X1 <- X * rep(D, each = nrow(X))  ## anisotropic expansion to get an axes-aligned ellipse
  Z1 <- Z * rep(D, each = 4L)  ## vertices on axes
  
  ## step 2: rotation
  Z2 <- tcrossprod(Z1, U)  ## rotated vertices on major / minor axes
  X2 <- tcrossprod(X1, U)  ## rotated ellipse
  
  ## different colour per quadrant
  g <- floor(4 * (1:nrow(X) - 1) / nrow(X)) + 1
  
  return(X2 + mean)
  
  ## draw rotated ellipse and vertices
  # plot(X2, asp = 1, col = g)
  # points(Z2, cex = 1.5, pch = 21, bg = 5)
  # 
  # ## draw axes-aligned ellipse and vertices
  # points(X1, col = g)
  # points(Z1, cex = 1.5, pch = 21, bg = 5)
  # 
  # ## draw original circle
  # points(X, col = g, cex = 0.25)
  # points(Z, cex = 1.5, pch = 21, bg = 5)
  # 
  # ## draw axes
  # abline(h = 0, lty = 3, col = "gray", lwd = 1.5)
  # abline(v = 0, lty = 3, col = "gray", lwd = 1.5)
  # 
  # ## draw major / minor axes
  # segments(Z2[1,1], Z2[1,2], Z2[3,1], Z2[3,2], lty = 2, col = "gray", lwd = 1.5)
  # segments(Z2[2,1], Z2[2,2], Z2[4,1], Z2[4,2], lty = 2, col = "gray", lwd = 1.5)
}

pred_cov <- read.csv("results/estimates/max_stable/pred_model_max_stable_cov_matrix.csv")

list_plots <- list()
for (s in seq_along(list_stations)){
  station_main = list_stations[s]
  
  a_v <- pred_cov$X[which(abs(pred_cov$X - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, c("X")]) == 
                            min(abs(pred_cov$X - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, c("X")])))][1] 
  b_v <- pred_cov$Y[which(abs(pred_cov$Y - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, c("Y")]) == 
                            min(abs(pred_cov$Y - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, c("Y")])))][1] 
  
  pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, ]
  
  Points_x <- get_point_cov(cov = matrix(c(pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_0_0"], 
                                           pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_1_0"],
                                           pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_1_0"], 
                                           pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_1_1"]), nrow = 2),
                            r = 0.4
  )
  Points_x[,1] <- Points_x[,1] + pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "X"]
  Points_x[,2] <- Points_x[,2] + pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Y"]
  
  
  list_plots[[s]] <- plot_map_1(pred_list[pred_list$id == list_stations[s], ], name_var = "V", limits = c(1.3,2.0)) + 
    geom_point(data = as.data.frame(Points_x), aes(x = V1, y = V2), colour = "red", shape = 21, size = 0.3) + 
    theme(axis.text.x = element_text(colour = "grey"),
          axis.text.y = element_text(colour = "grey"),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5),
          plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
    labs(x = "", y = "", title = list_stations[s], subtitle = "")
  
}
list_plots[[1]]

p <- wrap_plots(list_plots, nrow = 3, ncol = 2, guides = "collect") + 
  plot_annotation(title = "Mean value (extremal coefficient) \n \n", theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
ggsave("results/Figures/chapter_5_multivariate_analysis/save_extremal_coefficient_mean_max_stable.pdf", width = 30, height = 45, units = "cm", plot = p)



plotCostaRica_with_alt_p1 <- plot_map_1(pred_cov, name_var = "Q_0_0", name = "") + 
  theme(axis.text.x = element_text(colour = "grey"),
        axis.text.y = element_text(colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(x = "", y = "", title = "Cov[1,1]", subtitle = NULL)

plotCostaRica_with_alt_p2 <- plot_map_1(pred_cov, name_var = "Q_1_0", name = "") + 
  theme(axis.text.x = element_text(colour = "grey"),
        axis.text.y = element_text(colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(x = "", y = "", title = "Cov[2,1]", subtitle = NULL)

plotCostaRica_with_alt_p3 <- plot_map_1(pred_cov, name_var = "Q_1_1", name = "") + 
  theme(axis.text.x = element_text(colour = "grey"),
        axis.text.y = element_text(colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(x = "", y = "", title = "Cov[2,2]", subtitle = NULL)

plotCostaRica_with_alt_p4 <- plot_map_1(pred_cov, name_var = "a", name = "") + 
  theme(axis.text.x = element_text(colour = "grey"),
        axis.text.y = element_text(colour = "grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(x = "", y = "", title = "Weight function a(.)", subtitle = NULL)
p <- plotCostaRica_with_alt_p1 + plotCostaRica_with_alt_p2 + plotCostaRica_with_alt_p3 + plotCostaRica_with_alt_p4 + plot_layout(nrow = 2, ncol = 2) 
ggsave("results/Figures/chapter_5_multivariate_analysis/cov_matrix_max_stable.pdf", width = 30, height = 30, units = "cm", plot = p)




# ------------------ some plots (for poster) --------------

# Basic usage
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 0)) +
  coord_fixed()

# Rotation
# Note that it expects radians and rotates the ellipse counter-clockwise
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi / 4), fill = "blue", alpha = 0.4) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 5, b = 1.5, angle = pi / 4), fill = "red", alpha = 0.4) +
  coord_fixed()

# Draw a super ellipse
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 3, angle = -pi / 3, m1 = 3)) +
  coord_fixed()


list_stations <- c(75022, 84118, 77001, 69507, 98006, 76026)

pred_list <- list()
for (s in seq_along(list_stations)){
  pred_list[[s]] <- read.csv(paste("results/estimates/max_stable/max_stable_results/max_stable_",list_stations[s],".csv", sep = ""))
  pred_list[[s]]$id <- list_stations[s]
}
pred_list <- do.call(rbind, pred_list)


# pred_list$std <- (pred_list$upper - pred_list$lower)/(2*1.96)
pred_list <- pred_list[pred_list$X > pt.lim$xlim[1] & 
                         pred_list$X < pt.lim$xlim[2] & 
                         pred_list$Y > pt.lim$ylim[1] & 
                         pred_list$Y < pt.lim$ylim[2], ]
pred_list <- pred_list[pred_list$alt >= 0, ]

# station_main = 75022
duration_int = 1440


pred_cov <- read.csv("results/estimates/max_stable/pred_model_max_stable_cov_matrix.csv")


get_ellipse_attributes <- function(s){
  a_v <- pred_cov$X[which(abs(pred_cov$X - list_of_station_coordinates_df[list_of_station_coordinates_df$id == s, c("X")]) == 
                            min(abs(pred_cov$X - list_of_station_coordinates_df[list_of_station_coordinates_df$id == s, c("X")])))][1] 
  b_v <- pred_cov$Y[which(abs(pred_cov$Y - list_of_station_coordinates_df[list_of_station_coordinates_df$id == s, c("Y")]) == 
                            min(abs(pred_cov$Y - list_of_station_coordinates_df[list_of_station_coordinates_df$id == s, c("Y")])))][1] 
  
  print(pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, ])
  
  a = pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_0_0"]
  b = pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_1_0"]
  c = pred_cov[pred_cov$X == a_v & pred_cov$Y == b_v, "Q_1_1"]
  
  l1 <- sqrt((a+c)/2 + sqrt(((a-c)/2)**2 + b**2))
  l2 <- sqrt((a+c)/2 - sqrt(((a-c)/2)**2 + b**2))
  angle <- atan2(l1**2 - a, b)
  x = list_of_station_coordinates_df[list_of_station_coordinates_df$id == s, c("X")]
  y = list_of_station_coordinates_df[list_of_station_coordinates_df$id == s, c("Y")]
  
  return(list(l1 = l1, l2 = l2, angle = angle, x = x, y = y))
}

ellipse_76026 <- get_ellipse_attributes(76026)
ellipse_77001 <- get_ellipse_attributes(77001)
ellipse_69507 <- get_ellipse_attributes(69507)
ellipse_98006 <- get_ellipse_attributes(98006)

reduction_factor <- 1/3
p <- ggplot() + coord_quickmap() + 
  theme_for_the_plots + 
  
  geom_sf(data = border_costa_rica, colour = "white", fill = "#003657", size = 1.5) + 
  
  geom_ellipse(aes(x0 = ellipse_76026$x, y0 = ellipse_76026$y, a = reduction_factor*ellipse_76026$l1, b = reduction_factor*ellipse_76026$l2, angle = ellipse_76026$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_76026$x, y0 = ellipse_76026$y, a = reduction_factor*ellipse_76026$l1/2, b = reduction_factor*ellipse_76026$l2/2, angle = ellipse_76026$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_76026$x, y0 = ellipse_76026$y, a = reduction_factor*ellipse_76026$l1/4, b = reduction_factor*ellipse_76026$l2/4, angle = ellipse_76026$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == 76026,],mapping = aes(x = X, y = Y),
             size = 2, colour = "white") +
  
  geom_ellipse(aes(x0 = ellipse_77001$x, y0 = ellipse_77001$y, a = reduction_factor*ellipse_77001$l1, b = reduction_factor*ellipse_77001$l2, angle = ellipse_77001$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_77001$x, y0 = ellipse_77001$y, a = reduction_factor*ellipse_77001$l1/2, b = reduction_factor*ellipse_77001$l2/2, angle = ellipse_77001$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_77001$x, y0 = ellipse_77001$y, a = reduction_factor*ellipse_77001$l1/4, b = reduction_factor*ellipse_77001$l2/4, angle = ellipse_77001$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == 77001,],mapping = aes(x = X, y = Y),
             size = 2, colour = "white") +
  
  geom_ellipse(aes(x0 = ellipse_69507$x, y0 = ellipse_69507$y, a = reduction_factor*ellipse_69507$l1, b = reduction_factor*ellipse_69507$l2, angle = ellipse_69507$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_69507$x, y0 = ellipse_69507$y, a = reduction_factor*ellipse_69507$l1/2, b = reduction_factor*ellipse_69507$l2/2, angle = ellipse_69507$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_69507$x, y0 = ellipse_69507$y, a = reduction_factor*ellipse_69507$l1/4, b = reduction_factor*ellipse_69507$l2/4, angle = ellipse_69507$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == 69507,],mapping = aes(x = X, y = Y),
             size = 2, colour = "white") +
  
  geom_ellipse(aes(x0 = ellipse_98006$x, y0 = ellipse_98006$y, a = reduction_factor*ellipse_98006$l1, b = reduction_factor*ellipse_98006$l2, angle = ellipse_98006$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_98006$x, y0 = ellipse_98006$y, a = reduction_factor*ellipse_98006$l1/2, b = reduction_factor*ellipse_98006$l2/2, angle = ellipse_98006$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_ellipse(aes(x0 = ellipse_98006$x, y0 = ellipse_98006$y, a = reduction_factor*ellipse_98006$l1/4, b = reduction_factor*ellipse_98006$l2/4, angle = ellipse_98006$angle), fill = "#EEC60E", alpha = 0.4, colour = "transparent") +
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == 98006,],mapping = aes(x = X, y = Y),
             size = 2, colour = "white") +
  
  labs(title=NULL, 
       subtitle = NULL,
       x="longitude", y="latitude") + 
  theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal") + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent"), #"#003657"
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent", size = 0.1),
        panel.grid.minor = element_line(colour = "transparent", size = 0.05),
        axis.text = element_text(colour = "transparent"),
        axis.title = element_text(colour = "transparent"),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"), colour = "transparent"),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last"), colour = "transparent"),
        axis.ticks = element_line(colour = "transparent"))
ggsave("results/Figures/chapter_5_multivariate_analysis/max_stable_extremal_coeff_example.png", p, bg = "transparent")

