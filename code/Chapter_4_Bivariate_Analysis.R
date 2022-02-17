# =============================== Packages ==============================================================================
load("data/Env_intro.RData")
# using("raster", "sf", "secr", "geojsonio", "data.table")
using("ggstar", "ggplot2", "ggrepel", "scales", "patchwork", "plotly", "viridis", "tidyverse")

setwd("/Users/Antoine/Desktop/Master Thesis Extremes Costa Rica ")
load("data/rainfall_data.RData")

using("evgam", "plyr", "sf", "raster")

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



# =============================== Bivariate model (results) ==========================================================
# ------------------ Plot station vs other locations ---------------------
list_stations <- c(75022, 84118, 77001, 69507, 98006, 76026)

pred_list <- list()
for (s in seq_along(list_stations)){
  pred_list[[s]] <- read.csv(paste("results/estimates/bivariate/pred_model_bivariate_",list_stations[s],"_full.csv", sep = ""))
  pred_list[[s]]$id <- list_stations[s]
}
pred_list <- do.call(rbind, pred_list)


pred_list$std <- (pred_list$upper - pred_list$lower)/(2*1.96)
pred_list <- pred_list[pred_list$X > pt.lim$xlim[1] & 
                         pred_list$X < pt.lim$xlim[2] & 
                         pred_list$Y > pt.lim$ylim[1] & 
                         pred_list$Y < pt.lim$ylim[2], ]

station_main = 75022
duration_int = 1440
other_stations_of_interest <- c(69654, 77001, 69620, 98034)
other_stations_of_interest <- NULL

plot_map_1 <- function(data_to_plot, name_var = "mean", ...){
  return(
    ggplot() + coord_quickmap() + 
      theme_for_the_plots + 
      geom_raster(data = data_to_plot, aes_string(x = "X", y = "Y", fill = name_var)) + 
      scale_fill_viridis(option = "magma", name = "Extremal coefficient", direction = -1,...
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
      # geom_text_repel(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main,], 
      #                 aes(x=X, y=Y, label = id), colour ="white",
      #                 force=1,
      #                 vjust=1,
      #                 direction='y',
      #                 nudge_x=0.1) +
      # geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% other_stations_of_interest,],
      #            mapping = aes(x = X, y = Y),
      #            size = 2, colour = "green") +
      # geom_text_repel(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% other_stations_of_interest,], 
      #                 aes(x=X, y=Y, label = id), colour ="green",
    #                 force=1,
    #                 vjust=0,
    #                 direction='y',
    #                 nudge_x=-0.1) +
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

list_plots <- list()
for (s in seq_along(list_stations)){
  station_main = list_stations[s]
  list_plots[[s]] <- plot_map_1(pred_list[pred_list$id == list_stations[s], ], name_var = "mean", limits = c(1.2,1.9)) + 
    theme(axis.text.x = element_text(colour = "grey"),
          axis.text.y = element_text(colour = "grey"),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5),
          plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
    labs(x = "", y = "", title = list_stations[s], subtitle = "")
  
}
p <- wrap_plots(list_plots, nrow = 3, ncol = 2, guides = "collect") + 
  plot_annotation(title = "Mean value (extremal coefficient) \n \n", theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
ggsave("results/Figures/chapter_4_bivariate_analysis/save_extremal_coefficient_mean.pdf", width = 30, height = 45, units = "cm", plot = p)




list_plots <- list()
for (s in seq_along(list_stations)){
  station_main = list_stations[s]
  list_plots[[s]] <- plot_map_1(pred_list[pred_list$id == list_stations[s], ], name_var = "std", limits = c(0,0.25)) + 
    theme(axis.text.x = element_text(colour = "grey"),
          axis.text.y = element_text(colour = "grey"),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5),
          plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
    labs(x = "", y = "", title = list_stations[s], subtitle = "")
  
}
p <- wrap_plots(list_plots, nrow = 3, ncol = 2, guides = "collect") + 
  plot_annotation(title = "Standard error (extremal coefficient) \n \n", theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
ggsave("results/Figures/chapter_4_bivariate_analysis/save_extremal_coefficient_std.pdf", width = 30, height = 45, units = "cm", plot = p)




# ------------------ Plot station vs other stations ---------------------

file_name = "results/estimates/bivariate/pred_model_bivariate_69507_full_stations.csv"
data_pred <- read.csv(file_name)
colnames(data_pred) <- c("dur", "s1", "id", "mean", "upper", "lower", "empirical")
data_pred <- merge(data_pred, list_of_station_coordinates_df, by = "id")

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
    V <- data_pred[data_pred$id == other_stations_of_interest[s] & data_pred$dur == durations[dur], "mean"]
    V_lower <- data_pred[data_pred$id == other_stations_of_interest[s] & data_pred$dur == durations[dur], "lower"]
    V_upper <- data_pred[data_pred$id == other_stations_of_interest[s] & data_pred$dur == durations[dur], "upper"]
    V_empirical <- data_pred[data_pred$id == other_stations_of_interest[s] & data_pred$dur == durations[dur], "empirical"]
    df_l[[dur]] <- data.frame(return_periods = return_periods, 
                              return_levels = return_levels_frechet,
                              prob = res,
                              duration = durations[dur],
                              fitted = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V/return_levels_frechet),
                              lower = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V_lower/return_levels_frechet),
                              upper = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V_upper/return_levels_frechet),
                              empirical = 1.0 - 2*exp(-1/return_levels_frechet) + exp(-V_empirical/return_levels_frechet)
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
    geom_line(data = df_l, aes(x = return_periods, y = empirical, group = factor(duration), color = factor(duration))
              ,colour = "red"
              ,size = 0.6
              ,linetype = 2
    ) + 
    scale_color_viridis(name = "Duration", discrete = T) + 
    scale_x_continuous(trans = "log10") + 
    # scale_y_continuous(trans = "log10") + 
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
ggsave("results/Figures/chapter_4_bivariate_analysis/joint_prob_bivariate_1_60.pdf", width = 40, height = 15, units = "cm", plot = p_joint_prob)

p_joint_prob_bis <- wrap_plots(list_plots_pairwise, nrow = 1, guides = "collect")
ggsave("results/Figures/chapter_4_bivariate_analysis/joint_prob_bivariate_2_60.pdf", width = 40, height = 15, units = "cm", plot = p_joint_prob_bis)



# ------------------ CRPS with distance ------------

stations_interest <- c(69507, 77001, 75022, 98006, 76026)
bin_size <- 20

list_res_biv_1 <- list()
list_res_biv_2 <- list()

list_res_max_stable_1 <- list()
list_res_max_stable_2 <- list()
for (s in seq_along(stations_interest)){
  station_main <- stations_interest[s]
  
  data_pred <- read.csv(paste("results/estimates/bivariate/pred_model_bivariate_", station_main, "_full_stations.csv", sep = ""))
  colnames(data_pred) <- c("dur", "s1", "id", "mean", "upper", "lower", "empirical")
  data_pred <- merge(data_pred, list_of_station_coordinates_df, by = "id")
  
  data_pred$dist = sqrt((data_pred$X - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, "X"])**2 + 
                          (data_pred$Y - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, "Y"])**2) * 111
  data_pred$diff_ratio = log(data_pred$mean/data_pred$empirical)
  data_pred$std <- (data_pred$upper - data_pred$lower)/(2*1.96)
  
  rescaled <- (data_pred$empirical - data_pred$mean)/data_pred$std
  data_pred$CRPS <- data_pred$std*(-1/sqrt(pi) + 2*dnorm(rescaled) + rescaled*(2*pnorm(rescaled)-1))
  data_pred$mean_error <- (data_pred$mean - data_pred$empirical)**2
  
  data_pred$distance_bin <- floor(data_pred$dist /bin_size) * bin_size
  # bin_version <- data_pred[data_pred$id != station_main, ] %>% group_by(dur, distance_bin) %>%
  #   summarise(CRPS_mean = mean(CRPS), CRPS_upper = quantile(CRPS, 0.975), CRPS_lower = quantile(CRPS, 0.025)) %>%
  #   ungroup() %>% as.data.frame()
  
  bin_version_all <- data_pred[data_pred$id != station_main, ] %>% group_by(distance_bin) %>%
    summarise(CRPS_mean = mean(CRPS), CRPS_upper = quantile(CRPS, 0.975), CRPS_lower = quantile(CRPS, 0.025), mean_error = mean(mean_error)) %>%
    ungroup() %>% as.data.frame()
  
  list_res_biv_1[[s]] <- data_pred
  list_res_biv_2[[s]] <- bin_version_all
  list_res_biv_2[[s]]$id <- station_main
  
  data_pred_max_stable <- read.csv("results/estimates/max_stable/pred_model_max_stable_stations.csv")
  colnames(data_pred_max_stable) <- c("s1", "id", "dur", "rho", "var_rho", "V", "var_V")
  data_pred_max_stable <- merge(data_pred_max_stable, list_of_station_coordinates_df, by = "id")
  data_pred_max_stable <- merge(data_pred_max_stable, data_pred[,c("id", "s1", "empirical", "dur")], by = c("id", "s1", "dur"))
  data_pred_max_stable$std <- sqrt(data_pred_max_stable$var_V)
  data_pred_max_stable$dist = sqrt((data_pred_max_stable$X - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, "X"])**2 + 
                                     (data_pred_max_stable$Y - list_of_station_coordinates_df[list_of_station_coordinates_df$id == station_main, "Y"])**2) * 111
  data_pred_max_stable$diff_ratio = log(data_pred_max_stable$V/data_pred_max_stable$empirical)
  data_pred_max_stable$distance_bin <- floor(data_pred_max_stable$dist /bin_size) * bin_size
  
  rescaled <- (data_pred_max_stable$empirical - data_pred_max_stable$V)/data_pred_max_stable$std
  data_pred_max_stable$CRPS <- data_pred_max_stable$std*(-1/sqrt(pi) + 2*dnorm(rescaled) + rescaled*(2*pnorm(rescaled)-1))
  data_pred_max_stable$mean_error <- (data_pred_max_stable$V - data_pred_max_stable$empirical)**2
  
  # bin_version_max_stable <- data_pred_max_stable[data_pred_max_stable$id != station_main, ] %>% group_by(dur, distance_bin) %>%
  #   summarise(CRPS_mean = mean(CRPS), CRPS_upper = quantile(CRPS, 0.975), CRPS_lower = quantile(CRPS, 0.025)) %>%
  #   ungroup() %>% as.data.frame()
  
  bin_version_all_max_stable <- data_pred_max_stable[data_pred_max_stable$id != station_main, ] %>% group_by(distance_bin) %>%
    summarise(CRPS_mean = mean(CRPS), CRPS_upper = quantile(CRPS, 0.975), CRPS_lower = quantile(CRPS, 0.025), mean_error = mean(mean_error)) %>%
    ungroup() %>% as.data.frame()
  
  list_res_max_stable_1[[s]] <- data_pred_max_stable
  list_res_max_stable_2[[s]] <- bin_version_all_max_stable
  list_res_max_stable_2[[s]]$id <- station_main
}

list_res_max_stable_1 <- do.call(rbind, list_res_max_stable_1)
list_res_max_stable_2 <- do.call(rbind, list_res_max_stable_2)
list_res_biv_1 <- do.call(rbind, list_res_biv_1)
list_res_biv_2 <- do.call(rbind, list_res_biv_2)


# list_res_biv_2 <- list_res_biv_2 %>% 
#   group_by(distance_bin) %>% 
#   summarise(CRPS_mean = mean(CRPS_mean), CRPS_upper = mean(CRPS_upper), CRPS_lower = mean(CRPS_lower), mean_error = mean(mean_error)) %>%
#   ungroup() %>%
#   as.data.frame()
# 
# list_res_max_stable_2 <- list_res_max_stable_2 %>% 
#   group_by(distance_bin) %>% 
#   summarise(CRPS_mean = mean(CRPS_mean), CRPS_upper = mean(CRPS_upper), CRPS_lower = mean(CRPS_lower), mean_error = mean(mean_error)) %>%
#   ungroup() %>%
#   as.data.frame()


p1 <- ggplot() + 
  geom_boxplot(data = list_res_biv_1, aes(x = dist, y = CRPS, group = distance_bin)) +
  geom_hline(yintercept = 0, colour = "red") + 
  # geom_line(data = bin_version, aes(x = distance_bin, y = CRPS_mean, group = dur, colour = factor(dur))) + 
  # geom_ribbon(data = bin_version, aes(x = distance_bin, ymin = CRPS_lower, ymax = CRPS_upper, group = dur, colour = factor(dur)), alpha = 0.2) + 
  # geom_line(data = list_res_biv_2, aes(x = distance_bin, y = CRPS_mean), colour = "red", size = 0.6) + 
  # geom_ribbon(data = list_res_biv_2, aes(x = distance_bin, ymin = CRPS_lower, ymax = CRPS_upper), alpha = 0.2) + 
  
  scale_colour_viridis(discrete = T, name = "Duration") +
  scale_y_continuous(limits = c(0,0.5)) + 
  theme_for_the_plots + 
  labs(x = "Distance [km]", y = "CRPS", title = "Extended bivariate approach") + 
  theme(legend.position = "right",
        plot.margin = margin(0.2, 0.1, 0.2, 0.1, "cm"))

p2 <- ggplot() + 
  geom_boxplot(data = list_res_max_stable_1, aes(x = dist, y = CRPS, group = distance_bin)) +
  geom_hline(yintercept = 0, colour = "red") + 
  
  # geom_line(data = list_res_max_stable_2, aes(x = distance_bin, y = CRPS_mean), colour = "red", size = 0.6) + 
  # geom_ribbon(data = list_res_max_stable_2, aes(x = distance_bin, ymin = CRPS_lower, ymax = CRPS_upper), alpha = 0.2, fill = "red") + 
  
  scale_colour_viridis(discrete = T, name = "Duration") + 
  scale_y_continuous(limits = c(0,0.5)) + 
  theme_for_the_plots + 
  labs(x = "Distance [km]", y = "CRPS", title = "Non-stationary max-stable process") + 
  theme(legend.position = "right",
        plot.margin = margin(0.2, 0.1, 0.2, 0.1, "cm"))

pfinal <- p1 + p2 + plot_layout(nrow = 2)

ggsave("results/Figures/chapter_4_bivariate_analysis/max_stable_VS_bivariate_2.pdf", width = 40, height = 25, units = "cm", plot = pfinal)

# ------------------ Some plots ---------------------
list_stations <- c(75022, 84118, 77001, 69507, 98006, 76026)

pred_list <- list()
for (s in seq_along(list_stations)){
  pred_list[[s]] <- read.csv(paste("results/estimates/bivariate/pred_model_bivariate_",list_stations[s],"_full.csv", sep = ""))
  pred_list[[s]]$id <- list_stations[s]
}
pred_list <- do.call(rbind, pred_list)

pt.lim = data.frame(xlim=c(-86, -82.5), ylim=c(8, 11.2))
pred_list$std <- (pred_list$upper - pred_list$lower)/(2*1.96)
pred_list <- pred_list[pred_list$X > pt.lim$xlim[1] & 
                         pred_list$X < pt.lim$xlim[2] & 
                         pred_list$Y > pt.lim$ylim[1] & 
                         pred_list$Y < pt.lim$ylim[2], ]

data_to_plot <- pred_list[pred_list$id == list_stations[4], c("X", "Y", "mean")]
colnames(data_to_plot) <- c("long", "lat", "mean")
data_to_plot$rounded <- sapply(data_to_plot$mean, round_any, 0.05)

data_to_plot <- merge(data_to_plot, elevation_costa_rica_df, by = c("long", "lat"))


contours_sf <- rasterToContour(costa_rica_tif, 
                               levels = seq(1000, max(elevation_costa_rica_df[elevation_costa_rica_df$inside_costa_rica == TRUE,]), by = 1000)) %>% st_as_sf

p <- ggplot() + coord_quickmap() + 
  theme_for_the_plots + 
  # geom_raster(data = data_to_plot[data_to_plot$inside_costa_rica == TRUE,], aes_string(x = "long", y = "lat", alpha = "mean"), fill = "#EEC60E") + 
  # scale_alpha_continuous(range = c(1, 0)) + 
  geom_raster(data = data_to_plot[data_to_plot$inside_costa_rica == TRUE,], aes_string(x = "long", y = "lat", fill = "mean")) + 
  # scale_fill_gradient2(high = "white", low = "#EEC60E")+
  scale_fill_gradient(high = "white", low = "#EEC60E") + 
  
  # scale_fill_viridis(option = "magma", name = "Extremal coefficient", direction = -1,
  #                    # trans = "log10"
  # ) + 
  # geom_sf(data = contours_sf, linetype = 1, color = "white") +
  scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  geom_sf(data = border_costa_rica, colour = "white", fill = "transparent", size = 1.5) + 
  # geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% unique(rainfall_data_clean$id),],
  #            mapping = aes(x = X, y = Y),
  #            size = 0.5, colour = "green",
  # ) +
  geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id == list_stations[4],],
             mapping = aes(x = X, y = Y),
             size = 2, colour = "white") +s
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

ggsave("results/Figures/chapter_4_bivariate_analysis/tes2.png", p, bg = "transparent")




