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
# using("raster", "sf", "secr", "geojsonio", "data.table")
using("ggstar", "ggplot2", "ggrepel", "scales", "patchwork", "plotly", "viridis", "tidyverse")

setwd("/Users/Antoine/Desktop/Master Thesis Extremes Costa Rica ")
load("data/rainfall_data.RData")

using("corrr")

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

# =============================== Clustering Analysis ===================================================================

data_of_interest <- rainfall_data_clean
data_of_interest$month <- month.abb[as.numeric(format(data_of_interest$date, "%m"))]
data_of_interest$month <- factor(data_of_interest$month, levels = month.abb)
data_of_interest$year <- format(data_of_interest$date, "%Y")

# compute monthly and annual maxima
data_monthly_maxima <- aggregate(int ~ year + id + month + dur, data_of_interest, max)
data_annual_maxima <- aggregate(int ~ year + id + dur, data_of_interest, max)

stations_ids <- unique(data_of_interest$id)

#' Function to plot a set of data on a map (for example GEV parameters at some stations), and extend them with knn to any location
plot_map_data <- function(observed_data, k, interpolation_df = NULL, list_covariates = NULL, ...){
  observed_data_plots <- observed_data
  if (!(c("var_interest", "X", "Y") %in% colnames(observed_data_plots))) stop("Unknown response variable or coordinate variables. Please, name them as 'var_interest', 'X' and 'Y'.")
  
  if (!is.null(interpolation_df) & !is.null(list_covariates)){
    if (!(colnames(interpolation_df) %in% colnames(observed_data_plots))) stop("Missing columns!")
    if (!(list_covariates %in% colnames(interpolation_df))) stop("Unknown covariate names!")
    dat_for_knn_training <- observed_data_plots
    pred_v <- interpolation_df
    pred_v <- pred_v[pred_v$alt >= 0, ]
    
    for (j in seq_along(list_covariates)){
      pred_v[paste(list_covariates[j], ".norm", sep = "")] <- normalize(pred_v[list_covariates[j]], min(pred_v[list_covariates[j]]), max(pred_v[list_covariates[j]]))
      observed_data_plots[paste(list_covariates[j], ".norm", sep = "")] <- normalize(dat_for_knn_training[list_covariates[j]], min(pred_v[list_covariates[j]]), max(pred_v[list_covariates[j]]))
    }
    
    if (is.factor(observed_data_plots$var_interest)){
      model_knn <- FNN::knn(train = observed_data_plots[paste(list_covariates, ".norm", sep = "")], 
                            test = pred_v[paste(list_covariates, ".norm", sep = "")], 
                            cl = observed_data_plots$var_interest, 
                            k = k)
      pred_v$pred <- as.factor(as.numeric(model_knn)) 
    }else{
      model_knn <- knn.reg(train = observed_data_plots[paste(list_covariates, ".norm", sep = "")], 
                           test = pred_v[paste(list_covariates, ".norm", sep = "")], 
                           y = observed_data_plots$var_interest, 
                           k = k)
      pred_v$pred <- model_knn$pred
    }
  }
  
  p <- ggplot() + coord_quickmap() + 
    theme_for_the_plots
  
  
  if (!is.null(interpolation_df) & !is.null(list_covariates)){
    p <- p + geom_raster(data = pred_v,
                         aes(x = X, y = Y, fill = pred)) +
      scale_fill_viridis(name = "Classification", option = "viridis", discrete = is.factor(observed_data_plots$var_interest))
  }
  p <- p + 
    geom_point(data = observed_data, aes(x = X, y = Y, color = var_interest, 
                                         text=paste('</br>ID: ', id,'</br>long: ', round(X, digits = 2),'</br>lat: ',round(Y, digits = 2))), ...) +
    scale_color_viridis(name = "Clustering", option ="viridis", discrete = is.factor(observed_data_plots$var_interest))
  
  return(p)
}

#' Function to perform hierarchical clustering given a distance matrix, and extend it to any location
get_map <- function(mat_, title_ = "", subtitle_ = ""){
  corr_mat <- mat_
  corr_mat[is.na(corr_mat)] <- mean(corr_mat, na.rm = TRUE)
  
  # Clustering
  clustering_res <- hclust(as.dist(corr_mat), method = "ward.D")
  
  cut_avg <- cutree(clustering_res, k = nb_clusters)
  # plot(clustering_res)
  # rect.hclust(clustering_res , k = nb_clusters, border = 2:6)
  # abline(h = 3, col = 'red')
  
  cut_avg <- as.data.frame(cut_avg)
  cut_avg$id <- rownames(cut_avg)
  colnames(cut_avg) <- c("var_interest", "id")
  cut_avg <- merge(cut_avg, list_of_station_coordinates_df, by = "id")
  
  interpolation_df = elevation_costa_rica_df
  
  interpolation_df <- interpolation_df[interpolation_df$inside_costa_rica, c("long", "lat", "alt")]
  colnames(interpolation_df) <- c("X", "Y", "alt")
  cut_avg$var_interest <- as.factor(cut_avg$var_interest)
  
  res_pred <- plot_map_data_bis(observed_data = cut_avg, 
                                k = 3, 
                                interpolation_df = interpolation_df, 
                                list_covariates = c("X", "Y", "alt"))
  
  return(ggplot() + 
           coord_quickmap() + 
           geom_raster(data = res_pred[[1]],
                       aes(x = X, y = Y, fill = pred)) +
           scale_fill_viridis(name = "Classification", option = "viridis", discrete = is.factor(res_pred[[2]]$var_interest)) + 
           geom_point(data = cut_avg, aes(x = X, y = Y, color = var_interest, 
                                          text=paste('</br>ID: ', id,'</br>long: ', round(X, digits = 2),'</br>lat: ',round(Y, digits = 2)))) +
           scale_color_viridis(name = "Clustering", option ="viridis", discrete = is.factor(res_pred[[2]]$var_interest)) + 
           scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
           scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
           geom_sf(data = border_costa_rica, colour = "black", fill = "transparent") + 
           geom_sf(data = sf_basins, colour = "black", fill = "transparent") + 
           geom_point(data = cut_avg,
                      mapping = aes(x = X, y = Y, text=paste('</br>ID: ', id)),
                      color = "red", shape = 1, size = 2, stroke = 1) +
           labs(title = title_, subtitle=subtitle_, x="longitude", y="latitude") + 
           theme(plot.background = element_rect(fill = font_color_plot),
                 panel.background = element_rect(fill = "white", colour = "black"),
                 panel.grid.major = element_line(colour = "transparent"),
                 panel.grid.minor = element_line(colour = "transparent"),
                 # legend.position = c(.95, .95),
                 legend.justification = c("right", "top"),
                 legend.box.just = "right",
                 legend.margin = margin(6, 6, 6, 6),
                 plot.margin = unit(c(1,1,1,1), "cm"),
                 legend.key = element_rect(colour = "transparent", fill = "transparent"),
                 legend.position="none", legend.direction = "vertical", legend.box = "horizontal"))
}

#' Same as plot_map_data, but with a different package for the kNN algorithm
plot_map_data_bis <- function(observed_data, k, interpolation_df = NULL, list_covariates = NULL, ...){
  observed_data_plots <- observed_data
  if (!(c("var_interest", "X", "Y") %in% colnames(observed_data_plots))) stop("Unknown response variable or coordinate variables. Please, name them as 'var_interest', 'X' and 'Y'.")
  
  if (!is.null(interpolation_df) & !is.null(list_covariates)){
    if (!(colnames(interpolation_df) %in% colnames(observed_data_plots))) stop("Missing columns!")
    if (!(list_covariates %in% colnames(interpolation_df))) stop("Unknown covariate names!")
    dat_for_knn_training <- observed_data_plots
    pred_v <- interpolation_df
    pred_v <- pred_v[pred_v$alt >= 0, ]
    
    for (j in seq_along(list_covariates)){
      pred_v[paste(list_covariates[j], ".norm", sep = "")] <- normalize(pred_v[list_covariates[j]], min(pred_v[list_covariates[j]]), max(pred_v[list_covariates[j]]))
      observed_data_plots[paste(list_covariates[j], ".norm", sep = "")] <- normalize(dat_for_knn_training[list_covariates[j]], min(pred_v[list_covariates[j]]), max(pred_v[list_covariates[j]]))
    }
    
    if (is.factor(observed_data_plots$var_interest)){
      model_knn <- FNN::knn(train = observed_data_plots[paste(list_covariates, ".norm", sep = "")], 
                            test = pred_v[paste(list_covariates, ".norm", sep = "")], 
                            cl = observed_data_plots$var_interest, 
                            k = k)
      pred_v$pred <- as.factor(as.numeric(model_knn)) 
    }else{
      model_knn <- knn.reg(train = observed_data_plots[paste(list_covariates, ".norm", sep = "")], 
                           test = pred_v[paste(list_covariates, ".norm", sep = "")], 
                           y = observed_data_plots$var_interest, 
                           k = k)
      pred_v$pred <- model_knn$pred
    }
  }
  
  return(list(pred_v, observed_data_plots))
}


normalize <- function(x, vmin, vmax) {
  return ((x - vmin) / (vmax - vmin)) 
}

# ------------------------------------------- 1) F madogram approach ------------------------------------------------
fmadogram_custom <- function(df){
  df_intersected <- df[intersect(which(!is.na(df[,1])), which(!is.na(df[,2]))),]
  cdf1 <- ecdf(df_intersected[,1])
  cdf2 <- ecdf(df_intersected[,2])
  plot(cdf1(df_intersected[,1]), cdf2(df_intersected[,2]))
  
  return(sum(abs(cdf1(df_intersected[,1]) - cdf2(df_intersected[,2])))/(2*length(df_intersected[,1])))
}

fmadogram_custom_bis <- function(vec1, vec2){
  indices <- intersect(which(!is.na(vec1)), which(!is.na(vec2)))
  if (length(indices) == 0){
    return(1/6)
  }
  cdf1 <- ecdf(vec1[indices])
  cdf2 <- ecdf(vec2[indices])
  # plot(cdf1(vec1[indices]), cdf2(vec2[indices]))
  
  return(sum(abs(cdf1(vec1[indices]) - cdf2(vec2[indices])))/(2*length(indices)))
}

duration_ids <- sort(unique(data_of_interest$dur))

nb_clusters <- 2

# results_all_durations <- list()
# results_all_durations_plot <- list()
corr_pair_list_f_madogram <- list()
for (dur in seq_along(duration_ids)){
  print(duration_ids[dur])
  dat <- data_monthly_maxima[data_monthly_maxima$id %in% stations_ids & data_monthly_maxima$dur == duration_ids[dur], ]
  
  dat_split <- split(dat[c("year", "month", "id", "int")], list(dat$id))
  for (j in seq_along(names(dat_split))){
    id <- dat_split[[names(dat_split)[j]]]$id[1]
    colnames(dat_split[[names(dat_split)[j]]]) <- c("year", "month", "id", id)
    dat_split[[names(dat_split)[j]]] <- dat_split[[names(dat_split)[j]]][c("year", "month", id)]
  }
  
  dat_merged <- dat_split %>% purrr::reduce(full_join, by = c("year", "month"))     # make sure that values for x and y are occuring for during the same months
  dat_merged <- dat_merged[as.character(stations_ids)]
  corr_pair <- as.data.frame(colpair_map(dat_merged, fmadogram_custom_bis, .diagonal = 0))  # compute the distance matrix between each observation
  corr_pair <- corr_pair[,c(2:dim(corr_pair)[2])]  # remove first column
  
  corr_pair_list_f_madogram[[duration_ids[dur]]] <- corr_pair
  
  # - - - - - - - - Clustering - - - - - - - -
  # clustering_res <- hclust(as.dist(corr_pair), method = "ward.D")
  # 
  # cut_avg <- cutree(clustering_res, k = nb_clusters)
  # # plot(clustering_res)
  # # rect.hclust(clustering_res , k = nb_clusters, border = 2:6)
  # # abline(h = 3, col = 'red')
  # 
  # cut_avg <- as.data.frame(cut_avg)
  # cut_avg$id <- rownames(cut_avg)
  # colnames(cut_avg) <- c("var_interest", "id")
  # cut_avg <- merge(cut_avg, list_of_station_coordinates_df, by = "id")
  # 
  # interpolation_df = elevation_costa_rica_df
  # colnames(interpolation_df) <- c("X", "Y", "alt")
  # cut_avg$var_interest <- as.factor(cut_avg$var_interest)
  # 
  # results_all_durations[[duration_ids[dur]]] <- cut_avg
  # 
  # plotCostaRica_with_alt <- plot_map_data(observed_data = cut_avg, 
  #                                         k = 5, 
  #                                         interpolation_df = interpolation_df, 
  #                                         list_covariates = c("X", "Y", "alt"))
  # plotCostaRica_with_alt <- plotCostaRica_with_alt + geom_sf(data = contours_sf, linetype = 1, color = "black") +
  #   scale_x_continuous(limits = c(pt.lim$xlim[1], pt.lim$xlim[2]), expand = c(0, 0)) +
  #   scale_y_continuous(limits = c(pt.lim$ylim[1], pt.lim$ylim[2]), expand = c(0, 0)) + 
  #   geom_sf(data = border_costa_rica, colour = "black", fill = "transparent") + 
  #   # geom_point(data = list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% stations_ids,],
  #   #            mapping = aes(x = X, y = Y, text=paste('</br>ID: ', id)),
  #   #            color = "red", shape = 1, size = 3, stroke = 1) +
  #   labs(title = "Costa Rica classification \n", subtitle=paste("Duration: ", duration_ids[dur], " minutes", sep = ""), x="longitude", y="latitude") + 
  #   theme(legend.position="right", legend.direction = "vertical", legend.box = "horizontal")
  # 
  # results_all_durations_plot[[dur]] <- plotCostaRica_with_alt
}

# results_all_durations_plot[[10]]

# ------------------------------------------- 2) TPDM approach ------------------------------------------------------
inverse_fct_f_scale2 <- function(y){
  return((-log(y))**(-1/2))
}

TPDM_estim <- function(vec1, vec2, q = 0.95){
  rt <- sqrt(vec1**2 + vec2**2)
  wt1 <- vec1/rt; wt2 <- vec2/rt
  th <- stats::quantile(rt, probs = q, na.rm = T)
  i <- as.numeric(rt > th)
  i[is.na(i)] <- 0
  sigma <- sum(wt1 * wt2 * i, na.rm = T)
  return(2*sigma/sum(i))
}

duration_ids <- sort(unique(data_of_interest$dur))

corr_pair_list_TPDM <- list()
for (dur in seq_along(duration_ids)){
  print(duration_ids[dur])
  # monthly data
  dat <- data_monthly_maxima[data_monthly_maxima$id %in% stations_ids & data_monthly_maxima$dur == duration_ids[dur], ]
  dat_split <- split(dat[c("year", "month", "id", "int")], list(dat$id))
  for (j in seq_along(names(dat_split))){
    id <- dat_split[[names(dat_split)[j]]]$id[1]
    colnames(dat_split[[names(dat_split)[j]]]) <- c("year", "month", "id", id)
    dat_split[[names(dat_split)[j]]] <- dat_split[[names(dat_split)[j]]][c("year", "month", id)]
  }
  dat_merged <- dat_split %>% purrr::reduce(full_join, by = c("year", "month"))     # make sure that values for x and y are occuring for during the same months
  
  
  dat_merged_standardized <- dat_merged[,-1]
  for (j in c(1:dim(dat_merged_standardized)[2])){
    dd <- dat_merged_standardized[,j]
    cdf <- ecdf(dd[!is.na(dd)])
    dat_merged_standardized[,j] <- inverse_fct_f_scale2(cdf(dd))
  }
  # qs <- c(seq(0, 0.94, 0.02), seq(0.94, 0.995, 0.005), seq(0.995, 0.9995, 0.0005), seq(0.9995, 1.0, 0.00001))
  # res <- c()
  # for (j in seq_along(qs)){
  #   res <- c(res,  TPDM_estim(dat_merged_standardized[,4], dat_merged_standardized[,2],  q = qs[j]))
  # }
  # plot(qs, res, ylim = c(0, 1))
  # abline(v = 0.95, col = "red")
  
  corr_pair <- as.data.frame(colpair_map(dat_merged_standardized, TPDM_estim, .diagonal = T))
  corr_pair <- sapply(corr_pair[,-1],as.numeric)
  for (j in c(1:dim(dat_merged_standardized)[2])){
    corr_pair[j,j] <- TPDM_estim(dat_merged_standardized[,j], dat_merged_standardized[,j])
  }
  
  corr_pair_list_TPDM[[duration_ids[dur]]] <- -log(corr_pair[c(2:dim(corr_pair)[1]), c(2:dim(corr_pair)[2])])
}




# ------------------------------------------- 3) plot map -------------------------------------------

durations_selected <- c(10, 60, 1440)
list_pair <- corr_pair_list_f_madogram
nb_clusters <- 10
use_knn <- TRUE


map_10_F_mad <- get_map(corr_pair_list_f_madogram[[10]])
map_60_F_mad <- get_map(corr_pair_list_f_madogram[[60]])
map_1440_F_mad <- get_map(corr_pair_list_f_madogram[[1440]])
map_10_TPDM <- get_map(corr_pair_list_TPDM[[10]])
map_60_TPDM <- get_map(corr_pair_list_TPDM[[60]])
map_1440_TPDM <- get_map(corr_pair_list_TPDM[[1440]])

p1 <- map_10_F_mad + 
  theme_void() + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "horizontal", 
        axis.title.y = element_text(colour = "black", angle = 90, size = 20),
        title = element_text(colour = "black", angle = 0, size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(y = "Duration 10 minutes\n", title = "F-madogram")
p2 <- map_60_F_mad + 
  theme_void() + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "horizontal", 
        axis.title.y = element_text(colour = "black", angle = 90, size = 20),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(y = "Duration 60 minutes\n")
p3 <- map_1440_F_mad + 
  theme_void() + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "horizontal", 
        axis.title.y = element_text(colour = "black", angle = 90, size = 20),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(y = "Duration 1440 minutes\n")
p4 <- map_10_TPDM + 
  theme_void() + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "horizontal", 
        title = element_text(colour = "black", angle = 0, size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  labs(title = "TPDM")
p5 <- map_60_TPDM + 
  theme_void() + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "horizontal") +
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) 
p6 <- map_1440_TPDM + 
  theme_void() + 
  theme(legend.position="none", legend.direction = "vertical", legend.box = "horizontal") +
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) 


p <- p1+p4+p2+p5+p3+p6+
  plot_layout(nrow = 3, ncol = 2)

ggsave("results/Figures/chapter_6_clustering_analysis/clusters_2.pdf", width = 40, height = 60, units = "cm", plot = p)



# ------------------------------------------- 4) Clustering with all durations at the same time ---------------------
nb_clusters_ <- 3

dat <- data_monthly_maxima

dat_split <- split(dat[c("year", "month", "dur", "id", "int")], list(dat$id))
for (j in seq_along(names(dat_split))){
  id <- dat_split[[names(dat_split)[j]]]$id[1]
  colnames(dat_split[[names(dat_split)[j]]]) <- c("year", "month", "dur", "id", id)
  dat_split[[names(dat_split)[j]]] <- dat_split[[names(dat_split)[j]]][c("year", "month", "dur", id)]
}

dat_merged <- dat_split %>% purrr::reduce(full_join, by = c("year", "month", "dur"))     # make sure that values for x and y are occuring for during the same months
dat_merged <- dat_merged[as.character(stations_ids)]
corr_pair <- as.data.frame(colpair_map(dat_merged, fmadogram_custom_bis, .diagonal = 0))  # compute the distance matrix between each observation
corr_pair <- corr_pair[,c(2:dim(corr_pair)[2])]  # remove first column

map_all_durations <- get_map(corr_pair)

map_all_durations <- map_all_durations  +
  scale_fill_manual(breaks = c(1, 2, 3), values = c("#52b788", "#ffb703", "#1a759f")) + 
  scale_colour_manual(breaks = c(1, 2, 3), values = c("#52b788", "#ffb703", "#1a759f")) + 
  labs(title = "Hierarchical clustering of the extremal coefficient", subtitle = NULL) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = -83.3, y = 10.5, label = "Zone 1", colour = "#52b788") + 
  annotate("text", x = -85.5, y = 10.4, label = "Zone 2", colour = "white") + 
  annotate("text", x = -84, y = 9.2, label = "Zone 3", colour = "#1a759f")

ggsave("results/Figures/chapter_6_clustering_analysis/clustering_all_durations_3.pdf", width = 20, height = 20, units = "cm", plot = map_all_durations)
