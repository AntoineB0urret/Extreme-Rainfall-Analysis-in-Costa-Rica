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

using("evgam", "evd", "goftest", "reshape2", "KernelKnn")

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

# =============================== Univariate Analysis ===================================================================
# ------------------------------------------- Data monthly maxima -------------------------------------------------------

data_of_interest <- rainfall_data_clean
data_of_interest$month <- month.abb[as.numeric(format(data_of_interest$date, "%m"))]
data_of_interest$month <- factor(data_of_interest$month, levels = month.abb)
data_of_interest$year <- format(data_of_interest$date, "%Y")

# compute monthly and annual maxima
data_monthly_maxima <- aggregate(int ~ year + id + month + dur, data_of_interest, max)
data_annual_maxima <- aggregate(int ~ year + id + dur, data_of_interest, max)

# another way to compute monthly and annual maxima, but here we also collect the date and the number of observations per month
data_monthly_maxima_with_exact_date <- data_of_interest %>% group_by(year, month, dur, id) %>%
  summarize(exact_date = date[which(int == max(int))], int = max(int)) %>%
  ungroup() %>% as.data.frame()

data_monthly_maxima_with_nb_of_obs <- data_of_interest %>% group_by(year, month, dur, id) %>%
  summarize(nb_obs = length(int), int = max(int)) %>%
  ungroup() %>% as.data.frame()

data_annual_maxima_with_exact_date <- data_of_interest %>% group_by(year, dur, id) %>%
  summarize(exact_date = date[which(int == max(int))], int = max(int)) %>%
  ungroup() %>% as.data.frame()

#                                                                   i.1) (FIGURE) plot monthly and annual maxima, with nb obs ------
#' Plot monthly and annual maxima from a specific station, along with the number of observations (days) used to compute the maximum.

station_id <- 69507 # 75022 #98006 #76026 # 77001 # 69507, 84118
duration_id <- 60

original_dat <- data_of_interest[data_of_interest$id == station_id, ]
dat <- data_monthly_maxima[data_monthly_maxima$id == station_id, ]
dat_ <- data_monthly_maxima_with_exact_date[data_monthly_maxima_with_exact_date$id == station_id, ]
dat$month_id <- as.numeric(dat$month)

nb_obs_per_month <- data_of_interest %>%
  group_by(year, month, dur, id) %>%
  summarise(
    nb = length(int)
  ) %>% ungroup() %>% as.data.frame()
nb_obs_per_month$dates <- as.Date(paste(nb_obs_per_month$year, as.numeric(nb_obs_per_month$month), "15", sep = "-")) # to center around the month...

nb_obs_per_year <- data_of_interest %>%
  group_by(year, dur, id) %>%
  summarise(
    nb = length(int)
  ) %>% ungroup() %>% as.data.frame()
nb_obs_per_year$dates <- as.Date(paste(nb_obs_per_year$year, "07-01", sep = "-"))   # to center around the year...


plot1 <- ggplot(data = original_dat[original_dat$dur == duration_id, ], aes(x = date)) +
  geom_point(aes(y = int), alpha = 1, color = "black") +
  geom_point(data = dat_[dat_$dur == duration_id, ], aes(x = exact_date, y = int), alpha = 1, color = "red", size = 2) +
  scale_y_continuous(trans='log10') +
  # geom_vline(xintercept =  missing_dates, color = "red") +
  scale_x_date(name = 'time', date_breaks = '1 year', date_labels = '%Y') +
  theme_for_the_plots +
  labs(title=sprintf("Intensity (station %s) and monthly maxima (duration: %s)", station_id, duration_id), x="Time", y="intensity") +
  theme(plot.margin = margin(1, 2, 0, 2, "cm"))

bar_plot_1 <- ggplot()+
  geom_bar(data = nb_obs_per_month[nb_obs_per_month$id == station_id & nb_obs_per_month$dur == duration_id, ], mapping = aes(x = dates, y = nb), stat="identity", fill="black")+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y", limits = as.Date(layer_scales(plot1)$x$range$range, origin = "1970-01-01")) +
  theme_for_the_plots +
  theme(plot.margin = margin(0, 2, 0, 2, "cm"))

plot2 <- ggplot(data = original_dat[original_dat$dur == duration_id, ], aes(x = date)) +
  geom_point(aes(y = int), alpha = 1, color = "black") +
  geom_point(data = data_annual_maxima_with_exact_date[data_annual_maxima_with_exact_date$id == station_id & data_annual_maxima_with_exact_date$dur == duration_id, ], aes(x = exact_date, y = int), alpha = 1, color = "red", size = 2) +
  scale_y_continuous(trans='log10') +
  # geom_vline(xintercept =  missing_dates, color = "red") +
  scale_x_date(name = 'time', date_breaks = '1 year',date_labels = '%Y') +
  theme_for_the_plots +
  labs(title=sprintf("Intensity (station %s) and annual maxima (duration: %s)", station_id, duration_id), x="Time", y="intensity") +
  theme(plot.margin = margin(2, 2, 0, 2, "cm"))

bar_plot_2 <- ggplot()+
  geom_bar(data = nb_obs_per_year[nb_obs_per_year$id == station_id & nb_obs_per_year$dur == duration_id, ], mapping = aes(x = dates, y = nb), stat="identity", fill="black")+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y", limits = as.Date(layer_scales(plot2)$x$range$range, origin = "1970-01-01")) +
  theme_for_the_plots +
  theme(plot.margin = margin(0, 2, 1, 2, "cm"))

# plot_grid(plot1, plot2, nrow = 2)
plot1 + bar_plot_1 + plot2 + bar_plot_2 + plot_layout(ncol=1,heights=c(3, 1, 3, 1))

#                                                                   i.2) (FIGURE) boxplot intensities monthly and daily ------------

#' Boxplot of monthly and daily maxima, for each month and duration, in order to show the seasonality.

log_scale_boxplot <- TRUE
boxplot_original <- ggplot() +
  geom_boxplot(data = dat, mapping = aes(x = month, y = int, fill = as.factor(dur))) +
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "D")+
  labs(title=sprintf("Monthly max intensities, for each month (station %s)", station_id), x="month", y="intensity [mm/hr]") +
  theme_for_the_plots + 
  theme(legend.position="right")
if (log_scale_boxplot){boxplot_original <- boxplot_original + scale_y_continuous(trans='log10')}
boxplot_monthly_max <- ggplot() +
  geom_boxplot(data = original_dat, mapping = aes(x = month, y = int, fill = as.factor(dur))) +
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "D")+
  labs(title=sprintf("Daily max intensities, for each month (station %s)", station_id), x="month", y="intensity [mm/hr]") +
  theme_for_the_plots + 
  theme(legend.position="right")
if (log_scale_boxplot){boxplot_monthly_max <- boxplot_monthly_max + scale_y_continuous(trans='log10')}

boxplot_original + boxplot_monthly_max + plot_layout(ncol=1,heights=c(3, 3), guide = "collect")





# ------------------------------------------- Study of six stations -----------------------------------------------------
stations_of_interest <- c(75022, 77001, 69507, 98006, 76026, 84118)   # stations that will be studied in this part
station_id <- 75022
duration_id <- 60

#' quantile and probability plot functions. The two function differ by the way confidence bands are computed (the first is 
#' based on simulation, which take more time, and the second is based on subsampling, which is faster but less accurate).
quantile_and_probability_plot <- function(model, new_data, N = NULL, verbose = F, use_ggplot = F){
  if (!is.null(N)){
    simulated_params <- simulate(model, nsim = N, newdata = new_data, type = "response")
    boot_samp <- matrix(NA, nrow = N, ncol = dim(new_data)[1])
    for(b in 1:N){
      if (b %% 100 == 0 & verbose){print(b)}
      z <- (new_data$int - as.numeric(simulated_params$location[,b]))/as.numeric(simulated_params$scale[,b])
      z_standardize <- log(1 + as.numeric(simulated_params$shape[,b]) * z)/as.numeric(simulated_params$shape[,b])
      tryCatch(
        expr = {
          boot_samp[b, ] <- sort(z_standardize)
        },
        error = function(e){ 
          z_standardize[is.na(z_standardize)] <- mean(z_standardize, na.rm = T)
          boot_samp[b, ] <- sort(z_standardize)
        },
        warning = function(w){},
        finally = {}
      )
    }
    # boot_samp[is.na(boot_samp)] <- mean(boot_samp, na.rm = T)
    # env <- boot::envelope(mat = boot_samp)
  }
  
  # qqplot: 
  # standardized values:
  m_gev_per_month_pred <- predict(model, new_data, type = "response", se = T)
  z <- (new_data$int - m_gev_per_month_pred$fitted$location)/m_gev_per_month_pred$fitted$scale
  z_standardize <- log(1 + m_gev_per_month_pred$fitted$shape * z)/m_gev_per_month_pred$fitted$shape
  
  if (use_ggplot){
    df <- data.frame(xp = c(1:length(z_standardize))/(length(z_standardize)+1), 
                     yp = exp(-exp(-sort(z_standardize))),
                     xq = sort(z_standardize),
                     yq = -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1))))
    p1 <- ggplot(data = df) + 
      geom_point(aes(x = xp, y = yp), color = "black") + 
      geom_abline(slope = 1, intercept = 0, color = "blue") + 
      theme_for_the_plots + 
      labs(title="Probability plot", x="Theoretical", y="Empirical")
    p2 <- ggplot(data = df) + 
      geom_point(aes(x = xq, y = yq), color = "black") + 
      geom_abline(slope = 1, intercept = 0, color = "blue") +
      theme_for_the_plots + 
      labs(title="Quantile plot", x="Empirical", y="Theoretical")
    if (!is.null(N)){
      df2 <- data.frame(
        xp = c(1:length(z_standardize))/(length(z_standardize)+1),
        upper_p = exp(-exp(-sort(apply(boot_samp, 2, quantile, probs = 0.975, na.rm = T)))),
        lower_p = exp(-exp(-sort(apply(boot_samp, 2, quantile, probs = 0.025, na.rm = T)))),
        xq_upper = sort(apply(boot_samp, 2, quantile, probs = 0.975, na.rm = T)),
        xq_lower = sort(apply(boot_samp, 2, quantile, probs = 0.025, na.rm = T)),
        yq = -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1)))
      )
      p1 <- p1 + 
        geom_line(data = df2, mapping = aes(x = xp, y = upper_p), color = "blue", linetype = "dashed") + 
        geom_line(data = df2, mapping = aes(x = xp, y = lower_p), color = "blue", linetype = "dashed")
      p2 <- p2 + 
        geom_line(data = df2, mapping = aes(x = xq_upper, y = yq), color = "blue", linetype = "dashed") + 
        geom_line(data = df2, mapping = aes(x = xq_lower, y = yq), color = "blue", linetype = "dashed")
    }
    return(list(p1, p2))
  }else{
    par(mfrow = c(1, 2))
    # probability plot: 
    plot(c(1:length(z_standardize))/(length(z_standardize)+1), exp(-exp(-sort(z_standardize))), xlab = "Theoretical", ylab="Empirical",  main="Probability plot")
    if (!is.null(N)){
      lines(c(1:length(z_standardize))/(length(z_standardize)+1), exp(-exp(-sort(apply(boot_samp, 2, quantile, probs = 0.975, na.rm = T)))), lty = 2,  col = "blue")
      lines(c(1:length(z_standardize))/(length(z_standardize)+1), exp(-exp(-sort(apply(boot_samp, 2, quantile, probs = 0.025, na.rm = T)))), lty = 2, col = "blue")
    }
    abline(a=0, b=1, col = "blue")
    
    # quantile plot: 
    plot(sort(z_standardize), -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1))), xlab = "Empirical", ylab="Theoretical",  main="Quantile plot")
    if (!is.null(N)){
      lines(sort(apply(boot_samp, 2, quantile, probs = 0.975, na.rm = T)), -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1))), lty = 2,  col = "blue")
      lines(sort(apply(boot_samp, 2, quantile, probs = 0.025, na.rm = T)), -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1))), lty = 2, col = "blue")
    }
    abline(a=0, b=1, col = "blue")
    par(mfrow = c(1, 1))
  }
}

quantile_and_probability_plot_bis <- function(model, new_data, N = NULL, verbose = F){
  if (!is.null(N)){
    m_gev_per_month_pred <- predict(model, new_data, type = "response", se = T)
    z <- (new_data$int - m_gev_per_month_pred$fitted$location)/m_gev_per_month_pred$fitted$scale
    z_standardize <- log(1 + m_gev_per_month_pred$fitted$shape * z)/m_gev_per_month_pred$fitted$shape
    
    sample_size = 100
    boot_samp <- matrix(NA, nrow = N, ncol = sample_size)
    
    for(b in c(1:N)){
      if (b %% 100 == 0){print(b)}
      ind <- sample(x = c(1:length(z_standardize)), size = sample_size)
      Z = z_standardize[ind]
      Z[is.na(Z)] <- mean(Z, na.rm = T)
      boot_samp[b, ] <- sort(Z)
    }
  }
  z_standardize <- apply(boot_samp, 2, quantile, probs = 0.5, na.rm = T)
  
  df <- data.frame(xp = c(1:length(z_standardize))/(length(z_standardize)+1), 
                   yp = exp(-exp(-sort(z_standardize))),
                   xq = sort(z_standardize),
                   yq = -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1))))
  p1 <- ggplot(data = df) + 
    geom_point(aes(x = xp, y = yp), color = "black") + 
    geom_abline(slope = 1, intercept = 0, color = "blue") + 
    theme_for_the_plots + 
    labs(title="Probability plot", x="Theoretical", y="Empirical")
  p2 <- ggplot(data = df) + 
    geom_point(aes(x = xq, y = yq), color = "black") + 
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    theme_for_the_plots + 
    labs(title="Quantile plot", x="Empirical", y="Theoretical")
  if (!is.null(N)){
    df2 <- data.frame(
      xp = c(1:length(z_standardize))/(length(z_standardize)+1),
      upper_p = exp(-exp(-sort(apply(boot_samp, 2, quantile, probs = 0.975, na.rm = T)))),
      lower_p = exp(-exp(-sort(apply(boot_samp, 2, quantile, probs = 0.025, na.rm = T)))),
      xq_upper = sort(apply(boot_samp, 2, quantile, probs = 0.975, na.rm = T)),
      xq_lower = sort(apply(boot_samp, 2, quantile, probs = 0.025, na.rm = T)),
      yq = -log(-log(c(1:length(z_standardize))/(length(z_standardize)+1)))
    )
    p1 <- p1 + 
      geom_line(data = df2, mapping = aes(x = xp, y = upper_p), color = "blue", linetype = "dashed") + 
      geom_line(data = df2, mapping = aes(x = xp, y = lower_p), color = "blue", linetype = "dashed")
    p2 <- p2 + 
      geom_line(data = df2, mapping = aes(x = xq_upper, y = yq), color = "blue", linetype = "dashed") + 
      geom_line(data = df2, mapping = aes(x = xq_lower, y = yq), color = "blue", linetype = "dashed")
  }
  return(list(p1, p2, df, df2))
}


#                                                                   i.3) (MODEL) GEV model, one station, one duration ------------

dat <- data_monthly_maxima[data_monthly_maxima$id == station_id & data_monthly_maxima$dur == duration_id, ]
dat$month_id <- as.numeric(dat$month)


# Approach 1: no seasonality, monthly maxima: we use the fgev function first, but after we will use a package that allow for more flexibility (evgam) for modelling the GEV parameters
m_gev_simple <- fgev(dat$int, method="Nelder-Mead")
par(mfrow = c(1, 3))
profile_model <- profile(m_gev_simple)
plot(profile_model)
abline(v = 0, col = "red")
par(mfrow = c(1, 3))
par(mfrow=c(2,2)); plot(fgev((dat$int - m_gev_simple$estimate[1])/m_gev_simple$estimate[2],loc=0,scale=1)); par(mfrow=c(1,1))  # seems ok


# Approach 2: we use the evgam package, and test different models that we will compare with the deviance statistic, BIC, AIC, etc.

# m_gev_simple <- evgam(list(int ~ 1 , ~1, ~ 1), dat, family = "gev")
# summary(m_gev_simple)  # same values (the scale is in log scale...), but this package will allow us to use regressors for the parameters more easily

# --------- we fit the models:
#  Model 1: one set of parameters (loc, scale and shape) per month
m_gev_per_month <- evgam(list(int ~ month, ~ month, ~ month), dat, family = "gev") 

# Model 2: one model per month, but the shape is the same for all months
m_gev_per_month_fixed_shape <- evgam(list(int ~ month, ~ month, ~ 1), dat, family = "gev")  

# Model 3: same parameters for all months
m_gev_simple_ <- evgam(list(int ~ 1, ~ 1, ~ 1), dat, family = "gev")  

# Model 4: loc and scale smoothed over the months, fixed shape parameter
m_gev_per_month_fixed_shape_smooth <- evgam(list(int ~ s(month_id, bs = "cc", k = 10), 
                                                 ~ s(month_id, bs = "cc", k = 10), 
                                                 ~ 1), 
                                            dat, family = "gev")  

# Model 5: loc, scale and shape smoothed over the months
m_gev_per_month_smooth <- evgam(list(int ~ s(month_id, bs = "cc", k = 10), 
                                     ~ s(month_id, bs = "cc", k = 10), 
                                     ~ s(month_id, bs = "cc", k = 10)), 
                                dat, family = "gev")  
# Remarks: Model 3 is the simplest, (Model 3) included in (Model 2) included in (Model 1)
# (Model 3) included in (Model 4) included in (Model 5)

# (results for station 75022)


# --------- We select the best model (with deviance, bic, aic)
1 - pchisq(abs(m_gev_per_month$logLik - m_gev_per_month_fixed_shape$logLik), 
           df = length(m_gev_per_month$coefficients) - length(m_gev_per_month_fixed_shape$coefficients))
# 0.2431042 -> can not reject null hypothesis that a fixed shape is adequate, so we keep it 
# -> (Model 2) adequate compared to (Model 1)

1 - pchisq(abs(m_gev_per_month$logLik - m_gev_simple_$logLik), 
           df = length(m_gev_per_month$coefficients) - length(m_gev_simple_$coefficients))
# 6.795379e-07 -> reject null hypothesis, keep complex model
# -> (Model 3) NOT adequate compared to (Model 1)

1 - pchisq(abs(m_gev_per_month_fixed_shape_smooth$logLik - m_gev_simple_$logLik), 
           df = length(m_gev_per_month_fixed_shape_smooth$coefficients) - length(m_gev_simple_$coefficients))
# 1.50165e-07 -> reject null hypothesis, keep complex model
# -> (Model 3) NOT adequate compared to (Model 4)

1 - pchisq(abs(m_gev_per_month_smooth$logLik - m_gev_per_month_fixed_shape_smooth$logLik), 
           df = length(m_gev_per_month_smooth$coefficients) - length(m_gev_per_month_fixed_shape_smooth$coefficients))
# 0.9252244 -> can not reject null hypothesis: we can keep the smooth model with fixed shape
# -> (Model 4) adequate compared to (Model 5)

AIC(m_gev_per_month)  
AIC(m_gev_per_month_fixed_shape) 
AIC(m_gev_simple_)  
AIC(m_gev_per_month_fixed_shape_smooth)  
AIC(m_gev_per_month_smooth) 

BIC(m_gev_per_month)  
BIC(m_gev_per_month_fixed_shape)  
BIC(m_gev_simple_) 
BIC(m_gev_per_month_fixed_shape_smooth)  
BIC(m_gev_per_month_smooth)  

# --------- here is the best model:
model_of_interest <- m_gev_per_month_fixed_shape_smooth    # best BIC, second best AIC
summary(model_of_interest)

# --------- now we compute the quantiles by predicting the GEV parameters for each month
# new set of data used to compute the GEV parameters. It consist of each month.
new_dat <- data.frame(month = month.abb); new_dat$month <- factor(new_dat$month, level = month.abb); new_dat$month_id <- as.numeric(new_dat$month)
m_gev_per_month_pred <- predict(model_of_interest, new_dat, type = "response", se = T)
m_gev_per_month_pred <- cbind(new_dat, m_gev_per_month_pred)
quantiles_of_interest <- c(0.5, 0.8, 0.9, 0.95, 0.99)  # quantiles (p-return levels) of interest
list_quantiles_res <- list()
for (q in seq_along(quantiles_of_interest)){
  list_quantiles_res[[q]] <- do.call("cbind", predict(model_of_interest, new_dat, prob = quantiles_of_interest[q], se = T))
  list_quantiles_res[[q]]$quantile <- quantiles_of_interest[q]
  list_quantiles_res[[q]] <- cbind(new_dat, list_quantiles_res[[q]])
  colnames(list_quantiles_res[[q]]) <- c(colnames(new_dat), "fitted", "se", "quantile")
}
list_quantiles_res <- do.call("rbind", list_quantiles_res)
list_quantiles_res$quantile <- as.factor(list_quantiles_res$quantile)
list_quantiles_res$month <- as.factor(list_quantiles_res$month)

# --------- some plots for location, scale, shape and quantiles
# Location parameter
plot_loc <- ggplot(m_gev_per_month_pred, aes(x=month, y=fitted.location)) +
  geom_hline(yintercept = m_gev_simple$estimate[1], color = "blue") + 
  geom_hline(yintercept = m_gev_simple$estimate[1] - 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = m_gev_simple$estimate[1] + 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  geom_point() +
  geom_errorbar(aes(ymin=fitted.location-1.96*se.fit.location, ymax=fitted.location+1.96*se.fit.location), width=.2,
                position=position_dodge(.9)) +
  labs(title=paste("Estimate of location parameter for station ", station_id, " (", duration_id, " minutes)", sep = ""), x="Month", y="location") +
  scale_x_discrete(limits = month.abb) + 
  theme_for_the_plots + theme(plot.margin = margin(1, 1, 0.5, 1, "cm"))
# scale parameter
plot_scale <- ggplot(m_gev_per_month_pred, aes(x=month, y=fitted.scale)) + 
  geom_hline(yintercept = m_gev_simple$estimate[2], color = "blue") + 
  geom_hline(yintercept = m_gev_simple$estimate[2] - 1.96*m_gev_simple$std.err[2], color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = m_gev_simple$estimate[2] + 1.96*m_gev_simple$std.err[2], color = "blue", linetype = "dashed") + 
  geom_point() +
  geom_errorbar(aes(ymin=fitted.scale-1.96*se.fit.scale, ymax=fitted.scale+1.96*se.fit.scale), width=.2,
                position=position_dodge(.9)) +
  labs(title=paste("Estimate of shape parameter for station ", station_id, " (", duration_id, " minutes)", sep = ""), x="Month", y="shape") +
  scale_x_discrete(limits = month.abb) + 
  theme_for_the_plots + theme(plot.margin = margin(0.5, 1, 0.5, 1, "cm"))
# shape parameter
plot_shape <- ggplot(m_gev_per_month_pred, aes(x=month, y=fitted.shape)) + 
  geom_hline(yintercept = 0, color = "red") + 
  geom_hline(yintercept = m_gev_simple$estimate[3], color = "blue") + 
  geom_hline(yintercept = m_gev_simple$estimate[3] - 1.96*m_gev_simple$std.err[3], color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = m_gev_simple$estimate[3] + 1.96*m_gev_simple$std.err[3], color = "blue", linetype = "dashed") + 
  geom_point() +
  geom_errorbar(aes(ymin=fitted.shape-1.96*se.fit.shape, ymax=fitted.shape+1.96*se.fit.shape), width=.2,
                position=position_dodge(.9)) +
  labs(title=paste("Estimate of shape parameter for station ", station_id, " (", duration_id, " minutes)", sep = ""), x="Month", y="shape") +
  scale_x_discrete(limits = month.abb) + 
  theme_for_the_plots + theme(plot.margin = margin(0.5, 1, 1, 1, "cm"))

# return levels
plot_quantiles <- ggplot(list_quantiles_res, aes(x=month, y=fitted, color = quantile)) + 
  scale_color_viridis(name = "quantile", discrete = TRUE, option = "D")+
  geom_point() +
  geom_line(aes(group = quantile)) +
  geom_ribbon(aes(ymin=fitted-1.96*se, ymax=fitted+1.96*se, fill = quantile, group = quantile), alpha = 0.3) + 
  scale_fill_viridis(name = "quantile", discrete = TRUE, option = "D")+
  labs(title=paste("Quantiles for station ", station_id, " (", duration_id, " minutes)", sep = ""), x="Month", y="quantile") +
  theme_for_the_plots + theme(plot.margin = margin(0.5, 1, 1, 1, "cm")) + 
  scale_x_discrete(limits = month.abb) + 
  # scale_y_continuous(trans='log10') + 
  theme(legend.position="right") + 
  coord_cartesian(ylim = c(min(list_quantiles_res$fitted), max(list_quantiles_res$fitted)))
# coord_cartesian(ylim = c(min(list_quantiles_res$fitted-1.96*list_quantiles_res$se), max(list_quantiles_res$fitted+1.96*list_quantiles_res$se))) 

plot_loc + plot_scale + plot_shape + plot_quantiles + plot_layout(ncol=2,heights=c(1,1))


# --------- QQplot and model check
list_qqplots <- list()
for (j in seq_along(month.abb)){
  list_qqplots[[j]] <- quantile_and_probability_plot(model = model_of_interest, new_data = dat[dat$month == month.abb[j], ], 
                                                     N = 1000, verbose = F, use_ggplot = T)
  
  list_qqplots[[j]][[1]] <- list_qqplots[[j]][[1]] + labs(subtitle = month.abb[j])
  list_qqplots[[j]][[2]] <- list_qqplots[[j]][[2]] + labs(subtitle = month.abb[j])
}

# QQplot for each month:
Reduce('+', do.call(c, list("model" = lapply(list_qqplots, '[[', 1)))) + plot_layout(nrow=3)

# Global qqplot (probability and quantile plot)
quantile_and_probability_plot(model = model_of_interest, new_data = dat, N = 1000, verbose = F, use_ggplot = T)
quantile_and_probability_plot_bis(model = model_of_interest, new_data = dat, N = 1000, verbose = F)



#                                                                   i.4) (MODEL) GEV model, all durations ------------------ 
# Here, we will model all durations in the GEV parameters

original_dat <- data_of_interest[data_of_interest$id == station_id, ]
dat <- data_monthly_maxima[data_monthly_maxima$id == station_id, ]
dat$month_id <- as.numeric(dat$month)
dat$dur <- as.numeric(dat$dur)
dat$dur_factor <- as.factor(dat$dur)

#                                                                       - We specify the list of models we want to fit ------------------ 
list_of_models <- list(
  "basic" = list(int ~ 1, ~ 1, ~ 1),
  "model 1" = list(int ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ 1),
  "model 2" = list(int ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(dur, bs = "cs", k = 10)),
  "model 3" = list(int ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(month_id, bs = "cc", k = 10)),
  "model 4" = list(int ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10), 
                   ~ s(month_id, bs = "cc", k = 10) + s(dur, bs = "cs", k = 10)),
  
  "model 5" = list(int ~ te(month_id, dur, bs = c("cc", "cs"), k = c(10, 10)), 
                   ~ te(month_id, dur, bs = c("cc", "cs"), k = c(10, 10)), 
                   ~ 1),
  "model 6" = list(int ~ te(month_id, dur, bs = c("cc", "cs"), k = c(10, 10)),
                   ~ te(month_id, dur, bs = c("cc", "cs"), k = c(10, 10)),
                   ~ s(dur, bs = "cs", k = 10)),
  "model 7" = list(int ~ te(month_id, dur, bs = c("cs", "cs"), k = c(10, 10)),
                   ~ te(month_id, dur, bs = c("cs", "cs"), k = c(10, 10)),
                   ~ s(month_id, bs = "cs", k = 10)),
  "model 8" = list(int ~ te(month_id, dur, bs = c("cc", "cs"), k = c(10, 10)),
                   ~ te(month_id, dur, bs = c("cc", "cs"), k = c(10, 10)),
                   ~ s(dur, bs = "cs", k = 10) + s(month_id, bs = "cc", k = 10))
)

# and the anova tests
model_tests_anova <- list(
  list("basic", "model 1"),
  list("model 1", "model 2"),
  list("model 1", "model 3"),
  list("model 2", "model 4"),
  list("model 3", "model 4"),
  
  list("model 5", "model 6"),
  list("model 5", "model 7"),
  list("model 6", "model 8"),
  list("model 7", "model 8")
)

options(error=NULL)
#                                                                       - we fit each model ---------------------------------------------
list_model_fit <- list()
for (j in c(1:length(list_of_models))){
  print(paste("Fitting GEV model named ", names(list_of_models)[j], sep = ""))
  
  tryCatch(
    expr = {
      list_model_fit[[names(list_of_models)[j]]] <- evgam(list_of_models[[names(list_of_models)[j]]], dat, family = "gev")
    },
    error = function(e){ 
      print("ERROR....")
      list_model_fit[[names(list_of_models)[j]]] <- NULL
    },
    warning = function(w){
    },
    finally = {
    }
  )
}


#                                                                       - we collect AIC and BIC ----------------------------------------
AIC_models <- list()
BIC_models <- list()
for (j in c(1:length(list_model_fit))){
  AIC_models[[names(list_of_models)[j]]] <- AIC(list_model_fit[[names(list_of_models)[j]]])
  BIC_models[[names(list_of_models)[j]]] <- BIC(list_model_fit[[names(list_of_models)[j]]])
}
BIC_models[order(as.numeric(BIC_models))]
AIC_models[order(as.numeric(AIC_models))]

# models 8, 6 and 5 are the best here (station 75022)

#                                                                       - anova tests ---------------------------------------------------
test_results <- list()
level_rejection <- 0.05 # rejection level (5%)
for (j in c(1:length(model_tests_anova))){
  val <- 1 - pchisq(abs(list_model_fit[[model_tests_anova[[j]][[2]]]]$logLik - list_model_fit[[model_tests_anova[[j]][[1]]]]$logLik), 
                    df = abs(length(list_model_fit[[model_tests_anova[[j]][[2]]]]$coefficients) - length(list_model_fit[[model_tests_anova[[j]][[1]]]]$coefficients)))
  test_results[[j]] <- list(paste(model_tests_anova[[j]][[1]], " VS ", model_tests_anova[[j]][[2]], sep = ""), 
                            val, 
                            level_rejection < val)
  print(paste("Recap: ", test_results[[j]][[1]], ". Reject simpler model? ", !test_results[[j]][[3]], " (", test_results[[j]][[2]], ")", sep = ""))
}
test_results
# models 6 and 7 should be used (station 75022)


#                                                                       - KS and CVM tests ----------------------------------------------
durations_of_interest <- c(5, 10, 15, 30, 60, 120, 180, 360, 720, 1440)

global_check <- list()
check_by_duration <- list()
check_by_month <- list()
for (j in c(1:length(list_model_fit))){
  print(paste("Checking GEV model named ", names(list_of_models)[j], sep = ""))
  gev_pred <- predict(list_model_fit[[names(list_model_fit)[j]]], dat, type = "response")
  z <- (dat$int - gev_pred$location)/gev_pred$scale
  z_standardize <- log(1 + gev_pred$shape * z)/gev_pred$shape
  
  cvm_vals <- cvm.test(x = z_standardize, null = function(x){exp(-exp(-x))})$p.value    
  ks_vals <- ks.test(x = z_standardize, y = function(x){exp(-exp(-x))})$p.value    
  
  global_check[[names(list_model_fit)[j]]] <- list("CVM" = cvm_vals, "KS" = ks_vals)
  
  duration_check <- list()
  for (d in seq_along(durations_of_interest)){
    duration_check[[d]] <- list(
      "CVM" = cvm.test(x = z_standardize[dat$dur == durations_of_interest[d]], null = function(x){exp(-exp(-x))})$p.value,
      "KS" = ks.test(x = z_standardize[dat$dur == durations_of_interest[d]], y = function(x){exp(-exp(-x))})$p.value  ,
      "duration" = durations_of_interest[d]
    )
  }
  check_by_duration[[names(list_model_fit)[j]]] <- duration_check
  month_check <- list()
  for (m in c(1:12)){
    month_check[[m]] <- list(
      "CVM" = cvm.test(x = z_standardize[dat$month_id == m], null = function(x){exp(-exp(-x))})$p.value,
      "KS" = ks.test(x = z_standardize[dat$month_id == m], y = function(x){exp(-exp(-x))})$p.value,
      "month" = month.abb[m]
    )
  }
  check_by_month[[names(list_model_fit)[j]]] <- month_check
}

# global_check <- as.data.frame(do.call(rbind, global_check))
global_check_df <- as.data.frame(t(as.data.frame(lapply(global_check, unlist))))
global_check_df$model <- names(list_model_fit)

# model 5 and 6 not rejected for both tests (but close), model 7 rejected for KS test. All other models rejected.

df_list <- list()
for (j in c(1:length(list_model_fit))){
  df <- do.call(rbind.data.frame, check_by_month[[names(list_model_fit)[j]]])
  df <- data.frame(df[3], stack(df[1:2])); colnames(df) <- c("month", "p_value", "test")
  df$model <- names(list_model_fit)[j]
  df_list[[j]] <- df
}
df_list <- do.call(rbind, df_list)

df_list_dur <- list()
for (j in c(1:length(list_model_fit))){
  df <- do.call(rbind.data.frame, check_by_duration[[names(list_model_fit)[j]]])
  df <- data.frame(df[3], stack(df[1:2])); colnames(df) <- c("duration", "p_value", "test")
  df$model <- names(list_model_fit)[j]
  df_list_dur[[j]] <- df
}
df_list_dur <- do.call(rbind, df_list_dur)

#' Function for the Benjami-Yekutieli procedure
Benjamini_Yekutieli_procedure <- function(p_vals, idx){
  indices <- order(p_vals)
  sorted_p_val <- p_vals[indices]
  levels_th <- 0.05 * c(1:length(sorted_p_val))/(length(p_vals) * (log(length(p_vals)) + 0.57721 + 0.5/length(p_vals)))
  df_p_val_ks <- data.frame("id" = idx[indices], 
                            "p-value" = sorted_p_val, 
                            "level" = levels_th,
                            "over level" = as.factor(as.numeric(sorted_p_val > levels_th)))
  return(df_p_val_ks)
}

# Function to plot the results from the Benjami-Yekutieli procedure
test_possibilities  <- function(df_list_, name_sort, levels_y = NULL, global_check_all = NULL, ...){
  df_list_KS <- list()
  df_list_CVM <- list()
  for (j in c(1:length(list_model_fit))){
    df_list_KS[[j]] <- Benjamini_Yekutieli_procedure(df_list_[df_list_$model == names(list_model_fit)[j] & df_list_$test == "KS", c("p_value")], 
                                                     df_list_[df_list_$model == names(list_model_fit)[j] & df_list_$test == "KS", c(name_sort)])
    df_list_KS[[j]]$model <- names(list_model_fit)[j]
    df_list_KS[[j]]$test <- "KS"
    df_list_CVM[[j]] <- Benjamini_Yekutieli_procedure(df_list_[df_list_$model == names(list_model_fit)[j] & df_list_$test == "CVM", c("p_value")], 
                                                      df_list_[df_list_$model == names(list_model_fit)[j] & df_list_$test == "CVM", c(name_sort)])
    df_list_CVM[[j]]$model <- names(list_model_fit)[j]
    df_list_CVM[[j]]$test <- "CVM"
  }
  df_list_KS <- do.call(rbind, df_list_KS)
  df_list_KS <- df_list_KS[order(df_list_KS$model, df_list_KS$id), ]
  df_list_CVM <- do.call(rbind, df_list_CVM)
  df_list_CVM <- df_list_CVM[order(df_list_CVM$model, df_list_CVM$id), ]
  df_list_merged <- rbind(df_list_KS, df_list_CVM)
  colnames(df_list_merged) <- c(name_sort, "p_value", "level", "over.level", "model", "test")
  
  dff <- merge(df_list_merged, df_list_, 
               by = c(name_sort, "p_value", "test", "model"))
  colnames(dff) <- c("val", "p_value", "test", "model", "level", "over.level")
  
  p1 <- ggplot(data = dff[dff$test == "CVM", ], 
               aes(x=model, y=factor(val, levels = levels_y), fill=p_value)) + 
    geom_tile() + 
    scale_fill_viridis(limits = c(0,1), option = "plasma", name = "p-value") + 
    geom_point(data = dff[dff$over.level == 0 & dff$test == "CVM", ], shape=4, size=5, colour = "red", stroke = 2) +
    theme_for_the_plots + 
    theme(legend.position="right") + 
    labs(title = 'p-values from CVM test', ...) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    theme(legend.title.align=0.5) + 
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank()) +
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
  
  
  p3 <- ggplot(data = dff[dff$test == "KS", ], 
               aes(x=model, y=factor(val, levels = levels_y), fill=p_value)) + 
    geom_tile() + 
    scale_fill_viridis(limits = c(0,1), option = "plasma", name = "p-value") + 
    geom_point(data = dff[dff$over.level == 0 & dff$test == "KS", ], shape=4, size=5, colour = "red", stroke = 2) +
    theme_for_the_plots + 
    theme(legend.position="right") + 
    labs(title = 'p-values from KS test', ...) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    theme(legend.title.align=0.5) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank()) +
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
  
  
  procedure1 <- Benjamini_Yekutieli_procedure(global_check_all[, c("CVM")], 
                                              global_check_all[, c("model")])
  colnames(procedure1) <- c("model", "CVM", "level.CVM", "over.level.CVM")
  procedure2 <- Benjamini_Yekutieli_procedure(global_check_all[, c("KS")], 
                                              global_check_all[, c("model")])
  colnames(procedure2) <- c("model", "KS", "level.KS", "over.level.KS")
  
  procedures <- merge(procedure1[, c("model", "level.CVM", "over.level.CVM")], procedure2[, c("model", "level.KS", "over.level.KS")], by = "model")
  global_check_all_new <- merge(global_check_all, procedures, by = "model")
  
  global_check_all_new$duration <- "ALL"
  p2 <- ggplot(data = global_check_all_new[, c("CVM", "model", "duration")], aes(x=as.factor(model), y = as.factor(duration), fill=CVM)) + 
    geom_tile() + 
    scale_fill_viridis(limits = c(0,1), option = "plasma", name = "p-value") + 
    geom_point(data = global_check_all_new[global_check_all_new$over.level.CVM == 0, c("CVM", "model", "duration")], shape=4, size=5, colour = "red", stroke = 2) +
    theme_for_the_plots + 
    theme(legend.position="right") + 
    labs(title = '', ...) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    theme(legend.title.align=0.5) +
    theme(axis.title.y=element_blank()) +
    theme(plot.margin = margin(0.1, 0.1, 0.5, 0.1, "cm"))
  
  p4 <- ggplot(data = global_check_all_new[, c("KS", "model", "duration")], aes(x=as.factor(model), y = as.factor(duration), fill=KS)) + 
    geom_tile() + 
    scale_fill_viridis(limits = c(0,1), option = "plasma", name = "p-value") + 
    geom_point(data = global_check_all_new[global_check_all_new$over.level.KS == 0, c("KS", "model", "duration")], shape=4, size=5, colour = "red", stroke = 2) +
    theme_for_the_plots + 
    theme(legend.position="right") + 
    labs(title = '', ...) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    theme(legend.title.align=0.5) +
    theme(axis.title.y=element_blank()) +
    theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
  
  p1 + p2 + p3 + p4 + plot_layout(nrow = 4, heights = c(1, 0.1, 1, 0.1), guide = "collect")
  
}

test_possibilities(df_list_dur, "duration", levels_y = durations_of_interest, global_check_all = global_check_df, x = "model", y = "duration [minutes]")
test_possibilities(df_list, "month", levels_y = month.abb, global_check_all = global_check_df, x = "model name", y = "month")

#' How to interprete the graph? 
#' -> crosses indicate that the base hypothesis (model is adequate) for the corresponding model and month/duration was rejected. 
#' -> darker regions indicate lower p-value (so hypothesis is more likely to be rejected, as p-value is closer to the critical threshold)


#                                                                       - Selected model ------------------------------------------------
model_of_interest <- list_model_fit[["model 6"]]
summary(model_of_interest)


#                                                                       - QQplots -------------------------------------------------------
p <- quantile_and_probability_plot_bis(model = model_of_interest, new_data = dat, 
                                       N = 10000, verbose = T)
p[[1]] + p[[2]] + plot_layout(nrow = 1)

# porbability plot, for each duration
durations <- unique(dat$dur)
df_res_mean <- list()
df_res_conf <- list()
for (dur in seq_along(durations)){
  p <- quantile_and_probability_plot_bis(model = model_of_interest, new_data = dat[dat$dur == durations[dur], ], N = 10000, verbose = F)
  df_res_mean[[dur]] <- p[[3]]
  df_res_mean[[dur]]$dur <- durations[dur]
  df_res_conf[[dur]] <- p[[4]]
  df_res_conf[[dur]]$dur <- durations[dur]
}
df_res_mean <- do.call(rbind, df_res_mean)
df_res_conf <- do.call(rbind, df_res_conf)

p1 <- ggplot(data = df_res_mean) + 
  geom_point(aes(x = xp, y = yp, colour = as.factor(dur))) + 
  scale_colour_viridis(discrete = T, name = "Duration") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") + 
  theme_for_the_plots + 
  labs(title="Probability plot", x="Theoretical", y="Empirical") +
  # geom_line(data = df_res_conf, mapping = aes(x = xp, y = upper_p , colour = as.factor(dur), group = as.factor(dur)), linetype = "dashed") + 
  # geom_line(data = df_res_conf, mapping = aes(x = xp, y = lower_p , colour = as.factor(dur), group = as.factor(dur)), linetype = "dashed") + 
  theme(legend.position = "right")

p2 <- ggplot(data = df_res_mean) + 
  geom_point(aes(x = xq, y = yq, colour = as.factor(dur))) +
  scale_colour_viridis(discrete = T, name = "Duration") + 
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  theme_for_the_plots + 
  # geom_line(data = df_res_conf, mapping = aes(x = xq_upper, y = yq, colour = as.factor(dur), group = as.factor(dur)), linetype = "dashed") + 
  # geom_line(data = df_res_conf, mapping = aes(x = xq_lower, y = yq, colour = as.factor(dur), group = as.factor(dur)), linetype = "dashed") + 
  labs(title="Quantile plot", x="Empirical", y="Theoretical") +
  theme(legend.position = "right")

p1 + p2 + plot_layout(nrow = 1, guides=  "collect")


#                                                                       - Quantile score ------------------------------------------------
#' Check function
quantile_score <- function(obs, q, p){
  d <- obs - q
  res <- (p-1)*d
  res[d > 0] <- (p * d)[d > 0]
  return(res)
}

quantiles_of_interest <- c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99)
mat_quantile_score <- matrix(NA, ncol = length(durations_of_interest), nrow = length(quantiles_of_interest))
for (j in seq_along(quantiles_of_interest)){
  m_gev_pred <- predict(model_of_interest, dat, prob = quantiles_of_interest[j], se = F)
  for (i in seq_along(durations_of_interest)){
    mat_quantile_score[j, i] <- mean(quantile_score(dat$int[dat$dur == durations_of_interest[i]], m_gev_pred[[1]][dat$dur == durations_of_interest[i]], quantiles_of_interest[j]))
  }
}
rownames(mat_quantile_score) <- quantiles_of_interest
colnames(mat_quantile_score) <- durations_of_interest
ggplot(data = melt(mat_quantile_score), aes(x=as.factor(Var2), y=as.factor(Var1), fill=value)) +
  geom_tile() + 
  scale_fill_viridis() +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  theme_for_the_plots +
  theme(legend.position="right") +
  labs(x = "duration [minutes]", y = "p-quantile", title = "Quantile score") 
# remark: the smaller the value, the better it is (optimal value is 0). Here, model struggles the most for small durations and small quantiles


#                                                                       - Quantiles plot ------------------------------------------------
new_dat <- data.frame(month = month.abb, dur=rep(sort(unique(dat$dur)),ea=length(month.abb)))
new_dat$month <- factor(new_dat$month, level = month.abb)
new_dat$month_id <- as.numeric(new_dat$month)
new_dat$dur_factor <- as.factor(new_dat$dur)

m_gev_per_month_pred <- predict(model_of_interest, new_dat, type = "response", se = T)
m_gev_per_month_pred <- cbind(new_dat, m_gev_per_month_pred)
quantiles_of_interest <- c(0.5, 0.8, 0.9, 0.95, 0.99)
list_quantiles_res <- list()
for (q in seq_along(quantiles_of_interest)){
  list_quantiles_res[[q]] <- do.call("cbind", predict(model_of_interest, new_dat, prob = quantiles_of_interest[q], se = T))
  list_quantiles_res[[q]]$quantile <- quantiles_of_interest[q]
  list_quantiles_res[[q]] <- cbind(new_dat, list_quantiles_res[[q]])
  colnames(list_quantiles_res[[q]]) <- c(colnames(new_dat), "fitted", "se", "quantile")
}
list_quantiles_res <- do.call("rbind", list_quantiles_res)
list_quantiles_res$quantile <- as.factor(list_quantiles_res$quantile)
list_quantiles_res$month <- as.factor(list_quantiles_res$month)

list_quantiles_res$dur <- as.factor(list_quantiles_res$dur)
plot_quantiles <- ggplot(list_quantiles_res[list_quantiles_res$quantile == 0.95, ], aes(x=month, y=fitted, color = dur)) + 
  scale_color_viridis(name = "Duration", discrete = TRUE, option = "viridis")+
  # scale_color_manual(values = c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226")) + 
  geom_ribbon(aes(ymin=fitted-1.96*se, ymax=fitted+1.96*se, fill = dur, group = dur, colour = NULL), alpha = 0.3) + 
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "viridis")+
  # scale_fill_manual(values = c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226")) + 
  geom_point() +
  geom_line(aes(group = dur)) +
  labs(title=paste("0.95-quantile for station", station_id), x="Month", y="intensity [mm/hr]") +
  theme_for_the_plots + theme(plot.margin = margin(0.5, 1, 1, 1, "cm")) + 
  scale_x_discrete(limits = month.abb) + 
  scale_y_continuous(trans='log10') + 
  theme(legend.position="right") + 
  coord_cartesian(ylim = c(min(list_quantiles_res[list_quantiles_res$quantile == 0.95, ]$fitted), max(list_quantiles_res[list_quantiles_res$quantile == 0.95, ]$fitted)))
plot_quantiles



# dat$dur <- as.factor(dat$dur)
boxplot_monthly_max <- ggplot() +
  geom_boxplot(data = dat, mapping = aes(x = month, y = int, fill = as.factor(dur))) +
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "D")+
  labs(title=sprintf("Monthly max intensities, for each month (station %s)", station_id), x="month", y="intensity [mm/hr]") +
  theme_for_the_plots + 
  scale_y_continuous(trans='log10') + 
  theme(legend.position="right")

plot_quantiles + boxplot_monthly_max + plot_layout(ncol=1,heights=c(1,1))


#                                                                       - GEV parameters ------------------------------------------------
m_gev_per_month_pred$dur <- as.factor(m_gev_per_month_pred$dur)
plot_loc <- ggplot(m_gev_per_month_pred, aes(x=month, y=fitted.location, color = dur, group = dur)) +
  # geom_hline(yintercept = m_gev_simple$estimate[1], color = "blue") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1] - 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1] + 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  geom_ribbon(aes(ymin=fitted.location-1.96*se.fit.location, ymax=fitted.location+1.96*se.fit.location, color = NULL, fill = dur), alpha = 0.2) +
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "D")+
  geom_point() +
  geom_line() +
  scale_color_viridis(name = "Duration", discrete = TRUE, option = "D")+
  labs(title=paste("Estimate of location parameter for station", station_id), x="Month", y="location") +
  coord_cartesian(ylim = c(min(m_gev_per_month_pred$fitted.location), max(m_gev_per_month_pred$fitted.location))) + 
  scale_y_continuous(trans='log10') + 
  theme_for_the_plots + theme(plot.margin = margin(1, 1, 0.5, 1, "cm")) + theme(legend.position="right")

plot_scale <- ggplot(m_gev_per_month_pred, aes(x=month, y=fitted.scale, color = dur, group = dur)) +
  # geom_hline(yintercept = m_gev_simple$estimate[1], color = "blue") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1] - 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1] + 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  geom_ribbon(aes(ymin=fitted.scale-1.96*se.fit.scale, ymax=fitted.scale+1.96*se.fit.scale, color = NULL, fill = dur), alpha = 0.2) +
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "D")+
  geom_point() +
  geom_line() +
  scale_color_viridis(name = "Duration", discrete = TRUE, option = "D")+
  labs(title=paste("Estimate of scale parameter for station", station_id), x="Month", y="scale") +
  coord_cartesian(ylim = c(min(m_gev_per_month_pred$fitted.scale), max(m_gev_per_month_pred$fitted.scale))) +  
  scale_y_continuous(trans='log10') + 
  theme_for_the_plots + theme(plot.margin = margin(1, 1, 0.5, 1, "cm")) + theme(legend.position="right")

plot_shape <- ggplot(m_gev_per_month_pred, aes(x=month, y=fitted.shape, color = dur, group = dur)) +
  geom_hline(yintercept =0, color = "red") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1], color = "blue") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1] - 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  # geom_hline(yintercept = m_gev_simple$estimate[1] + 1.96*m_gev_simple$std.err[1], color = "blue", linetype = "dashed") + 
  geom_ribbon(aes(ymin=fitted.shape-1.96*se.fit.shape, ymax=fitted.shape+1.96*se.fit.shape, color = NULL, fill = dur), alpha = 0.2) +
  scale_fill_viridis(name = "Duration", discrete = TRUE, option = "D")+
  geom_point() +
  geom_line() +
  scale_color_viridis(name = "Duration", discrete = TRUE, option = "D")+
  labs(title=paste("Estimate of shape parameter for station", station_id), x="Month", y="shape") +
  coord_cartesian(ylim = c(min(m_gev_per_month_pred$fitted.shape-1.96*m_gev_per_month_pred$se.fit.shape), max(m_gev_per_month_pred$fitted.shape+1.96*m_gev_per_month_pred$se.fit.shape))) + 
  # scale_y_continuous(trans='log10') + 
  theme_for_the_plots + theme(plot.margin = margin(1, 1, 0.5, 1, "cm")) + theme(legend.position="right") + 
  theme(legend.position = "none")

plot_loc + plot_scale + plot_shape + plot_layout(ncol=3,heights=c(1), guide = "collect")


#                                                                       - Plot recap, IDF curves ----------------------------------------

month_of_interest <- c("Jan", "Apr", "Jul", "Oct")
durations <- sort(unique(list_quantiles_res$dur))
list_quantiles_res$dur <- as.numeric(levels(list_quantiles_res$dur)[list_quantiles_res$dur])

list_pl <- list()
for (m in seq_along(month_of_interest)){
  list_pl[[m]] <- ggplot() + theme_for_the_plots + 
    theme(plot.margin = margin(0, 0.3, 0, 0.1, "cm"))
  if (m == 1){
    list_pl[[m]] <- list_pl[[m]] + 
      labs(subtitle = month_of_interest[m],
           title = NULL,
           # title=paste("IDF curves for station ", station_id, sep = ""), 
           # x = "",
           x = "duration [minutes]",
           y="intensity [mm/hr]")
  }else if (m == length(month_of_interest)){
    list_pl[[m]] <- list_pl[[m]] + 
      labs(title=NULL, 
           subtitle = month_of_interest[m],
           x = "duration [minutes]",
           y= "intensity [mm/hr]")
  }else{
    list_pl[[m]] <- list_pl[[m]] + 
      labs(title=NULL, 
           subtitle = month_of_interest[m],
           # x = "",
           x = "duration [minutes]",
           y= "intensity [mm/hr]")
  }
  list_pl[[m]] <- list_pl[[m]] + 
    geom_boxplot(data = dat[dat$month == month_of_interest[m], ], mapping = aes(x = dur, y = int, group = dur)) +
    # scale_color_viridis(name = "p-quantile", discrete = TRUE, option = "magma")+
    scale_colour_manual(name = "p-quantile", breaks = quantiles_of_interest, values = c("#5a1a1a","#811313","#a70b0b","#d3650b","#ffbe0b")) + 
    geom_ribbon(data = list_quantiles_res[list_quantiles_res$month == month_of_interest[m], ], 
                aes(x = dur, ymin=fitted-1.96*se, ymax=fitted+1.96*se, fill = quantile, group = quantile, colour = NULL), alpha = 0.3) + 
    # scale_fill_viridis(name = "p-quantile", discrete = TRUE, option = "magma")+
    scale_fill_manual(name = "p-quantile", breaks = quantiles_of_interest, values = c("#5a1a1a","#811313","#a70b0b","#d3650b","#ffbe0b")) + 
    geom_point(data = list_quantiles_res[list_quantiles_res$month == month_of_interest[m], ], aes(x=dur, y=fitted, color = quantile)) +
    geom_line(data = list_quantiles_res[list_quantiles_res$month == month_of_interest[m], ], aes(x=dur, y=fitted, color = quantile, group = quantile)) +
    
    coord_cartesian(ylim = c(0.7, 300)) + 
    # scale_x_discrete(limits = month.abb) + 
    scale_y_continuous(trans='log10', labels = scales::comma) + 
    scale_x_continuous(trans='log10',
                       # minor_breaks = durations
    ) +
    # coord_cartesian(ylim = c(min(list_quantiles_res[list_quantiles_res$quantile == 0.95, ]$fitted), 
    #                          max(list_quantiles_res[list_quantiles_res$quantile == 0.95, ]$fitted))) + 
    # theme(legend.position="right", plot.title = element_text(hjust = 0.5)) + 
    theme(legend.justification = c(-0.05, -0.05), legend.position = c(0, 0))
}

# list_pl[[1]] <- list_pl[[1]] + theme(legend.position="none") 
# list_pl[[2]] <- list_pl[[2]] + theme(legend.position="none") 
# list_pl[[3]] <- list_pl[[3]] + theme(legend.position="none") 
# list_pl[[4]] <- list_pl[[4]] + theme(legend.justification = c(-0.05, -0.05), 
#                                      legend.position = c(0, 0), 
#                                      # legend.direction = "horizontal",
#                                      legend.background = element_rect(fill = "transparent"))
# list_pl[[1]] <- list_pl[[1]] + theme()
plot_IDF <- wrap_plots(list_pl, ncol = 2, guides = "collect") + plot_annotation(title = paste("IDF curves for station ", station_id, sep = ""), theme = theme(plot.title = element_text(hjust = 0.5)))

plot_IDF2 <- plot_quantiles + theme(plot.margin = margin(0, 0.3, 0, 0.1, "cm")) + 
  p1  + theme(plot.margin = margin(0, 0.3, 0, 0.1, "cm"), legend.position = "none") + 
  plot_layout(nrow = 1, ncol = 2, guides = "collect")

# ggsave(paste("/Users/Antoine/Desktop/IDF_1_", station_id, ".pdf", sep = ""), width = 30, height = 20, units = "cm", plot = plot_IDF)
# ggsave(paste("/Users/Antoine/Desktop/IDF_2_", station_id, ".pdf", sep = ""), width = 30, height = 10, units = "cm", plot = plot_IDF2)






# ------------------------------------------- All stations, extension ---------------------------------------------------
#                                                                   i.1) Approach 1: kNN --------------------------------
#                                                                       - train model for each station ------------------
stations_ids <- unique(data_of_interest$id)
durations <- unique(data_of_interest$dur)
model_formula_each_dur <- list(int ~ s(month_id, bs = "cc", k = 10), ~ s(month_id, bs = "cc", k = 10), ~ 1)

list_models <- list()
for (s in seq_along(stations_ids)){
  print(paste("Fitting GEV model for station ", stations_ids[s], sep = ""))
  dat <- data_monthly_maxima[data_monthly_maxima$id == stations_ids[s], ]
  dat$month_id <- as.numeric(dat$month)
  dat$dur <- as.numeric(dat$dur)
  dat$dur_factor <- as.factor(dat$dur)
  list_models[[stations_ids[s]]] <- list()
  for (dur in seq_along(durations)){
    dat_ <- dat[dat$dur == durations[dur], ]
    tryCatch(
      expr = {
        list_models[[stations_ids[s]]][[durations[dur]]] <- evgam(model_formula_each_dur, dat_, family = "gev")
      },
      error = function(e){ 
        list_models[[stations_ids[s]]][[durations[dur]]] <- NULL
      },
      warning = function(w){
      },
      finally = {
      }
    )
  }
}

#                                                                       - Get parameters and quantiles --------------------

list_model_fit <- list()
list_quantiles <- list()
list_transformed_values <- list()
for (s in seq_along(stations_ids)){
  dat <- data_monthly_maxima[data_monthly_maxima$id == stations_ids[s], ]
  dat$month_id <- as.numeric(dat$month)
  dat$dur <- as.numeric(dat$dur)
  dat$dur_factor <- as.factor(dat$dur)
  
  print(paste("Prediction GEV model for station ", stations_ids[s], sep = ""))
  
  for (dur in seq_along(durations)){
    dat_ <- dat[dat$dur == durations[dur], ]
    
    new_dat <- data.frame(month = month.abb, dur=rep(sort(unique(dat_$dur)),ea=length(month.abb)))
    new_dat$month <- factor(new_dat$month, level = month.abb)
    new_dat$month_id <- as.numeric(new_dat$month)
    new_dat$dur_factor <- as.factor(new_dat$dur)
    
    m_gev_per_month_pred <- predict(list_models[[stations_ids[s]]][[durations[dur]]], new_dat, type = "response", se = T)
    list_model_fit[[paste(stations_ids[s], durations[dur], sep = "-")]] <- cbind(new_dat, m_gev_per_month_pred)
    
    quantiles_of_interest <- c(0.5, 0.8, 0.9, 0.95, 0.99)
    list_quantiles_res <- list()
    for (q in seq_along(quantiles_of_interest)){
      list_quantiles_res[[q]] <- do.call("cbind", predict(list_models[[stations_ids[s]]][[durations[dur]]], new_dat, prob = quantiles_of_interest[q], se = T))
      list_quantiles_res[[q]]$quantile <- quantiles_of_interest[q]
      list_quantiles_res[[q]] <- cbind(new_dat, list_quantiles_res[[q]])
      colnames(list_quantiles_res[[q]]) <- c(colnames(new_dat), "fitted", "se", "quantile")
    }
    list_quantiles_res <- do.call("rbind", list_quantiles_res)
    list_quantiles_res$quantile <- as.factor(list_quantiles_res$quantile)
    list_quantiles_res$month <- as.factor(list_quantiles_res$month)
    
    list_quantiles_res$dur <- as.factor(list_quantiles_res$dur)
    list_quantiles[[paste(stations_ids[s], durations[dur], sep = "-")]] <- list_quantiles_res
    
    
    m_gev_per_month_pred <- predict(list_models[[stations_ids[s]]][[durations[dur]]], dat_, type = "response", se = T)
    z <- (dat_$int - m_gev_per_month_pred$fitted$location)/m_gev_per_month_pred$fitted$scale
    z_standardize <- log(1 + m_gev_per_month_pred$fitted$shape * z)/m_gev_per_month_pred$fitted$shape
    
    list_transformed_values[[paste(stations_ids[s], durations[dur], sep = "-")]] <- dat_
    list_transformed_values[[paste(stations_ids[s], durations[dur], sep = "-")]]$z_standardize <- z_standardize
  }
  
}

list_model_fit <- do.call(rbind, list_model_fit)
list_quantiles <- do.call(rbind, list_quantiles)
list_transformed_values <- do.call(rbind, list_transformed_values)
list_quantiles$id <- sapply(rownames(list_quantiles), function (x){as.numeric(strsplit(x, "-")[[1]][1])})
list_model_fit$id <- sapply(rownames(list_model_fit), function (x){as.numeric(strsplit(x, "-")[[1]][1])})


#                                                                       - QQPlot all --------------------

# list_model_fit
df <- data.frame(xp = c(1:length(list_transformed_values$z_standardize))/(length(list_transformed_values$z_standardize)+1),
                 yp = exp(-exp(-sort(list_transformed_values$z_standardize))),
                 xq = sort(list_transformed_values$z_standardize),
                 yq = -log(-log(c(1:length(list_transformed_values$z_standardize))/(length(list_transformed_values$z_standardize)+1))))
indices_sample <- sample(c(1:length(df$xp)), size = 100, replace = FALSE)

N <- 1000
boot_samp_prob <- matrix(NA, nrow = N, ncol = 100)
boot_samp_quant <- matrix(NA, nrow = N, ncol = 100)
for(b in 1:N){
  if (b %% 100 == 0){print(b)}
  indices_sample <- sample(c(1:length(list_transformed_values$z_standardize)), size = 100, replace = TRUE)
  Z <- list_transformed_values$z_standardize[indices_sample]
  df <- data.frame(xp = c(1:length(Z))/(length(Z)+1),
                   yp = exp(-exp(-sort(Z))),
                   xq = sort(Z),
                   yq = -log(-log(c(1:length(Z))/(length(Z)+1))))
  boot_samp_prob[b, ] <- df$yp
  boot_samp_quant[b, ] <- df$xq
}

quantile_0975_prob <- apply(boot_samp_prob, 2, quantile, 0.975)
quantile_0025_prob <- apply(boot_samp_prob, 2, quantile, 0.025)
quantile_05_prob <- apply(boot_samp_prob, 2, quantile, 0.5)

quantile_0975_quant <- apply(boot_samp_quant, 2, quantile, 0.975)
quantile_0025_quant <- apply(boot_samp_quant, 2, quantile, 0.025)
quantile_05_quant <- apply(boot_samp_quant, 2, quantile, 0.5)

indices_sample <- sample(c(1:length(list_transformed_values$z_standardize)), size = 100, replace = FALSE)
df_result_prob <- data.frame(x = c(1:length(quantile_05_prob))/(length(quantile_05_prob)+1),
                             median = quantile_05_prob,
                             upper = quantile_0975_prob,
                             lower = quantile_0025_prob,
                             sample = exp(-exp(-sort(list_transformed_values$z_standardize[indices_sample]))))
prob_plot <- ggplot(data = df_result_prob) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  geom_point(aes(x = x, y = median)) + 
  geom_line(aes(x = x, y = upper), color = "blue", linetype = 2, size = 0.5) + 
  geom_line(aes(x = x, y = lower), color = "blue", linetype = 2, size = 0.5) + 
  theme_for_the_plots + 
  labs(x = "theoretical", y = "empirical", title = "Probability plot")

df_result_quant <- data.frame(x = -log(-log(c(1:length(quantile_05_quant))/(length(quantile_05_quant)+1))),
                              median = quantile_05_quant,
                              upper = quantile_0975_quant,
                              lower = quantile_0025_quant)
quant_plot <- ggplot(data = df_result_quant) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  geom_point(aes(x = median, y = x)) + 
  geom_line(aes(x = upper, y = x), color = "blue", linetype = 2, size = 0.5) + 
  geom_line(aes(x = lower, y = x), color = "blue", linetype = 2, size = 0.5) + 
  theme_for_the_plots + 
  labs(x = "theoretical", y = "empirical", title = "Quantile plot")

prob_plot + quant_plot + plot_layout(nrow = 1)

#                                                                       - QQPlot test set ------------------
list_model_fit <- list()
list_quantiles <- list()
list_transformed_values <- list()
for (s in seq_along(stations_ids)){
  dat <- data_monthly_maxima[data_monthly_maxima$id == stations_ids[s], ]
  dat$month_id <- as.numeric(dat$month)
  dat$dur <- as.numeric(dat$dur)
  dat$dur_factor <- as.factor(dat$dur)
  
  print(paste("Prediction GEV model for station ", stations_ids[s], sep = ""))
  
  for (dur in seq_along(durations)){
    dat_ <- dat[dat$dur == durations[dur], ]
    
    new_dat <- data.frame(month = month.abb, dur=rep(sort(unique(dat_$dur)),ea=length(month.abb)))
    new_dat$month <- factor(new_dat$month, level = month.abb)
    new_dat$month_id <- as.numeric(new_dat$month)
    new_dat$dur_factor <- as.factor(new_dat$dur)
    
    m_gev_per_month_pred <- predict(list_models[[stations_ids[s]]][[durations[dur]]], new_dat, type = "response", se = T)
    list_model_fit[[paste(stations_ids[s], durations[dur], sep = "-")]] <- cbind(new_dat, m_gev_per_month_pred)
    
    quantiles_of_interest <- c(0.5, 0.8, 0.9, 0.95, 0.99)
    list_quantiles_res <- list()
    for (q in seq_along(quantiles_of_interest)){
      list_quantiles_res[[q]] <- do.call("cbind", predict(list_models[[stations_ids[s]]][[durations[dur]]], new_dat, prob = quantiles_of_interest[q], se = T))
      list_quantiles_res[[q]]$quantile <- quantiles_of_interest[q]
      list_quantiles_res[[q]] <- cbind(new_dat, list_quantiles_res[[q]])
      colnames(list_quantiles_res[[q]]) <- c(colnames(new_dat), "fitted", "se", "quantile")
    }
    list_quantiles_res <- do.call("rbind", list_quantiles_res)
    list_quantiles_res$quantile <- as.factor(list_quantiles_res$quantile)
    list_quantiles_res$month <- as.factor(list_quantiles_res$month)
    
    list_quantiles_res$dur <- as.factor(list_quantiles_res$dur)
    list_quantiles[[paste(stations_ids[s], durations[dur], sep = "-")]] <- list_quantiles_res
    
    
    m_gev_per_month_pred <- predict(list_models[[stations_ids[s]]][[durations[dur]]], dat_, type = "response", se = T)
    z <- (dat_$int - m_gev_per_month_pred$fitted$location)/m_gev_per_month_pred$fitted$scale
    z_standardize <- log(1 + m_gev_per_month_pred$fitted$shape * z)/m_gev_per_month_pred$fitted$shape
    
    list_transformed_values[[paste(stations_ids[s], durations[dur], sep = "-")]] <- dat_
    list_transformed_values[[paste(stations_ids[s], durations[dur], sep = "-")]]$z_standardize <- z_standardize
  }
  
}

list_model_fit <- do.call(rbind, list_model_fit)
list_quantiles <- do.call(rbind, list_quantiles)
list_transformed_values <- do.call(rbind, list_transformed_values)
list_quantiles$id <- sapply(rownames(list_quantiles), function (x){as.numeric(strsplit(x, "-")[[1]][1])})
list_model_fit$id <- sapply(rownames(list_model_fit), function (x){as.numeric(strsplit(x, "-")[[1]][1])})


# list_model_fit
df <- data.frame(xp = c(1:length(list_transformed_values$z_standardize))/(length(list_transformed_values$z_standardize)+1),
                 yp = exp(-exp(-sort(list_transformed_values$z_standardize))),
                 xq = sort(list_transformed_values$z_standardize),
                 yq = -log(-log(c(1:length(list_transformed_values$z_standardize))/(length(list_transformed_values$z_standardize)+1))))
indices_sample <- sample(c(1:length(df$xp)), size = 100, replace = FALSE)

N <- 1000
boot_samp_prob <- matrix(NA, nrow = N, ncol = 100)
boot_samp_quant <- matrix(NA, nrow = N, ncol = 100)
for(b in 1:N){
  if (b %% 100 == 0){print(b)}
  indices_sample <- sample(c(1:length(list_transformed_values$z_standardize)), size = 100, replace = TRUE)
  Z <- list_transformed_values$z_standardize[indices_sample]
  df <- data.frame(xp = c(1:length(Z))/(length(Z)+1),
                   yp = exp(-exp(-sort(Z))),
                   xq = sort(Z),
                   yq = -log(-log(c(1:length(Z))/(length(Z)+1))))
  boot_samp_prob[b, ] <- df$yp
  boot_samp_quant[b, ] <- df$xq
}

quantile_0975_prob <- apply(boot_samp_prob, 2, quantile, 0.975)
quantile_0025_prob <- apply(boot_samp_prob, 2, quantile, 0.025)
quantile_05_prob <- apply(boot_samp_prob, 2, quantile, 0.5)

quantile_0975_quant <- apply(boot_samp_quant, 2, quantile, 0.975)
quantile_0025_quant <- apply(boot_samp_quant, 2, quantile, 0.025)
quantile_05_quant <- apply(boot_samp_quant, 2, quantile, 0.5)

indices_sample <- sample(c(1:length(list_transformed_values$z_standardize)), size = 100, replace = FALSE)
df_result_prob <- data.frame(x = c(1:length(quantile_05_prob))/(length(quantile_05_prob)+1),
                             median = quantile_05_prob,
                             upper = quantile_0975_prob,
                             lower = quantile_0025_prob,
                             sample = exp(-exp(-sort(list_transformed_values$z_standardize[indices_sample]))))
prob_plot <- ggplot(data = df_result_prob) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  geom_point(aes(x = x, y = median)) + 
  geom_line(aes(x = x, y = upper), color = "blue", linetype = 2, size = 0.5) + 
  geom_line(aes(x = x, y = lower), color = "blue", linetype = 2, size = 0.5) + 
  theme_for_the_plots + 
  labs(x = "theoretical", y = "empirical", title = "Probability plot") + 
  theme(plot.title = element_text(hjust = 0.5))

df_result_quant <- data.frame(x = -log(-log(c(1:length(quantile_05_quant))/(length(quantile_05_quant)+1))),
                              median = quantile_05_quant,
                              upper = quantile_0975_quant,
                              lower = quantile_0025_quant)
quant_plot <- ggplot(data = df_result_quant) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  geom_point(aes(x = median, y = x)) + 
  geom_line(aes(x = upper, y = x), color = "blue", linetype = 2, size = 0.5) + 
  geom_line(aes(x = lower, y = x), color = "blue", linetype = 2, size = 0.5) + 
  theme_for_the_plots + 
  labs(x = "theoretical", y = "empirical", title = "Quantile plot") + 
  theme(plot.title = element_text(hjust = 0.5))

prob_plot + quant_plot + plot_layout(nrow = 1)

ggsave("results/Figures/chapter_3_univariate_analysis/qqplot_knn_probability.pdf", width = 15, height = 15, units = "cm", plot = prob_plot)
ggsave("results/Figures/chapter_3_univariate_analysis/qqplot_knn_quantile.pdf", width = 15, height = 15, units = "cm", plot = quant_plot)

#                                                                       - Test model with test set --------------------------- 
data_training_testing <- read.csv("data/stations/data_training_testing.csv")
list_stations_testing <- data_training_testing[!data_training_testing["train"],]$id
list_stations_training <- data_training_testing[data_training_testing["train"] == TRUE,]$id
list_of_station_coordinates_df <- read.csv("data/stations/coordinates.csv", header = TRUE, row.names = 1)

normalize <- function(x, vmin, vmax) {
  return ((x - vmin) / (vmax - vmin)) 
}

list_res_testing_stations <- list()
N_sample <- 100
prop_station_train <- 0.8
param_name <- c("location", "scale", "shape")

# options(warn=-1)

for (param in param_name){
  list_res_testing_stations[[param]] <- list()
  print(paste("------------ ", param, " ------------", sep = ""))
  for (dur in durations){
    print(paste("   ", dur, sep = ""))
    for (m in month.abb){
      print(paste("         ", m, sep = ""))
      dat_for_knn_training <- list_model_fit[list_model_fit$month == m & 
                                               list_model_fit$dur == dur, ]
      
      dat_for_knn_training <- merge(dat_for_knn_training, list_of_station_coordinates_df, by = "id")[c("id", paste("fitted.", param, sep = ""), paste("se.fit.", param, sep = ""), 
                                                                                                       "X", "Y", "alt")]
      colnames(dat_for_knn_training) <- c("id", paste("fitted.", param, sep = ""), paste("se.fit.", param, sep = ""), "X", "Y", "alt")
      list_res <- list()
      for (b in c(1:N_sample)){
        pred_v <- list_of_station_coordinates_df[list_of_station_coordinates_df$id %in% list_stations_testing, c("X", "Y", "alt", "id")]
        colnames(pred_v) <- c("X", "Y", "alt", "id")
        pred_v <- pred_v[pred_v$alt >= 0, ]
        
        knn_training <- dat_for_knn_training[dat_for_knn_training$id %in% sample(list_stations_training, size = floor(length(list_stations_training)*prop_station_train)), ]
        
        pred_v["X_norm"] <- normalize(pred_v["X"], min(pred_v["X"]), max(pred_v["X"]))
        pred_v["Y_norm"] <- normalize(pred_v["Y"], min(pred_v["Y"]), max(pred_v["Y"]))
        pred_v["alt_norm"] <- normalize(pred_v["alt"], min(pred_v["alt"]), max(pred_v["alt"]))
        
        knn_training["X_norm"] <- normalize(knn_training["X"], min(pred_v["X"]), max(pred_v["X"]))
        knn_training["Y_norm"] <- normalize(knn_training["Y"], min(pred_v["Y"]), max(pred_v["Y"]))
        knn_training["alt_norm"] <- normalize(knn_training["alt"], min(pred_v["alt"]), max(pred_v["alt"]))
        
        preds_fitted <- KernelKnn(knn_training[c("X_norm", "Y_norm", "alt_norm")], TEST_data = pred_v[c("X_norm", "Y_norm", "alt_norm")], 
                                  as.numeric(knn_training[, paste("fitted.", param, sep = "")]), k = 2, 
                                  method = 'euclidean', weights_function = 'tricube', regression = T)
        
        preds_se <- KernelKnn(knn_training[c("X_norm", "Y_norm", "alt_norm")], TEST_data = pred_v[c("X_norm", "Y_norm", "alt_norm")], 
                              as.numeric(knn_training[,paste("se.fit.", param, sep = "")]), k = 2, 
                              method = 'euclidean', weights_function = 'tricube', regression = T)
        pred_v$fitted <- preds_fitted
        pred_v$se <- preds_se
        pred_v$sample <- b
        pred_v$dur <- dur
        pred_v$month <- m
        
        # model_knn <- knn.reg(train = knn_training[c("X_norm", "Y_norm", "alt_norm")], 
        #                      test = pred_v[c("X_norm", "Y_norm", "alt_norm")], 
        #                      y = knn_training$fitted, 
        #                      k = 2)
        # model_knn2 <- knn.reg(train = knn_training[c("X_norm", "Y_norm")], 
        #                       test = pred_v[c("X_norm", "Y_norm")], 
        #                       y = knn_training$fitted, 
        #                       k = 2)
        # pred_v$pred <- model_knn$pred
        # pred_v$pred2 <- model_knn2$pred
        
        list_res[[b]] <- pred_v
      }
      list_res <- do.call(rbind, list_res)
      invisible(capture.output(list_res <- list_res %>% 
                                 group_by(X, Y, alt, id, dur, month) %>% 
                                 summarize(mean_fitted = mean(fitted), mean_se = mean(se), var_fitted = var(fitted), var_se = var(se)) %>% 
                                 ungroup() %>% 
                                 as.data.frame()))
      
      list_res_testing_stations[[param]][[paste(dur, m, sep = "-")]] <- list_res
    }
  }
}
# options(warn)
locations_testing <- do.call(rbind, list_res_testing_stations[["location"]])
colnames(locations_testing) <- c("X", "Y", "alt", "id", "dur", "month", "mean.fitted.location", "mean.se.location", "var.fitted.location", "var.se.location")
scale_testing <- do.call(rbind, list_res_testing_stations[["scale"]])
colnames(scale_testing) <- c("X", "Y", "alt", "id", "dur", "month", "mean.fitted.scale", "mean.se.scale", "var.fitted.scale", "var.se.scale")
shape_testing <- do.call(rbind, list_res_testing_stations[["shape"]])
colnames(shape_testing) <- c("X", "Y", "alt", "id", "dur", "month", "mean.fitted.shape", "mean.se.shape", "var.fitted.shape", "var.se.shape")

# locations_testing[locations_testing$id == 77001 & locations_testing$dur == 10 & locations_testing$month == "Jan", "mean.fitted.location"]
# scale_testing[scale_testing$id == 77001 & scale_testing$dur == 10 & scale_testing$month == "Jan", "mean.fitted.scale"]
# shape_testing[shape_testing$id == 77001 & shape_testing$dur == 10 & shape_testing$month == "Jan", "mean.fitted.shape"]
# 
# list_model_fit[list_model_fit$id == 77001 & list_model_fit$dur == 10 & list_model_fit$month == "Jan", ]




#                                                                   i.2) Approach 2: CDN --------------------------------
# see: Python notebook

#                                                                   i.3) Comparison: kNN vs CDN -------------------------
#                                                                       - NN vs kNN expected quantile scores ------------

# Computing quantile scores:
quantiles_of_interest <- c(0.5, 0.8, 0.9, 0.95, 0.99, 0.999)

quantile_score <- function(obs, q_fitted, p){
  d <- obs - q_fitted
  res <- (p-1)*d
  res[d > 0] <- (p * d)[d > 0]
  return(res)
}

get_quantile <- function(p, location_param, scale_param, shape_param){
  return(location_param + scale_param/shape_param * ((-log(p))**(-shape_param) - 1))
}

create_quantile_bootstrap <- function(p, loc.fitted, loc.se, scale.fitted, scale.se, shape.fitted, shape.se, N = 100){
  locations <- rnorm(n = N, mean = loc.fitted, sd = loc.se)
  scales <- rnorm(n = N, mean = scale.fitted, sd = scale.se)
  shapes <- rnorm(n = N, mean = shape.fitted, sd = shape.se)
  quantiles <- get_quantile(p, locations, scales, shapes)
  return(data.frame("fitted" = mean(quantiles), "se" = sqrt(var(quantiles))))
}

CRPS_QS_empirical <- function(obs, mean_estimate, sd_estimate, N_sample = 50, N_val_CRPS = 50){
  sample_estimate <- rnorm(n = N_sample, mean = mean_estimate, sd = sd_estimate)
  alpha <- c(1:N_val_CRPS)/N_val_CRPS
  quant <- quantile(sample_estimate, alpha)
  term1 <- 1.0 * (obs < quant) - alpha
  term2 <- quant - obs
  return(2 * sum(term1 * term2)/N_val_CRPS)
}

QS_distr_empirical <- function(p, obs, mean_estimate, sd_estimate, N_sample =100){
  sample_estimate <- rnorm(n = N_sample, mean = mean_estimate, sd = sd_estimate)
  return(mean(quantile_score(obs, sample_estimate, p)))
}


#                                                                       - neural network prediction ---------------------
data_GEV_all_NN <- read.csv("results/estimates/univariate_CDN/pred_model_GEV_all.csv")
colnames(data_GEV_all_NN) <- c("quantile95", "location", "scale", "shape","month","month_id","dur","Model","id")

dat_NN <- data_GEV_all_NN[c("location", "scale", "shape","month","dur","Model","id")] %>% 
  group_by(month, dur, id) %>%
  summarize(fitted.location = mean(location), 
            se.location = sqrt(var(location)), 
            fitted.scale = mean(scale), 
            se.scale = sqrt(var(scale)), 
            fitted.shape = mean(shape), 
            se.shape = sqrt(var(shape))) %>%
  ungroup() %>% 
  as.data.frame()


dat_NN_all_quantiles <- list()
for (quant in seq_along(quantiles_of_interest)){
  dat_NN_all_quantiles[[quant]] <- dat_NN %>% group_by(dur, month, id) %>%
    summarize(descr = create_quantile_bootstrap(quantiles_of_interest[quant], fitted.location, 
                                                se.location, fitted.scale, se.scale, fitted.shape, se.shape)) %>%
    unpack(cols = descr) %>%
    ungroup() %>%
    as.data.frame()
  dat_NN_all_quantiles[[quant]]$quantile <- quantiles_of_interest[quant]
}
dat_NN_all_quantiles <- do.call(rbind, dat_NN_all_quantiles)

dat_NN_all_quantiles <- merge(data_monthly_maxima, dat_NN_all_quantiles, by = c("dur", "id", "month"))


dat_NN_CRPS_QS <- dat_NN_all_quantiles %>% group_by(dur, month, id, year, quantile) %>%
  summarize(QS = QS_distr_empirical(quantile, int, fitted, se)) %>%
  ungroup() %>%
  as.data.frame() %>% 
  group_by(month, quantile) %>%
  summarize(QS = mean(QS)) %>%
  ungroup() %>%
  as.data.frame()


#                                                                       - knn prediction --------------------------------
dat_knn_loc <- locations_testing[, c("dur", "month", "id", "mean.fitted.location", "mean.se.location", "var.fitted.location")]
dat_knn_loc$var.fitted.location <- sqrt(dat_knn_loc$var.fitted.location)
colnames(dat_knn_loc) <- c("dur", "month", "id", "location", "se1", "se2")
dat_knn_loc$se.location <- sqrt(dat_knn_loc$se1**2 + dat_knn_loc$se2**2)
dat_knn_loc <- dat_knn_loc[,c("dur", "month", "id", "location", "se.location")]

dat_knn_scale <- scale_testing[, c("dur", "month", "id", "mean.fitted.scale", "mean.se.scale", "var.fitted.scale")]
dat_knn_scale$var.fitted.location <- sqrt(dat_knn_scale$var.fitted.scale)
colnames(dat_knn_scale) <- c("dur", "month", "id", "scale", "se1", "se2")
dat_knn_scale$se.scale <- sqrt(dat_knn_scale$se1**2 + dat_knn_scale$se2**2)
dat_knn_scale <- dat_knn_scale[,c("dur", "month", "id", "scale", "se.scale")]

dat_knn_shape <- shape_testing[, c("dur", "month", "id", "mean.fitted.shape", "mean.se.shape", "var.fitted.shape")]
dat_knn_shape$var.fitted.location <- sqrt(dat_knn_shape$var.fitted.shape)
colnames(dat_knn_shape) <- c("dur", "month", "id", "shape", "se1", "se2")
dat_knn_shape$se.shape <- sqrt(dat_knn_shape$se1**2 + dat_knn_shape$se2**2)
dat_knn_shape <- dat_knn_shape[,c("dur", "month", "id", "shape", "se.shape")]

dat_knn <- merge(merge(dat_knn_loc, dat_knn_scale, by = c("dur","month", "id")), dat_knn_shape, by = c("dur","month", "id"))

dat_kNN_all_quantiles <- list()
for (quant in seq_along(quantiles_of_interest)){
  dat_kNN_all_quantiles[[quant]] <- dat_knn %>% group_by(dur, month, id) %>%
    summarize(descr = create_quantile_bootstrap(quantiles_of_interest[quant], location, se.location, scale, se.scale, shape, se.shape)) %>%
    unpack(cols = descr) %>%
    ungroup() %>%
    as.data.frame()
  dat_kNN_all_quantiles[[quant]]$quantile <- quantiles_of_interest[quant]
}
dat_kNN_all_quantiles <- do.call(rbind, dat_kNN_all_quantiles)

dat_kNN_all_quantiles <- merge(data_monthly_maxima, dat_kNN_all_quantiles, by = c("dur", "id", "month"))

dat_kNN_CRPS_QS <- dat_kNN_all_quantiles %>% group_by(dur, month, id, year, quantile) %>%
  summarize(QS = QS_distr_empirical(quantile, int, fitted, se)) %>%
  ungroup() %>%
  as.data.frame() %>% 
  group_by(month, quantile) %>%
  summarize(QS = mean(QS)) %>%
  ungroup() %>%
  as.data.frame()




#                                                                       - reference model prediction --------------------

dat_reference_all_quantiles <- list()
for (quant in seq_along(quantiles_of_interest)){
  dat_reference_all_quantiles[[quant]] <- list_model_fit %>% group_by(dur, month, id) %>%
    summarize(descr = create_quantile_bootstrap(quantiles_of_interest[quant], fitted.location, se.fit.location, fitted.scale, se.fit.scale, fitted.shape, se.fit.shape)) %>%
    unpack(cols = descr) %>%
    ungroup() %>%
    as.data.frame()
  dat_reference_all_quantiles[[quant]]$quantile <- quantiles_of_interest[quant]
}
dat_reference_all_quantiles <- do.call(rbind, dat_reference_all_quantiles)

dat_reference_all_quantiles <- merge(data_monthly_maxima, dat_reference_all_quantiles, by = c("dur", "id", "month"))

dat_reference_CRPS_QS <- dat_reference_all_quantiles %>% group_by(dur, month, id, year, quantile) %>%
  summarize(QS = QS_distr_empirical(quantile, int, fitted, se)) %>%
  ungroup() %>%
  as.data.frame() %>% 
  group_by(month, quantile) %>%
  summarize(QS = mean(QS)) %>%
  ungroup() %>%
  as.data.frame()





#                                                                       - all scores ------------------------------------
# first, compute quantile scores
QS_scores <- merge(merge(dat_reference_CRPS_QS, dat_NN_CRPS_QS, by = c("month", "quantile")), dat_kNN_CRPS_QS, by = c("month", "quantile"))
colnames(QS_scores) <- c("month", "quantile", "reference", "CDN", "kNN")

QS_scores$QSS_M <- 1- QS_scores$kNN/QS_scores$CDN
QS_scores$QSS_R <- 1- QS_scores$CDN/QS_scores$kNN
QS_scores$QSI_kNN_vs_CDN <- 0
QS_scores$QSI_kNN_vs_CDN[QS_scores$kNN <= QS_scores$CDN] <- QS_scores$QSS_M[QS_scores$kNN <= QS_scores$CDN]
QS_scores$QSI_kNN_vs_CDN[QS_scores$kNN > QS_scores$CDN] <- -QS_scores$QSS_R[QS_scores$kNN > QS_scores$CDN]

QS_scores$QSS_M <- 1- QS_scores$kNN/QS_scores$reference
QS_scores$QSS_R <- 1- QS_scores$reference/QS_scores$kNN
QS_scores$QSI_kNN_vs_ref <- 0
QS_scores$QSI_kNN_vs_ref[QS_scores$kNN <= QS_scores$reference] <- QS_scores$QSS_M[QS_scores$kNN <= QS_scores$reference]
QS_scores$QSI_kNN_vs_ref[QS_scores$kNN > QS_scores$reference] <- -QS_scores$QSS_R[QS_scores$kNN > QS_scores$reference]

QS_scores$QSS_M <- 1- QS_scores$CDN/QS_scores$reference
QS_scores$QSS_R <- 1- QS_scores$reference/QS_scores$CDN
QS_scores$QSI_CDN_vs_ref <- 0
QS_scores$QSI_CDN_vs_ref[QS_scores$CDN <= QS_scores$reference] <- QS_scores$QSS_M[QS_scores$CDN <= QS_scores$reference]
QS_scores$QSI_CDN_vs_ref[QS_scores$CDN > QS_scores$reference] <- -QS_scores$QSS_R[QS_scores$CDN > QS_scores$reference]


# plot the results
vals_neg = c(-1, -0.7, -0.4, -0.3, -0.2, -0.1)
vals_pos = c(0.1, 0.2, 0.3, 0.4, 0.7, 1.0)
box_fct <- function(x, vals_neg = c(-1, -0.4, -0.3, -0.2, -0.1), vals_pos = c(0.1, 0.2, 0.3, 0.4, 1.0)){
  if (x < 0){
    diff_val <- x - vals_neg
    indices <- diff_val >= 0
    return(vals_neg[indices][which(diff_val[indices] == min(diff_val[indices]))])
  }else{
    diff_val <- vals_pos - x
    indices <- diff_val >= 0
    return(vals_pos[indices][which(diff_val[indices] == min(diff_val[indices]))])
  }
}
construct_color_def <- function(color, vals){
  return(data.frame(color = color,
                    x = vals[1:(length(vals)-1)], xend = vals[2:(length(vals))],
                    y = 0, yend = 1))
}

colors <- rev(c("#485696", "#5C69A1", "#707bab","#989fbf","#c0c3d3","#e7e7e7", "#EAE3DB", "#F0D7B6", "#f9c784", "#FAB46B", "#fba151", "#F77729"))
color_values_df <- construct_color_def(colors,  c(vals_neg, 0.0, vals_pos))

#' Function to plot EQSI depending on the month or the duration, and the p-quantile
plot_QSI <- function(which_variable, DAT, name = "CRPSI", ...){
  scoreDF <- DAT
  scoreDF$score_bin <- sapply(scoreDF[, which_variable], box_fct, vals_pos = vals_pos, vals_neg = vals_neg)
  colnames(scoreDF)[length(colnames(scoreDF))] <- "x"
  scoreDF <- merge(scoreDF, color_values_df, by = c("x"))
  colnames(scoreDF)[1] <- "score_bin"
  
  plot_q <- ggplot(data = scoreDF, aes(x=as.factor(month), y=as.factor(quantile), fill=as.factor(color))) + 
    geom_tile() + 
    # scale_fill_manual(values = colors, name = name) +
    scale_fill_manual(breaks = colors, values = colors, name = name) +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    theme_for_the_plots +
    theme(legend.position="none") +
    labs(x = "month", y = "p-quantile", ...) +
    theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          panel.grid.major = element_line(colour = "transparent"),
          panel.grid.minor = element_line(colour = "transparent"))
  
  theme_for_the_plots +
    theme(legend.position="right") +
    # labs(x = "month", y = "p-quantile", title = "Quantile Score Index of CDN model versus reference model") +
    theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"))
  
  return(plot_q)
}

plot_qsi_knn_vs_cdn_quant_dur <- plot_QSI("QSI_kNN_vs_CDN", QS_scores, subtitle = "Quantile Score Index of kNN model versus CDN model")
plot_qsi_knn_vs_ref_quant_dur <- plot_QSI("QSI_kNN_vs_ref", QS_scores, subtitle = "Quantile Score Index of kNN model versus reference model")
plot_qsi_cdn_vs_ref_quant_dur <- plot_QSI("QSI_CDN_vs_ref", QS_scores, subtitle = "Quantile Score Index of CDN model versus reference model")


# construct the legend...
p <- ggplot()
create_geom_rect <- function(j, reverted = FALSE) {
  force(j)
  if (reverted){
    return(geom_rect(data = NULL, aes(ymin = color_values_df$x[j], ymax = color_values_df$xend[j], 
                                      xmin = color_values_df$y[j], xmax = color_values_df$yend[j]), 
                     fill = color_values_df$color[j], colour = "transparent"))
  }else{
    return(geom_rect(data = NULL, aes(xmin = color_values_df$x[j], xmax = color_values_df$xend[j], 
                                      ymin = color_values_df$y[j], ymax = color_values_df$yend[j]), 
                     fill = color_values_df$color[j], colour = "transparent"))
  }
}
for (j in seq_along(color_values_df$color)){
  p <- p + create_geom_rect(j, reverted = T)
}
p <- p + theme_for_the_plots + 
  scale_y_continuous(breaks = c(color_values_df$x, color_values_df$xend[length(color_values_df$xend)]), position = "right", 
                     limits = c(-1, 1), expand = c(0,0)) + 
  theme(panel.grid.major = element_line(colour = "transparent"), 
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.text.x = element_text(colour = "transparent"),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        plot.margin = margin(0.2, 0.2, 0.2, 1.5, "cm")) + 
  labs(subtitle = "CRPSI", title = NULL, y = NULL)


layout <- "
AAAAAAAAAAAAAAAAAAAD
BBBBBBBBBBBBBBBBBBBD
CCCCCCCCCCCCCCCCCCCD
"
p_final <- plot_qsi_knn_vs_cdn_quant_dur + 
  plot_qsi_knn_vs_ref_quant_dur + 
  plot_qsi_cdn_vs_ref_quant_dur + 
  p + plot_layout(design = layout)

ggsave("results/Figures/chapter_3_univariate_analysis/GEV_comparison_knn_vs_CDN_vs_ref.pdf", width = 20, height = 20, units = "cm", plot = p_final)




#                                                                       - Comparing IDF curves --------------------------
stations_of_interest <- 69507
q_of_interest <- 0.95
# month_of_interest <- "Sep"

monthss <- c("Jan", "May", "Sep")

data_GEV_all_NN <- read.csv("results/estimates/univariate_CDN/pred_model_GEV_all.csv")

list_plots_GEV <- list()
for (m in seq_along(monthss)){
  month_of_interest <- monthss[m]
  
  # reference model
  dat_ref <- list_quantiles[list_quantiles$id == stations_of_interest & list_quantiles$month == month_of_interest & list_quantiles$quantile == q_of_interest,]
  dat_ref$dur <- as.numeric(levels(dat_ref$dur)[dat_ref$dur])
  
  # knn model
  dat_knn_loc <- locations_testing[locations_testing$id == stations_of_interest & locations_testing$month == month_of_interest, c("dur", "mean.fitted.location", "mean.se.location", "var.fitted.location")]
  dat_knn_loc$var.fitted.location <- sqrt(dat_knn_loc$var.fitted.location)
  colnames(dat_knn_loc) <- c("dur", "location", "se1", "se2")
  dat_knn_loc$se.location <- sqrt(dat_knn_loc$se1**2 + dat_knn_loc$se2**2)
  dat_knn_loc <- dat_knn_loc[,c("dur", "location", "se.location")]
  
  dat_knn_scale <- scale_testing[scale_testing$id == stations_of_interest & scale_testing$month == month_of_interest, c("dur", "mean.fitted.scale", "mean.se.scale", "var.fitted.scale")]
  dat_knn_scale$var.fitted.location <- sqrt(dat_knn_scale$var.fitted.scale)
  colnames(dat_knn_scale) <- c("dur", "scale", "se1", "se2")
  dat_knn_scale$se.scale <- sqrt(dat_knn_scale$se1**2 + dat_knn_scale$se2**2)
  dat_knn_scale <- dat_knn_scale[,c("dur", "scale", "se.scale")]
  
  dat_knn_shape <- shape_testing[shape_testing$id == stations_of_interest & shape_testing$month == month_of_interest, c("dur", "mean.fitted.shape", "mean.se.shape", "var.fitted.shape")]
  dat_knn_shape$var.fitted.location <- sqrt(dat_knn_shape$var.fitted.shape)
  colnames(dat_knn_shape) <- c("dur", "shape", "se1", "se2")
  dat_knn_shape$se.shape <- sqrt(dat_knn_shape$se1**2 + dat_knn_shape$se2**2)
  dat_knn_shape <- dat_knn_shape[,c("dur", "shape", "se.shape")]
  
  
  dat_knn <- merge(merge(dat_knn_loc, dat_knn_scale, by = "dur"), dat_knn_shape, by = "dur")
  
  N <- 100
  get_quantile <- function(p, location_param, scale_param, shape_param){
    return(location_param + scale_param/shape_param * ((-log(p))**(-shape_param) - 1))
  }
  durations <- unique(dat_knn$dur)
  dat_knn$mean_quantile <- NULL
  dat_knn$se_quantile <- NULL
  for (dur in seq_along(durations)){
    locations <- rnorm(n = N, mean = dat_knn[dat_knn$dur == durations[dur], "location"],
                       sd = dat_knn[dat_knn$dur == durations[dur], "se.location"])
    scales <- rnorm(n = N, mean = dat_knn[dat_knn$dur == durations[dur], "scale"],
                    sd = dat_knn[dat_knn$dur == durations[dur], "se.scale"])
    shapes <- rnorm(n = N, mean = dat_knn[dat_knn$dur == durations[dur], "shape"],
                    sd = dat_knn[dat_knn$dur == durations[dur], "se.shape"])
    quantiles <- get_quantile(q_of_interest, locations, scales, shapes)
    dat_knn[dat_knn$dur == durations[dur], "mean_quantile"] <- mean(quantiles)
    dat_knn[dat_knn$dur == durations[dur], "se_quantile"] <- sqrt(var(quantiles))
  }
  
  
  # neural network model
  dat_NN <- data_GEV_all_NN[data_GEV_all_NN$id == stations_of_interest & data_GEV_all_NN$month == month_of_interest, ]
  dat_NN$quantile_q <- get_quantile(q_of_interest, dat_NN$location, dat_NN$scale, dat_NN$shape)
  dat_NN <- dat_NN[,c("Duration", "Model", "quantile_q")] %>% 
    group_by(Duration) %>% 
    summarize(mean_quantile = mean(quantile_q), se_quantile = sqrt(var(quantile_q))) %>%
    ungroup() %>%
    as.data.frame()
  # dat_NN$Duration <- as.numeric(levels(dat_NN$Duration)[dat_NN$Duration])
  
  # plot final
  dat_ <- data_monthly_maxima[data_monthly_maxima$id == stations_of_interest & data_monthly_maxima$month == month_of_interest, ]
  
  dat_ref <- dat_ref[,c("dur", "fitted", "se")]
  dat_ref$method <- "reference"
  dat_knn <- dat_knn[,c("dur", "mean_quantile", "se_quantile")]
  colnames(dat_knn) <- c("dur", "fitted", "se")
  dat_knn$method <- "kNN"
  dat_NN <- dat_NN[,c("Duration", "mean_quantile", "se_quantile")]
  colnames(dat_NN) <- c("dur", "fitted", "se")
  dat_NN$method <- "CDN"
  all_results_GEV <- rbind(dat_ref, dat_knn, dat_NN)
  
  list_plots_GEV[[m]] <- ggplot() + 
    geom_boxplot(data = dat_, aes(x = dur, y = int, group = dur)) + 
    
    geom_ribbon(data = all_results_GEV, aes(x = dur, ymin = fitted-2*se, ymax = fitted+2*se, group = method, colour = method, fill = method), 
                alpha = 0.2, colour = "transparent") + 
    geom_line(data = all_results_GEV, aes(x = dur, y = fitted, group = method, colour = method)) + 
    
    scale_fill_viridis(discrete = T, name = "Method") + 
    scale_colour_viridis(discrete = T, name = "Method") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10") + 
    labs(x = "Duration [minutes]", y = "intensity [mm/hr]", title = paste("20-year return levels (", month_of_interest, ")", sep = "")) + 
    theme_for_the_plots + 
    theme(legend.position = "right")
  
}

list_plots_GEV[[1]] <- list_plots_GEV[[1]] + 
  labs(x = NULL, title =paste("20-year return levels, station ", stations_of_interest, "\n", sep = ""), subtitle = monthss[1], y = NULL) + 
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  scale_y_continuous(limits = c(0.5, 250), trans = "log10")
list_plots_GEV[[2]] <- list_plots_GEV[[2]] + 
  labs(x = NULL, title =NULL, subtitle = monthss[2], y = NULL) + 
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) +
  scale_y_continuous(limits = c(0.5, 250), trans = "log10")

list_plots_GEV[[3]] <- list_plots_GEV[[3]] + 
  labs(x = "Duration [minutes]", title =NULL, subtitle = monthss[3], y = NULL) + 
  theme(plot.margin = margin(0.1, 0.5, 0.1, 0.5, "cm")) + 
  scale_y_continuous(limits = c(0.5, 250), trans = "log10")

p1 <- list_plots_GEV[[1]]
p2 <- list_plots_GEV[[2]]
p3 <- list_plots_GEV[[3]]
p_gev_final <- p1 + p2 + p3 + plot_layout(nrow = 3, ncol = 1, guides = "collect")

ggsave(paste("results/Figures/chapter_3_univariate_analysis/plot_GEV_comparison_", stations_of_interest, ".pdf", sep = ""), width = 20, height = 35, units = "cm", plot = p_gev_final)






