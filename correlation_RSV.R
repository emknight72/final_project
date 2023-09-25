library(lubridate)
library(missRanger)
library(tidyverse)
library(readr)

#### E-CIGARETTE ####
# Load GT data
ecig_RSV_full <- read_csv("/Users/emilyknight/Documents/final_project/RSV_data/ecig_RSV_full.csv")
ecig_RSV_full$Time <- as.POSIXct(ecig_RSV_full$Time, format = '%Y-%m-%d')

# convert to long format 
ecig_RSV_long <- ecig_RSV_full %>%
  pivot_longer(cols = -Time, names_to = "geo", values_to = "RSV")
ecig_RSV_long$geo <- str_remove(ecig_RSV_long$geo, "_RSV") # clean up col name
ecig_RSV_long$Time <- as.POSIXct(ecig_RSV_long$Time, format = "%Y-%m-%d")
ecig_RSV_long$RSV <- as.numeric(ecig_RSV_long$RSV)
ecig_RSV_long$year <- lubridate::year(ecig_RSV_long$Time) # add a year col 

# we only want to plot Europe as a whole, so calculate mean RSV across all of Europe for each date
ecig_RSV_long_yearly <- ecig_RSV_long %>%
  group_by(year, geo) %>%
  summarise(yearly_RSV_ecig = median(RSV))


# load eurobarometer data
eurobarom <- read_csv("/Users/emilyknight/Documents/final_project/eurobarometer/eurobarom_relative_long.csv")

# replace ISO for long names
countries <- c("Total" = "Total", "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CY" = "Cyprus", "CZ" = "Czechia",
               "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia","EL" = "Greece", "ES" = "Spain","FI" = "Finland","FR" = "France",
               "HU" = "Hungary","IE" = "Ireland","IT" = "Italy",
               "LT" = "Lithuania","LU" = "Luxembourg","LV" = "Latvia", "MT" = "Malta","NL" = "Netherlands","PL" = "Poland",
               "PT" = "Portugal","RO" = "Romania","SE" = "Sweden", "SI" = "Slovenia","SK" = "Slovakia","UK" = "UK")

# Replace country codes with long country names
eurobarom$geo <- countries[as.character(eurobarom$geo)]

# merge the data
GT_eurobarom_ecig <- left_join(ecig_RSV_long_yearly, eurobarom, by =c("geo", "year"))

# impute missing years of the eurobarometer data
set.seed(1)
GT_eurobarom_ecig <- missRanger(GT_eurobarom_ecig, pmm.k = 3)

# cross correlation per country
unique_countries <- unique(GT_eurobarom_ecig$geo)

cross_corr_results_1 <- list()

for (country in unique_countries) {
  country_data <- GT_eurobarom_ecig[GT_eurobarom_ecig$geo == country, ]
  ccf <- ccf(country_data$yearly_RSV_ecig, country_data$ecig_users)
  # plot the ccf for the current country
  plot(ccf, main = paste("Cross-correlation for 'electronic cigarette' in", country), xlab = "Lag", ylab = "CCF",  ylim = c(-1, 1), yaxp = c(-1, 1, 10))
  # store results of the ccf
  cross_corr_results_1[[country]] <- list(
    cross_corr_values = ccf$acf,
    upper_ci = qnorm((1 + 0.95)/2)/sqrt(ccf$n.used),
    lower_ci <- -qnorm((1 + 0.95)/2)/sqrt(ccf$n.used),
    lags = ccf$lag,
    p_values = 2 * (1 - pnorm(abs(ccf$acf), mean = 0, sd = 1/sqrt(ccf$n.used)))
  )
}

# Create a data frame for plotting
plot_data <- data.frame(
  country = rep(unique_countries, each = length(ccf$lag)),
  lag = rep(ccf$lag, times = length(unique_countries)),
  ccf = unlist(lapply(cross_corr_results, function(x) x$cross_corr_values))
)
# Create the plot using ggplot2
ggplot(plot_data, aes(x = lag, y = ccf, color = country)) +
  geom_line() +
  labs(title = "Cross-correlation for Different Countries",
       x = "Lag",
       y = "Cross-correlation") +
  theme_minimal() +
  theme(legend.position = "right")



# cross correlation for all of Europe
# summarise GT data into RSV per year for all of Europe
ecig_RSV_long_total <- ecig_RSV_long %>%
  group_by(year) %>%
  summarize(median_RSV_ecig = median(RSV))

# summarise eurobarometer data into ecig prevalence per year for all of Europe
eurobarom_total <- eurobarom %>%
  filter(geo == "Total")

# merge
GT_eurobarom_ecig_total <- left_join(ecig_RSV_long_total, eurobarom_total, by ="year")
GT_eurobarom_ecig_total <- GT_eurobarom_ecig_total %>% select(-c("geo")) # drop geo col

# impute
GT_eurobarom_ecig_total <- missRanger(GT_eurobarom_ecig_total, pmm.k = 3)

ccf(GT_eurobarom_ecig_total$median_RSV_ecig, GT_eurobarom_ecig_total$ecig_users, main = "", xlab = "Lag", ylab = "CCF",  ylim = c(-1, 1), yaxp = c(-1, 1, 10))


# spearman?
cor.test(GT_eurobarom_ecig_total$median_RSV_ecig, GT_eurobarom_ecig_total$ecig_users, method = c("spearman"), exact = FALSE)


#### VAPORIZER ####
# Load GT data
vap_RSV_full <- read_csv("/Users/emilyknight/Documents/final_project/RSV_data/vap_RSV_full.csv")
vap_RSV_full$Time <- as.POSIXct(vap_RSV_full$Time, format = '%Y-%m-%d')

# convert to long format 
vap_RSV_long <- vap_RSV_full %>%
  pivot_longer(cols = -Time, names_to = "geo", values_to = "RSV")
vap_RSV_long$geo <- str_remove(vap_RSV_long$geo, "_RSV") # clean up col name
vap_RSV_long$Time <- as.POSIXct(vap_RSV_long$Time, format = "%Y-%m-%d")
vap_RSV_long$RSV <- as.numeric(vap_RSV_long$RSV)
vap_RSV_long$year <- lubridate::year(vap_RSV_long$Time) # add a year col 

# we only want to plot Europe as a whole, so calculate mean RSV across all of Europe for each date
vap_RSV_long_yearly <- vap_RSV_long %>%
  group_by(year, geo) %>%
  summarise(yearly_RSV_vap = median(RSV))


# merge the data with eurobarom data
GT_eurobarom_vap <- left_join(vap_RSV_long_yearly, eurobarom, by =c("geo", "year"))

# impute missing years of the eurobarometer data
set.seed(234)
GT_eurobarom_vap <- missRanger(GT_eurobarom_vap, pmm.k = 3)

# cross correlation per country
unique_countries <- unique(GT_eurobarom_vap$geo)

cross_corr_results_vap_none <- list()

for (country in unique_countries) {
  country_data <- GT_eurobarom_vap[GT_eurobarom_vap$geo == country, ]
  ccf <- ccf(country_data$yearly_RSV_vap, country_data$ecig_users)
  # plot the ccf for the current country
  plot(ccf, main = paste("Cross-correlation for 'vaporizer' in", country), xlab = "Lag", ylab = "CCF",  ylim = c(-1, 1), yaxp = c(-1, 1, 10))
  # store results of the ccf
  cross_corr_results_vap_none[[country]] <- list(
    cross_corr_values = ccf$acf,
    p_values = 2 * (1 - pnorm(abs(ccf$acf), mean = 0, sd = 1/sqrt(ccf$n.used))),
    lags = ccf$lag,
    upper_ci = qnorm((1 + 0.95)/2)/sqrt(ccf$n.used),
    lower_ci <- -qnorm((1 + 0.95)/2)/sqrt(ccf$n.used)
  )
}

write.table(cross_corr_results_vap, "/Users/emilyknight/Documents/final_project/ccf_vap_results")

# cross correlation for all of Europe
# summarise GT data into RSV per year for all of Europe
vap_RSV_long_total <- vap_RSV_long %>%
  group_by(year) %>%
  summarize(median_RSV_vap = median(RSV))

# merge with total eurobarometer data
GT_eurobarom_vap_total <- left_join(vap_RSV_long_total, eurobarom_total, by ="year")
GT_eurobarom_vap_total <- GT_eurobarom_vap_total %>% select(-c("geo")) # drop geo col

# impute
set.seed(2)
GT_eurobarom_vap_total <- missRanger(GT_eurobarom_vap_total, pmm.k = 3)

ccf_europe <- ccf(GT_eurobarom_vap_total$median_RSV_vap, GT_eurobarom_vap_total$ecig_users, main = "", xlab = "Lag", ylab = "CCF",  ylim = c(-1, 1), yaxp = c(-1, 1, 10))



#### SENSITIVITY ANALYSIS, NO IMPUTATION (ELECTRONIC CIGARETTE) ####
# merge the data
GT_eurobarom_ecig2 <- inner_join(ecig_RSV_long_yearly, eurobarom, by =c("geo", "year"))

# cross correlation per country
unique_countries <- unique(GT_eurobarom_ecig2$geo) # get list of unique countries
cross_corr_results <- list()

for (country in unique_countries) {
  country_data <- GT_eurobarom_ecig2[GT_eurobarom_ecig2$geo == country, ]
  ccf <- ccf(country_data$yearly_RSV_ecig, country_data$ecig_users)
  # plot the ccf for the current country
  plot(ccf, main = paste("Cross-correlation for", country), xlab = "Lag")
  # store results of the ccf
  cross_corr_results[[country]] <- list(
    cross_corr_values = ccf$acf,
    upper_ci = qnorm((1 + 0.95)/2)/sqrt(ccf$n.used),
    lower_ci <- -qnorm((1 + 0.95)/2)/sqrt(ccf$n.used),
    p_values = ccf$p,
    lags = ccf$lag
  )
}

# cross correlation for all of Europe
# summarise GT data into RSV per year for all of Europe
ecig_RSV_long_total <- ecig_RSV_long %>%
  group_by(year) %>%
  summarize(median_RSV_ecig = median(RSV))

# summarise eurobarometer data into ecig prevalence per year for all of Europe
eurobarom_total <- eurobarom %>%
  filter(geo == "Total")

# merge
GT_eurobarom_ecig_total2 <- inner_join(ecig_RSV_long_total, eurobarom_total, by ="year")
GT_eurobarom_ecig_total2 <- GT_eurobarom_ecig_total2 %>% select(-c("geo")) # drop geo col

# correlation
ccf(GT_eurobarom_ecig_total2$median_RSV_ecig, GT_eurobarom_ecig_total2$ecig_users, main = "Cross-correlation of GT RSV and Eurobarometer, Europe")



#### SENSITIVITY ANALYSIS, NO IMPUTATION (VAPORIZER) ####
# merge the data with eurobarom data
GT_eurobarom_vap2 <- inner_join(vap_RSV_long_yearly, eurobarom, by =c("geo", "year"))

# cross correlation per country
unique_countries <- unique(GT_eurobarom_vap2$geo)
cross_corr_results <- list()

for (country in unique_countries) {
  country_data <- GT_eurobarom_vap2[GT_eurobarom_vap2$geo == country, ]
  ccf <- ccf(country_data$yearly_RSV_vap, country_data$ecig_users)
  # plot the ccf for the current country
  plot(ccf, main = paste("Cross-correlation for 'vaporizer' term in", country), xlab = "Lag")
  # store results of the ccf
  cross_corr_results[[country]] <- list(
    cross_corr_values = ccf$acf,
    p_values = ccf$p,
    lags = ccf$lag
  )
}

# cross correlation for all of Europe
# summarise GT data into RSV per year for all of Europe
vap_RSV_long_total <- vap_RSV_long %>%
  group_by(year) %>%
  summarize(median_RSV_vap = median(RSV))

# merge with total eurobarometer data
GT_eurobarom_vap_total2 <- inner_join(vap_RSV_long_total, eurobarom_total, by ="year")
GT_eurobarom_vap_total2 <- GT_eurobarom_vap_total2 %>% select(-c("geo")) # drop geo col

ccf(GT_eurobarom_vap_total2$median_RSV_vap, GT_eurobarom_vap_total2$ecig_users, main = "Cross-correlation of GT RSV and Eurobarometer, Europe")






