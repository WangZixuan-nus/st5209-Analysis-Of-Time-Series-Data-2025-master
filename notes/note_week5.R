# ---
# title: "Week 5 note"
# author: "Wang Zixuan"
# format:
#   pdf:
#     toc: true
#     toc-depth: 3 
#     number-sections: true 
# editor: visual
# ---
# 
# ## Set up
# 
#| message: false
#| warning: false
library(fpp3)
library(tidyverse)
library(slider)
library(TSA)  
# 
# # pre-reading
# 
# A time series dominated by trend
# 
globtemp <- readRDS("D:/AAnus/semester2/ST5209X Analysis Of Time Series Data/st5209-2025-master/_data/cleaned/globtemp.rds")
globtemp |> 
    as_tsibble() |>
    autoplot()
# 
# Compute ACF values: use ACF() function
# 
globtemp |>
  as_tsibble() |>
  ACF(type = "covariance")
# 
# Compute ACVF:
# 
globtemp |>
  as_tsibble() |>
  ACF(type = "correlation")
# 
# ACF plot(autocorrelation plot)
# 
globtemp |> 
    as_tsibble() |>
    ACF() |>
    autoplot()
# 
# Take a first difference
# 
globtemp |> 
    as_tsibble() |>
    mutate(TempIncr = difference(value)) |>
    ACF(TempIncr) |>
    autoplot()
# 
# A time series dominated by seasonality:
# 
# -   TIME plot:
# 
vic_elec |>
    filter(year(Time) == 2013, month(Time) %in% c(1,2)) |> 
    model(
        STL(Demand ~ trend() + season(period = 48))) |> 
        components() |>
        autoplot(season_48)
# 
# -   ACF PLOT:
# 
vic_elec |>
    filter(year(Time) == 2013, month(Time) %in% c(1,2)) |> 
    model(
        STL(Demand ~ trend() + season(period = 48))) |> 
        components() |> 
        ACF(season_48, lag_max = 72) |> 
        autoplot()
# 
# A time series dominated by cycles:
# 
# -   Time plot
# 
pelt |> 
    autoplot(Lynx)
# 
# Seasonal plot
# 
gg_custom_season <- function(data, y, period, start = 1) {
  # Make a seasonal plot with period specified in integer
  # start argument specifies the row number that will be the first season
  # in the period
  y <- enquo(y)
  data |>
    mutate(Season = (row_number() - start) %% period + start,
           Iteration = as.factor((row_number() - start) %/% period + 1)) |>
    ggplot(aes(x = Season, y = !!y, color = Iteration)) +
    geom_line()
} 
pelt |> 
  as_tsibble()|>
  gg_custom_season(Lynx, period = 10)

# 
# ACF plot
# 
pelt |> 
    ACF(Lynx, lag_max = 40) |>
    autoplot()
# 
# ACF values
# 
aus_arrivals |>
    features(Arrivals, feat_acf)
# 
# ## 8.3 Cross-correlation
# 
pelt |> CCF(x = Lynx, y = Hare) |> autoplot()
# 
# ## **8.4 Decomposition statistics**
# 
aus_arrivals |> 
    features(Arrivals, feat_stl)
# 
aus_arrivals |> gg_season()
# 
# ## **8.5 Other statistics**
# 
aus_arrivals |> 
    features(Arrivals, feature_set(pkgs = "feasts"))
# 
# ## **9.3 Periodogram**
# 
# example: **Star magnitude**
# 
# Time plot
# 
library(astsa)
star |>
    as_tsibble() |>
    rename(Day = index, Value = value) |>
    autoplot() + labs(y = "Star magnitude")
# 
# periodogram
# 
#star |>
   # periodogram(max_freq = 0.08)
# 
# example: **Lynx furs**
# 
# time plot
# 
pelt |>
    autoplot(Lynx)
# 
# periodogram
# 
library(TSA)  
pelt$Lynx |>
    periodogram(max_freq = 0.5)
# 
# example:**US retail employment**
# 
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID, -Title) 
# 
us_retail_employment|>
  model(STL(Employed)) |>
  components() |>
  autoplot()
# 
us_retail_employment |>
    model(STL(Employed)) |>
    components() |> 
    mutate(trend_adjust = Employed - trend) |> 
    pluck("trend_adjust") |> 
    periodogram()
# 
# # CLASS
# 
# ## 1. White noise and ACF plots
# 
# Create a time series of length 200 comprising i.i.d. standard Gaussian measurements. Make a time plot of the time series as well as an ACF plot. Compare this with the remainder term from an STL decomposition of `aus_arrivals_jap`. What do you observe?
# 
wn <- tibble(t = 1:200,
             y = rnorm(200))|>
  as_tsibble(index = t)

wn |> autoplot()
# 
wn |> ACF(y)|>
  autoplot()
# 
aus_arrivals |>
  filter(Origin == "Japan")|>
  model(STL(Arrivals))|>
  components()|>
  autoplot(remainder)
# 
aus_arrivals |>
  filter(Origin == "Japan")|>
  model(STL(Arrivals))|>
  components()|>
  ACF(remainder)|>
  autoplot()
# 
# ## 2. Summary statistics
# 
# Glance through the summary statistics described in [Chapter 4.4](https://otexts.com/fpp3/other-features.html) of Hyndman and Athanasopoulos (2021). What do the following summary statistics compute?
# 
# -   Strength of trend
# -   Strength of seasonality
# -   `shift_level_max`
#     -   finds the largest mean shift between two consecutive sliding windows of the time series. This is useful for finding sudden jumps or drops in a time series.
# -   `shift_level_index`
#     -   gives the index at which the largest mean shift occurs.
# -   `var_tiled_var`
#     -   gives the variances of the “tiled variances” (i.e., the variances of consecutive non-overlapping blocks of observations). This is sometimes called the “lumpiness” feature.
# 
#         ![](lumpiness%20Time%20series.png){width="500"}
# 
# When would they be useful?
# 
# ## 3. EDA with summary statistics
# 
# The follow code snippet generates summary statistics for the various time series in the `tourism` dataset.
# 
#compute features 
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
# 
# information of this time series goes
# 
# Make a scatter plot of seasonal strength against trend strength and color the points according to `Purpose`. What can you tell from the plot?
# 
tourism_features|>
  ggplot()+
  geom_point(aes(x = trend_strength, 
                 y = seasonal_strength_year, 
                 color = Purpose))+
  labs(title = "Seasonal Strength vs. Trend Strength",
       x = "Trend Strength",
       y = "Seasonal Strength",
       color = "Purpose") +
  theme_minimal()
# 
# Travel for holiday is more seasonal.
# 
library(broom)
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)
# 
# Here, we also see a separation between the time series for holiday travel and other time series. We now inspect some of the outlier time series, i.e. those that have especially large values for the first PC
# 
# Perform PCA on the `tourism_features` dataframe and make a scatter plot of the first two PCs. What do you observe?
# 
pcs |> 
  filter(.fittedPC1 > 10) |>
  left_join(tourism) |>
  as_tsibble(index = Quarter, key = c(Region, State, Purpose)) |>
  ggplot(aes(x = Quarter, y = Trips)) + 
  geom_line() + 
  facet_wrap(~ Region)
# 
# Potential reasons why these are outliers:
# 
# -   Holiday visits to the south coast of NSW is highly seasonal but has almost no trend, whereas most holiday destinations in Australia show some trend over time.
# 
# -   Melbourne is an unusual holiday destination because it has almost no seasonality, whereas most holiday destinations in Australia have highly seasonal tourism.
# 
# -   The north western corner of Western Australia is unusual because it shows an increase in business tourism in the last few years of data, but little or no seasonality.
# 
# -   The south western corner of Western Australia is unusual because it shows both an increase in holiday tourism in the last few years of data and a high level of seasonality.
# 
# ## 4. Seasonal periods and frequencies
# 
# Consider the time series $(y_t)$ generated using the following code snippet.
# 
y_dat <- tibble(t = 1:100,
       y = t %% 10) |>
  as_tsibble(index = t)
y_dat |> autoplot()
# 
# we care about amplitude.
# 
require(rlang)

gg_custom_season <- function(data, y, period, start = 1) {
  # Make a seasonal plot with period specified in integer
  # start argument specifies the row number that will be the first season
  # in the period
  y <- enquo(y)
  data |>
    mutate(Season = (row_number() - start) %% period + start,
           Iteration = as.factor((row_number() - start) %/% period + 1)) |>
    ggplot(aes(x = Season, y = !!y, color = Iteration)) +
    geom_line()
}

periodogram <- function(x, max_freq = 0.5) {
  #' Plot a Periodogram for a Time Series
  #'
  #' This function takes a vector representing a time series,
  #' and plots the periodogram of the time series.
  #'
  #' @param x A numeric vector representing the time series.
  #' @param max_freq The max frequency to be plotted
  #'
  #' @return A ggplot object representing the periodogram of the time series.
  #'
  
  # old_warn <- options(warn = -1)  # Disable warnings
  n <- length(x)
  freq <- 0 : (n - 1) / n
  per <- Mod(fft(x - mean(x, na.rm = TRUE))) ^ 2
  tibble(freq = freq, per = per) |>
    ggplot(aes(x = freq, y = per)) +
    geom_segment(aes(xend = freq, yend = 0)) +
    labs(x = "Frequency", y = "Periodogram") +
    theme_minimal() + xlim(0, max_freq)
  # on.exit(options(old_warn))      # Restore warning setting on exit
}
# 
# Plot a periodogram for $(y_t)$. What periods do the peaks correspond to? Why are there multiple peaks when $(y_t)$ was defined using a seasonal period of 10?
# 
periodogram(y_dat$y)
# 
# The frequencies are 0.1, 0.2, 0.3, 0.4, and 0.5. The periods are 10, 5, 3.3, 2.5, 2.
# 
# The periodogram values measure the correlation of the time series with *sinusoidal* functions. Since $(y_t)$ is not sinusoidal, it requires sinusoidal functions of multiple frequencies to build up its waveform. However, the frequencies required are all multiples of the overall waveform frequencies (0.1). Put another way, their periods are all divisors of the period of $(y_t)$.
# 
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title== "Retail Trade") |>
  select(-Series_ID, -Title)

us_retail_employment|>
  autoplot(Employed)

# Decomposition
us_retail_employment |>
  model(STL(Employed)) |>
  components()|>
  mutate(detrend = Employed - trend) |>
  pull(detrend)|>
  periodogram()
# 
# Peak = 0.08(dominate frequency) , 1/0.08 = 12, yearly seasonality , 0.16, 0.24...
# 
# ## 5. Periodogram and filters
# 
# Consider tourist arrivals to Australia from the UK, contained in the `aus_arrivals` dataset.
# 
aus_arrivals |>
  filter(Origin== "UK") |>
  autoplot()
# 
# we did not remove the trend, so the periodogram represent a trend.
# 
# a.  Plot a periodogram for the time series of arrivals from UK. Where are the peaks?
# 
    aus_arrivals |>
      filter(Origin == "UK") |>
      pull(Arrivals) |>
      periodogram(max_freq = 0.5)
# 
#     Peaks are around 0.008 and around 0.25.
# 
#     peak = 0.25, period = 4, A yearly seasonality
# 
# b.  a moving average of window size 4 to the time series. What happens to the periodogram?
# 
#     Still a upward trend. So period canot simply represent the time seires.
# 
#     How do I represent the trends?
# 
#     do some transformations on this time series and observe how these transformations affect the shape of the periodic gram
# 
    # Moving average 
    aus_arrivals |>
      filter(Origin == "UK") |>
      mutate(ArrivalsMA4 = slide_dbl(Arrivals, mean, .before = 2, .after = 1)) |>
      pull(ArrivalsMA4) |>
      periodogram()
# 
#     The peak at 0.25 gets removed. ( Moving average of a window of size 4-\>remove yearly seasonality)
# 
#     If you perform moving average with the window size, then as a multiple of the seasonal periods you remove that seasonality.
# 
#     performing a moving average is applying a low pass philtre to the time series.
# 
#     Low pass philtre means that it preserves the low frequencies and removes the higher frequencies.
# 
# c.  Apply a difference transformation to the time series. What happens to the periodogram?
# 
    aus_arrivals |>
      filter(Origin == "UK") |>
      mutate(ArrivalsDiff = difference(Arrivals, 1)) |>
      filter(!is.na(ArrivalsDiff)) |>
      pull(ArrivalsDiff) |>
      periodogram()
# 
#     a difference is just one, right? So it doesn't change the seasonality because so we still have this seasonality of period 4, but it suddenly removes the trend in the time series.
# 
#     The peaks for low frequencies get removed.
