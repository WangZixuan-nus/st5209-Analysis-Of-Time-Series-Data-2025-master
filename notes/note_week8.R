# ---
# title: "Week 8 Demonstration"
# format: pdf
# editor: visual
# ---
# 
# # **11  Autoregressive Models**
# 
# ## Set up
# 
#| message: false
#| warning: false
library(fpp3)
library(tidyverse)
library(slider)
library(gridExtra)
library(ggplot2)
library(dplyr)
par(mar = c(5, 4, 4, 2) + 0.1)
# 
#| include: false
require(rlang)
require(magrittr)

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

sim_ar <- function(ar_model) {
  n <- ar_model |> augment() |> nrow()
  alpha <- ar_model |> tidy() |> pluck("estimate", 1)
  ar_coefs <- ar_model |> tidy() |> pluck("estimate") |> extract(-1)
  p <- length(ar_coefs)
  mu <- alpha / (1 - sum(ar_coefs))
  sigma2 <- ar_model |> pluck(1, 1, "fit", "sigma2")
  xt <- arima.sim(n = n, model = list(order = c(p, 0, 0), ar = ar_coefs), 
                  sd = sqrt(sigma2)) + mu
  xt
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
# ## Prerecorded videos
# 
# ### **11.1 Motivation**
# 
sunspots <- read_rds("D:/AAnus/semester2/ST5209X Analysis Of Time Series Data/st5209-2025-master/_data/cleaned/sunspots.rds") 
sunspots |> autoplot(Sunspots)
# 
# ### **11.1.2 A sinusoidal model**
# 
sunspots_fit <- sunspots |>
    model(sin_model = TSLM(Sunspots ~ sin(2 * pi * Year / 11) + 
                           cos(2 * pi * Year / 11)))
#calculating the standard deviation (sigma) of the residuals
sigma <- sunspots_fit |>
    pluck(1, 1, "fit", "sigma2") |>
    sqrt()
# 
sunspots_fit_values <- sunspots_fit %>%
  augment() %>%
  select(Year, .fitted)

# Combine observed and fitted values
combined_data <- sunspots %>%
  left_join(sunspots_fit_values, by = "Year")

# Plotting the observed time series and fitted values
ggplot(data = combined_data, aes(x = Year)) +
  geom_line(aes(y = Sunspots, color = "Observed")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Observed and Fitted Sunspots Time Series",
       x = "Year",
       y = "Sunspots",
       color = "Legend") +
  theme_minimal()
# 
set.seed(5209)
sunspots_fit |>
    augment() |> 
    mutate(Sample = .fitted + rnorm(nrow(sunspots)) * sigma,
           Observed = sunspots$Sunspots) |>
    rename(Fitted = .fitted) |>
    pivot_longer(cols = c("Fitted", "Observed", "Sample"),
                 values_to = "Number",
                 names_to = "Type") |>
    filter(Year > 1900) |>
    ggplot() +
    geom_line(aes(x = Year, y = Number, color = Type)) + 
    scale_color_manual(values = c("Fitted" = "blue", "Observed" = "black", "Sample" = "orange"))
# 
# ### **11.1.5 An autoregressive model**
# 
sunspots_fit <- sunspots |>
    model(ar_model = AR(Sunspots ~ order(2)))
sigma <- sunspots_fit |>
    pluck(1, 1, "fit", "sigma2") |>
    sqrt()

set.seed(5209)
sunspots_fit |>
    augment() |> 
  # new columns : Sample and  Observed
    mutate(Sample = sim_ar(sunspots_fit),
           Observed = sunspots$Sunspots) |>
    rename(Fitted = .fitted) |>
    pivot_longer(cols = c("Fitted", "Observed", "Sample"),
                 values_to = "Number",
                 names_to = "Type") |>
    filter(Year > 1900) |>
    ggplot() +
    geom_line(aes(x = Year, y = Number, color = Type)) + 
    scale_color_manual(values = c("Fitted" = "blue", "Observed" = "black", "Sample" = "orange"))
# 
# residuals
# 
sunspots_fit |>
    gg_tsresiduals()
# 
# Load necessary library
library(stats)

# Set the seed for reproducibility
set.seed(123)

# Generate AR(1) model with phi = 0.3
ar_model_0.3 <- arima.sim(n = 100, list(ar = 0.3))

# Generate AR(1) model with phi = 0.9
ar_model_0.9 <- arima.sim(n = 100, list(ar = 0.9))

# Generate AR(1) model with phi = -0.9
ar_model_neg_0.9 <- arima.sim(n = 100, list(ar = -0.9))

# Plotting the time series
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))   # Set up the plotting area to have 3 rows and 1 column

# Plot the AR(1) model with phi = 0.3
plot(ar_model_0.3, type = "l", main = "AR(1) Model with phi = 0.3", ylab = "Value", xlab = "Time")

# Plot the AR(1) model with phi = 0.9
plot(ar_model_0.9, type = "l", main = "AR(1) Model with phi = 0.9", ylab = "Value", xlab = "Time")

# Plot the AR(1) model with phi = -0.9
plot(ar_model_neg_0.9, type = "l", main = "AR(1) Model with phi = -0.9", ylab = "Value", xlab = "Time")
# 
# ### **10.9 Testing for white noise**
# 
# Visual “test”
# 
set.seed(123)
white_noise <- rnorm(100)

ts.plot(white_noise, main = "White Noise Time Series", ylab = "Value", xlab = "Time", col = "blue", lwd = 2)

acf(white_noise, main = "ACF of White Noise")
# 
apple_2015 <- gafa_stock |> 
    filter(Symbol == "AAPL") |> 
    filter(year(Date) == 2015) |> 
    mutate(trading_day = row_number()) |>
    as_tsibble(index = trading_day, regular = TRUE)

apple_2015 |>
    autoplot(Close)
# 
# gg_tsresiduals() has automatically generated three plots for the residuals of the model:
# 
# -   (Top) A time plot
# 
# -   (Bottom left) An ACF plot
# 
# -   (Bottom right) A histogram
# 
apple_fit <- apple_2015 |> 
    model(Naive = NAIVE(Close))
    
apple_fit |> 
    gg_tsresiduals()
# 
# Ljung-Box test
# 
apple_fit |>
    augment()
# 
# perform Ljung-Box on the innovation residuals
# 
apple_fit |>
    augment() |>
    features(.innov, ljung_box, lag = 10)
# 
# do not reject the null hypothesis that the residuals are white noise. In other words, the random walk model / naive method seems to have a good fit to the data.
# 
# **Example 2: Tourist arrivals to Australia**
# 
arrivals_remainder <- aus_arrivals |>
    filter(Origin == "UK") |>
    model(
        classical_decomposition(Arrivals, type = "multiplicative")
        ) |>
    components() |>
    select(Quarter, random)
# 
arrivals_remainder |>
    autoplot(random)
# 
arrivals_remainder |>
    ACF(random) |>
    autoplot()
# 
arrivals_remainder |>
    features(random, ljung_box, lag = 8)
# 
#  remainder component is significantly different from white noise
# 
# # Lecture
# 
# Main Idea:
# 
# time series -\> make time series stationary -\> fit a time series model -\> check whether the residuals are white noise -\>\[if the residuals are white noise\] : the model fit well -\>\[if the residuals are NOT white noise\] fit another model
# 
# ## 1. Residual analysis
# 
# a.  What is the difference between innovation residuals and regular residuals? Why do we perform residual analysis on the former rather than the latter?
# b.  Starting with the following code snippet, fit several models to the Australian takeaway turnover time series and analyze their residuals. Which models have a good fit? Compare this with their CV error.
# 
takeaway <- aus_retail |>
  filter(Industry == "Takeaway food services") |>
  summarise(Turnover = sum(Turnover))
takeaway |> autoplot(.vars = Turnover)
# 
# fit a model:
# 
takeaway_fit <- takeaway |> 
  model(ets_log = ETS(log(Turnover)))

takeaway_fit

# 
# ETS(A,A,A): noise: additive, trend: addtive no damping, seasonality : additive
# 
# **-The residuals:**
# 
takeaway_fit |> augment()
# 
# innovation residuals: because of transformation, = log_turnover - log_fitted value
# 
# IF donot perform transformation
# 
takeaway_notrans_fit <- takeaway |> 
  model(ets = ETS(Turnover ~ error("A")))
takeaway_notrans_fit |> augment()
# 
# innovation residuals and residuals are the same
# 
# residuals analysis : ( innovation residuals)
# 
takeaway_fit |> gg_tsresiduals()
# 
# From the picture, the innovation residuals are not white noise. Since ACF plot, all the spikes are too big ( correlations are too strong).
# 
# -   Test for white noise : ljung_box test
# 
takeaway_fit <- takeaway |>
  model(ets_log = ETS(log(Turnover)),
        ets = ETS(Turnover),
        mean = MEAN(Turnover),
        naive = NAIVE(Turnover),
        drift = NAIVE(Turnover~drift()),
        snaive = SNAIVE(Turnover ~ drift())
        )

# Ljung_box test
takeaway_fit |> 
  augment() |>
  features(.innov, ljung_box, lag = 24) |>
  arrange(lb_stat)
# 
# ets has the smallest p value.
# 
# **Ljung-Box test:**
# 
# $$
# Q_{LB}:=n(n+2)\sum_{j=1}^{h}\frac{\hat{\rho}_X(j)^2}{n-j}
# $$
# 
# IF x is white noise -\> no matter the h value we pick, Q_LB will converge to $\chi^2_h$
# 
# select h appropritately:
# 
# h=min{10,n/5}for non-seasonal data, and h=min{2p,n/5} for seasonal data
# 
# -   why lag = 24?
# 
# The time series have yearly seasonality, preriod = 12, so 2\*p = 24.
# 
# For $\chi^2_h,\text{mean} = h =24$, so the lb_stat is much bigger than 24.
# 
# IN SUMMARY, we can reject the H0 that the residuals are white noise.
# 
# **time series cross-validation**
# 
takeaway_cv <- takeaway |>
  stretch_tsibble(.step = 20, .init = 200) 
takeaway_fit <- takeaway_cv|>
  model(
    mean = MEAN(Turnover),
    naive = NAIVE(Turnover),
    drift = NAIVE(Turnover ~ drift()),
    snaive = SNAIVE(Turnover),
    ets = ETS(Turnover ~ error("M") + trend("Ad") + season("M")),
    ets_log = ETS(log(Turnover) ~ error("A") + trend("A") + season("A"))
    )
# 
takeaway_fc <- takeaway_fit|>
  forecast(h = 6) |>
  accuracy(takeaway) |>
  select(.model, MASE,RMSSE) |>
  arrange(MASE)
takeaway_fc
# 
# ets_log has the smallest MASE.
# 
# **How can we understand Cross-validation and Residual analysis?**
# 
# Cross-validation is a method for affair comparing multiple models. It tells which model has the best performance.
# 
# Residual analysis is not for comparing multiple models. When we fit one model at a time, we test if it is a good fit to a data (whether satisfy the assumption of the model). If not a good model, we try to fit another model. Residual analysis tells whether the performance is good or bad.
# 
# ## 2. ACF for AR(2)
# 
# a.  Using `ARMAacf`, plot the ACF for AR(2) models with the following coefficients:
# 
# -   $(X_t)$: $\phi_1 = 1.5$, $\phi_2 = -0.55$
# 
# -   $(Y_t)$: $\phi_1 = 1.5$, $\phi_2 = -0.75$
# 
#     Consider the AR(2) equation $$ X_t = 1.5 X_{t-1} - 0.55 X_{t-2} + W_t. $$
# 
#     $$
#     Y_t = 1.5Y_{t-1}-0.75Y_{t-2}+W_t
#     $$
# 
#     ```{r}
#     # get the sequence of the acf values
#     acf_dat <- tibble(h = 0:30, 
#                       X_acf = ARMAacf(ar = c(1.5, -0.55), lag.max = 30),
#                       Y_acf = ARMAacf(ar = c(1.5, -0.75), lag.max = 30))
# 
#     plt1 <- acf_dat |>
#       ggplot() + 
#       geom_linerange(aes(x = h, ymin = 0, ymax = X_acf))
# 
#     plt2 <- acf_dat |>
#       ggplot() + 
#       geom_linerange(aes(x = h, ymin = 0, ymax = Y_acf))
# 
#     grid.arrange(plt1,plt2)
#     ```
# 
# -   What is the qualitative behavior of the ACFs?
# 
# -   From the ACF plots, try to guess what patterns you may observe from the sample trajectories.
# 
#     For X_t, the ACF value has exp decay. For Y_t, the absolate value of ACF has exp decay. But it has a sinusoidal pattern.
# 
#     Y_t have cycles. From the acf values, the period = 12.
# 
# ## 3. Sample trajectories
# 
# a.  Using `arima.sim()`, draw sample trajectories from the AR(2) models in Q2.
# 
#     Simulate from an ARIMA Model:
# 
#     -   Description:
# 
#         Simulate from an ARIMA model.
# 
#     -   Usage
# 
#         arima.sim(model, n, rand.gen = rnorm, innov = rand.gen(n, ...), n.start = NA, start.innov = rand.gen(n.start, ...), ...)
# 
#     ```{r}
#     #generate sample trajectories
#     set.seed(5209)
#     ar2_dat <- tibble(t = 1:80, 
#            #generate the gaussion noise
#            whitenoise = rnorm(80), 
#            X = arima.sim(model = list(ar = c(1.5, -0.55)),n = 80, innov = whitenoise), 
#            Y = arima.sim(model = list(ar = c(1.5, -0.75)),n = 80, innov = whitenoise))|>
#       as_tsibble(index = t)
# 
#     plt1 <- ar2_dat |> autoplot(X)
#     plt2 <- ar2_dat |> autoplot(Y)
#     grid.arrange(plt1,plt2)
#     ```
# 
#     X_t is more like a white noise. Y_t have some cycle pattern. So the some change in the model parameters($\phi_2: -0.55 \text{ to} -0.75$) cause a big change of the behavior the time series.
# 
# b.  Fit AR(2) models to these generated time series using `ARIMA()`.
# 
# c.  What are the fitted coefficients?
# 
#     ```{r}
#     X_mod <- ar2_dat |>
#       model(X = AR(X~order(2)))
# 
#     Y_mod <- ar2_dat |>
#       model(Y = AR(Y~order(2)))
# 
#     # coefficients:
#     X_mod |>
#       tidy()
#     ```
# 
#     $\hat{\phi_1}=1.3827952,\text{while the true value is}1.5. .\hat{\phi_2}=-0.4815748,\text{while the true value is}-0.55$
# 
#     So the estimate is good.
# 
#     ```{r}
#     # coefficients:
#     Y_mod |>
#       tidy()
#     ```
# 
# $\hat{\phi_1}=1.4551972,\text{while the true value is}1.5.\hat{\phi_2}=-0.7468688,\text{while the true value is}-0.75$
# 
# So the estimate is good.
# 
# a.  Plot the forecast curves.
# 
#     ```{r}
#     plt3 <- X_mod |>
#       forecast(h = 20) |>
#       autoplot(ar2_dat)
#     plt4 <- Y_mod |>
#       forecast(h = 20) |>
#       autoplot(ar2_dat)
#     grid.arrange(plt3,plt4)
#     ```
# 
# Prediction horizon = 50:
plt5 <- X_mod |>
  forecast(h = 50) |>
  autoplot(ar2_dat)
plt6 <- Y_mod |>
  forecast(h = 50) |>
  autoplot(ar2_dat)
grid.arrange(plt3,plt4)
# 
# The width of the prediction intervals does not change. Because it is a stationary time series.
# 
# The width of the prediction intervals increases, but after a while, it remains the same. As the forecast horizons increases, forecast distribution eventually converges to the marginal distribution of a single time point. Because the time series is stationary, every marginal distribution is the same.
# 
# ## 4. AR(2) solution
# 
# Consider the AR(2) equation $$
# X_t = 1.5 X_{t-1} - 0.75 X_{t-2} + W_t.
# $$
# 
# a.  What is the autoregressive polynomial
# 
#     For the AR(2) equation: $X_t = 1.5 X_{t-1} - 0.75 X_{t-2} + W_t,$
# 
#     the autoregressive polynomial $\phi(B)$ in terms of the backshift operator B is given by: $\phi(B) = 1 - 1.5B + 0.75B^2$
# 
# b.  What is the formula for the stationary solution
# 
#     ![](images/note8question4.jpg){width="1080"}
# 
# ## 5. Nonlinear autoregressive models
# 
# Fit a nonlinear AR model on the `globtemp` dataset. Compute its sum of squared residuals and compare it to that of a linear AR model.
