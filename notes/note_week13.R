# ---
# title: "Week 13 note"
# format: pdf
# editor: visual
# ---
# 
# # **14  Multivariate Models**
# 
# # PRE recorded video
# 
# ## Set up
# 
#| message: false
#| warning: false
library(fpp3)
library(tidyverse)
library(slider)
library(gridExtra)
# 
# ### **Simple linear regression for time series**
# 
us_change |>
  pivot_longer(c(Consumption, Income), names_to="Series") |>
  autoplot(value) +
  labs(y = "% change")
# 
us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# 
# Fit a simple linear regression
# 
# We do so using the `TSLM()` function 
# 
lr_fit <- us_change |>
  model(TSLM(Consumption ~ Income))
lr_fit |> report()
# 
# Interpretation: Each 1% further increase in income leadsto a further 0.27% predicted increase in consumption
# 
lr_fit |> gg_tsresiduals()
# 
# These plots provide evidence that the residuals of the model are not white noise, which violates the linear modeling assumption. This means that we should be suspicious of any conclusions obtained using this model, such as prediction intervals and coefficient interpretations.
# 
# ### **Multiple linear regression for time series**
# 
# pairwise scatter plot:
# 
us_change |>
  GGally::ggpairs(columns = 2:6)
# 
mlr_fit <- us_change |>
  model(TSLM(Consumption ~ Income + Production + Savings + Unemployment))
mlr_fit |> report()
# 
mlr_fit |> gg_tsresiduals()
# 
# ## **Dynamic regression**
# 
consumption_fit <- us_change |>
  model(dr1 = ARIMA(Consumption ~ Income),
        dr2 = ARIMA(Consumption ~ Income + lag(Income, 1))) # 1st lag
consumption_fit |> select(dr1) |> report()
# 
# fitted model can be written as
# 
# $$
# \Delta \text{Consumption}_t=0.59+0.20\Delta \text{Income}_t+\eta_t \\\eta_t =0.71\eta_{t−1}+W_t−0.62W_{t−1}+0.21W_{t−2}\\W_t \sim N(0,0.31)
# $$
# 
consumption_fit |> select(dr1) |> gg_tsresiduals()
# 
# ### **Parameter estimation and model selection**
# 
consumption_fit <- us_change |>
  model(dr1 = ARIMA(Consumption ~ Income + lag(Income, 1)),
        dr2 = ARIMA(Consumption ~ Income + lag(Income, 1) + Production + 
                    lag(Production, 1)),
        dr3 = ARIMA(Consumption ~ Income + lag(Income, 1) + Production + 
                    lag(Production, 1) + Unemployment + lag(Unemployment, 1)),
        dr4 = ARIMA(Consumption ~ Income + lag(Income, 1) + Production + 
                    lag(Production, 1) + Unemployment + lag(Unemployment, 1) + 
                    Savings + lag(Savings, 1)),
        dr5 = ARIMA(Consumption ~ Income + lag(Income, 1) + Production + 
                    lag(Production, 1) + Unemployment + lag(Unemployment, 1) + 
                    Savings + lag(Savings, 1) + lag(Income, 2) + lag(Production, 2) + 
                    lag(Unemployment, 2) + lag(Savings, 2)))
consumption_fit |> glance()
# 
consumption_fit |> select(dr5) |> gg_tsresiduals()
# 
# ## **Vector autoregression (VAR) models**
# 
consumption_fit <- us_change |>
  model(VAR(vars(Consumption, Income)))
consumption_fit
# 
consumption_fit |> 
  tidy() |> select(-.model) |> print(n = 22)

# 
consumption_fit |> 
  augment() |> 
  ACF(.innov) |> 
  autoplot()
# 
# Forecast:
# 
consumption_fit |> 
  forecast(h = 20) |> 
  autoplot(us_change)
# 
# ## **Dynamic harmonic regression**
# 
diabetes <- readRDS("../_data/cleaned/diabetes.rds")
diabetes_dhr_fit <- diabetes |> 
  model(ARIMA(log(TotalC) ~ fourier(K = 6)))
diabetes_dhr_fit
# 
diabetes_dhr_fit |> tidy() |> select(-.model)
# 
diabetes_dhr_fit |> 
  forecast(h = 24) |> 
  autoplot(diabetes)
# 
# # Lecture
# 
# ## 1. Nonlinear trend
# 
# In this problem, we will analyze the `boston_marathon` dataset, which records the winning times for the race from 1897 to 2019 (across several categories). We will focus on the men's event from 1924 onwards. Use the following code snippet to extract the relevant time series.
# 
boston_men <- boston_marathon |>
  filter(Year >= 1924) |>
  filter(Event == "Men's open division") |>
  mutate(Minutes = as.numeric(Time)/60)
# 
# a.  Make a time plot of the time series.
# 
    boston_men %>% autoplot(.vars = Minutes)
# 
# b.  If you were fitting a piecewise linear trend, where would you place the knots?
# 
#     1950,1980
# 
# c.  Fit three models: (i) A linear trend model, (ii) An exponential trend model, (iii) A piecewise linear trend model with the knots selected in b).
# 
#     linear trend model:
# 
    boston_fit <- boston_men %>% model(linear_trend = TSLM(Minutes ~ trend()),
                         exponential_trend = TSLM(log(Minutes) ~ trend()),
                         piecewise_linear = TSLM(Minutes ~ trend(knots = c(1950,1980))))
# 
# d.  Plot the forecasts of all three methods and comment.
# 
    boston_fc <- boston_fit %>% forecast(h = 10)
    boston_fc %>% autoplot(boston_men) + 
      # Also plot the fitted value:
      geom_line(data = augment(boston_fit), 
                aes(x = Year, y = .fitted, color = .model))
# 
#     The piecewise linear trend seems to have the best fit. The forecasts for the linear and exponential trend methods seem to be biased downwards.
# 
# e.  Compare the AICc values of the models.
# 
    boston_fit %>% 
      glance() %>% 
      select(.model, AICc)
# 
#     The exponential trend model has the smallest AICc value.
# 
# f.  Comment on why it may or may not be fair to select from these models using AICc.
# 
#     The exponential trend model has the smallest AICc value. However, because it has **performed a transformation** of the observations, the **likelihood function** of the model is entirely **different**, and it does not make sense to compare its AICc value with those of the other models.
# 
#     The **linear trend** and **piecewise linear trend** models are indeed directly comparable. As expected, the piecewise linear trend model has a smaller AICc model. In general, however, we need to be careful. Because we selected the knots of the piecewise linear trend model *after* having seen the data. This is some additional degrees of freedom that was not accounted for and difficult to model. (Called researcher degrees of freedom).
# 
# ## 2. Scenario-based forecasting
# 
# Consider the `us_change` dataset, which comprises percentage changes in quarterly personal consumption expenditure, personal disposable income, production, savings and the unemployment rate for the US from 1970 to 2016. Imagine you are a policy maker and would like to know what would happen to change in consumption under two different scenarios for change in income, savings, and unemployment.
# 
# a.  Fit a time series regression model of Consumption on Income, Savings, and Unemployment (we refer to the various time series by their column names). What are the fitted coefficients?
# 
    us_fit <- us_change %>% 
      model(lm = TSLM(Consumption ~ Income + Savings + Unemployment))
    us_fit %>% tidy
# 
# b.  We want to examine two scenarios for 2019 Q3 to 2020 Q2 (next 4 quarters). Under scenario 1, there will be a constant growth of 1% and 0.5% respectively for income and savings, with no change in unemployment.
# 
#     Under scenario 2, there will be a constant decrease of 1% and 0.5% respectively for income and savings, with no change in unemployment. Create tsibbles with these values using the `new_data()` function.
# 
#     new_data(): new_data(object, n = number_of_periods)
# 
#     future_df \<- new_data(df, n = 6) ; forecast(model_fit, new_data = future_df)
# 
    # next 4 quarters(4 cols)
    scenario1_dat <-
      new_data(us_change, 4) |>
      mutate(Income = 1, Savings = 0.5, Unemployment = 0)

    scenario2_dat <-
      new_data(us_change, 4) |>
      mutate(Income = -1, Savings = -0.5, Unemployment = 0)
# 
# c.  Forecast change in consumption under the two scenarios in b). Plot and interpret your results.
# 
    # Forecast for scenario1 
    us_fit |>
      forecast(new_data = scenario1_dat, h = 4) |>
      autoplot(us_change)

    # Forecast for scenario2
    us_fit |>
      forecast(new_data = scenario2_dat, h = 4) |>
      autoplot(us_change)

# 
#     Under scenario 1, we forecast change in consumption to be about 0.996%. Under scenario 2, we forecast change in consumption to be about -0.464%.
# 
# d.  When performing the analysis in c), did we view the model in a) as a predictive model or as a causal model?
# 
#     We viewed the model as a causal model. These scenarios involved intervening to change variables via policy, and we are interested in the true value(the real world...) of change in consumption (policy outcome).
# 
# ## 3. Multiple seasonality
# 
# Consider the `bank_calls` dataset, which records the number of calls to a North American commercial bank per 5-minute interval between 7:00am and 9:05pm each weekday over a 33 week period. For computational tractability, we restrict the time series to the first 4 weeks.
# 
#| warning: FALSE
bank_calls_filtered <- bank_calls |>
  filter_index(~ "2003-03-30")
# 
# a.  Make a time plot. What types of seasonality do you observe?
# 
    bank_calls_filtered %>% 
      autoplot(.vars = Calls)
# 
#     Night time and weekends: gaps
# 
    bank_calls_filtered %>% 
        fill_gaps() |>
      autoplot(.vars = Calls)
# 
#     We observe daily and weekly seasonality.
# 
# b.  There are many **gaps** in the time series so we use the following snippet to index according to the observation number.
# 
#     -connect the Friday to the Monday, get rid of the gaps
# 
calls <- bank_calls_filtered |>
  mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)
# 
# c.  Under this indexing, what are the periods of the seasonality?
# 
    calls %>% autoplot(.vars = Calls)
# 
#     per 5-minute observations, 5hours in the morning , 9 hours in the afternoon - \> 14 hours
# 
#     total number of observations = 14\*12 +1 = 169 (14\*12 blocks + 9:00-9:05)
# 
#     The daily seasonality has period 169. The weekly seasonality has period 169 \* 5 = 845.
# 
# d.  Apply STL decomposition with these seasonal periods. Which seasonality is stronger?
# 
    # cannot automatical decide the seasonality 
    # mannuly set the seasonal periods:
    calls |> 
      model(STL(Calls ~ season(period = 169) + season(period = 845))) |> 
      components() |> 
      autoplot()
# 
#     Daily seasonality is stronger than weekly seasonality.( scales )
# 
# e.  Fit a dynamic harmonic regression model to this time series and compare its prediction performance with time series regression with seasonal dummies as well as DHR without an ARIMA terms.
# 
    # not correct...
    calls_fit <- calls |> 
      model(dhr = ARIMA(Calls ~ 
                          # No seasonal term in ARIMA, no differencing 
                          PDQ(0, 0, 0) + pdq(6, 0, 0) + 1 +
                          # Daily seasonality 
                          fourier(period = 169, K = 10) + 
                          # Weekly seasonality
                          fourier(period = 5 * 169, K = 5)),
            # regression
            seasonal_dummy = TSLM(Calls ~ season(5 * 169)),
            # harmonic regression 
            hr = TSLM(Calls ~ fourier(period = 5 * 169, K = 100)))
# 
calls_fit <- calls |> 
  model(dhr = ARIMA(Calls ~ PDQ(0, 0, 0) + pdq(d = 0) + 
                      # Daily seasonality 
                      fourier(period = 169, K = 10) + 
                      # Weekly seasonality
                      fourier(period = 5 * 169, K = 5)),
        # TSLM
        # seasonal dummy variables to capture the seasonality
        seasonal_dummy = TSLM(Calls ~ season(5 * 169)),
        # hr, use fourier to capture seasonality
        hr = TSLM(Calls ~ fourier(period = 5 * 169, K = 100)))
# 
# Test set 
calls_test <- bank_calls |>
  mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)
# Forecast for next week
calls_fit |> forecast(h = 169 * 5) |> accuracy(calls_test)
# 
# Use the seasonal dummy - more parameters
# 
# k=100, 100 cos() and 100 sin() -\> total 200 parameters ( fewer parameters)
# 
# fit well and smaller variance
# 
# ## 4. Electricity demand forecasting
# 
# Consider the `vic_elec` dataset, which measures the half-hourly electricity demand in Victoria, Australia, between 2012 and 2014. The time series has daily, weekly, and yearly seasonality. Furthermore, it also has two other covariates, measuring the daily temperature, and a dummy for public holidays. Let us use all this information to build a forecasting model.
# 
# a.  Create a dummy for whether the day is a working day.
# 
    elec <- vic_elec |>
      mutate(
        # day of week 
        DOW = wday(Date, label = TRUE),
        # workingday : not holiday and not in weekends
        WorkingDay = !Holiday & !(DOW %in% c("Sat", "Sun"))
      )
    view(elec)
# 
# b.  Make a scatter plot of Demand against Temperature, categorized according to whether the day is a working day. What do you observe?
# 
    elec |>
      ggplot(aes(x = Temperature, y = Demand, color = WorkingDay)) + geom_point(alpha = 0.6)
# 
#     \
#     Demand has a nonlinear relationship with Temperature and is also affected by whether the day is a working day.( quadratic?)
# 
# c.  Fit a dynamic harmonic regression model for Demand, using a **quadratic** function of the Temperature and the working day dummy as predictors.
# 
    elec_fit <- elec |>
      model(ARIMA(Demand ~ WorkingDay + Temperature + 
                    # quadratic function 
                    I(Temperature ** 2) + 
                    # 3 seasonal patterns 
                    fourier(period = "day", K = 10) + 
                    fourier(period = "week", K = 5) + 
                    fourier(period = "year", K = 3) +
                    pdq(6, 0, 0) + 
                    PDQ(0, 0, 0) + 1))
# 
# d.  Forecast the temperature for the next week.
# 
    elec_newdata <- new_data(elec, 7*48) |>
      mutate(
        Temperature = tail(elec$Temperature, 7 * 48),
        Date = lubridate::as_date(Time),
        DOW = wday(Date, label = TRUE),
        WorkingDay = (Date != "2015-01-01") &
                       !(DOW %in% c("Sat", "Sun"))
      )
    fc <- elec_fit |>
      forecast(new_data = elec_newdata)

    fc |> autoplot(tail(elec, 48 * 14))
# 
saveRDS(elec_fit, file = "elec_dhr_fit")
