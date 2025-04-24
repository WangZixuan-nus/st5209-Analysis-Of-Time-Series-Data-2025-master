# ---
# title: "Week 11 Notes"
# format: pdf
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
library(gridExtra)
library(tseries)
# 
# # **13  ARIMA Models**
# 
# # PRE recorded video
# 
# ## **Forecasting**
# 
# generate sample trajectories and forecasts from the following three models:
# 
# $$(I−0.5B)X_t=0.1+W_t\\(I−0.5B)(I−B)Y_t=0.1+W_t\\(I−0.5B)(I−B)^2Z_t=0.1+W_t$$We use the same white noise process to create all three trajectories, so the sequence (Yt) is the discrete integral of (Xt), and likewise, (Zt) is the discrete integral of (Yt).
# 
set.seed(5209)
n <- 100
h <- 50
arima_data <-
    tibble(t = 1:n,
           wn = rnorm(n) + 0.1,
           X = arima.sim(model = list(ar = 0.5),
                         n = n),
                         innov = wn,
           Y = arima.sim(model = list(ar = 0.5, order = c(1, 1, 0)), 
                         n = n - 1, innov = wn),
           Z = arima.sim(model = list(ar = 0.5, order = c(1, 2, 0)), 
                         n = n - 2, innov = wn)
    ) |>
    as_tsibble(index = t)
plt1 <- arima_data |>
    model(X = ARIMA(X ~ pdq(1, 0, 0) + 1, 
          fixed = list(ar1 = 0.5, constant = 0.2))) |> 
    forecast(h = h) |>
    autoplot(arima_data) + ylab("Xt,d=0")
plt2 <- arima_data |>
    model(Y = ARIMA(Y ~ pdq(1, 1, 0) + 1, 
          fixed = list(ar1 = 0.5, constant = 0.2))) |>
    forecast(h = h) |>
    autoplot(arima_data) + ylab("Yt,d=1")
plt3 <- arima_data |>
    model(Z = ARIMA(Z ~ pdq(1, 2, 0) + 1, 
          fixed = list(ar1 = 0.5, constant = 0.2))) |>
    forecast(h = h) |>
    autoplot(arima_data) + ylab("Zt,d=2")
grid.arrange(plt1,plt2,plt3)
# 
# Notice how the shape of the forecast curves depend on d:
# 
# If d=0, then it converges to the mean $\mu$=0.2.
# 
# If d=1, then it converges to a straight line with slope $\mu$.
# 
# If d=2, then it converges to a quadratic with curvature $\mu$.Moreover, for d\>1, the width of the prediction intervals grows as h increases. Bear in mind the y-axis scale of the plots. Finally, we see that the trajectories become smoother as d increases.
# 
# Unit root tests
# 
sunspots <- readRDS("../_data/cleaned/sunspots.rds")
sunspots$Sunspots |> adf.test()
# 
sunspots$Sunspots |> kpss.test()
# 
# ### **Sunspots**
# 
# We shall simultaneously fit an AR(2) model as before as well as let ARIMA() perform an automatic search.
# 
sunspots_fit <- sunspots |>
    model(ar2 = ARIMA(Sunspots ~ pdq(2, 0, 0) + 1),
          search = ARIMA(Sunspots))
sunspots_fit
# 
# We see that ARIMA() has returned an ARMA(2,1) model with a mean term. We can inspect the log likelihood, AIC, and AICc values for each of these models by calling glance():
# 
sunspots_fit |> glance()
# 
# Here, we see that the ARMA(2,1) model indeed has a smaller AICc value. To view the model parameters, we can use tidy():
# 
sunspots_fit |> tidy()
# 
# To check whether ARMA(2,1) model is indeed better, we can use time-series cross-validation.
# 
sunspots |>
    stretch_tsibble(.init = 100, .step = 10) |>
    model(ar2 = ARIMA(Sunspots ~ pdq(2, 0, 0) + 1),
          search = ARIMA(Sunspots ~ pdq(2, 0, 1) + 1)) |>
    forecast(h = 5) |>
    accuracy(sunspots) |>
    select(.model, MASE, RMSSE)
# 
# Here, we see that the comparison is a bit ambiguous. Finally, we perform a residual analysis to check whether the model is a good fit.
# 
sunspots_fit |> select(search) |> gg_tsresiduals()
# 
# The ACF plot shows that there are some residual autocorrelations, which means that the model is not a very good fit. The residual density plot shows that there is some skewness in favor of positive residuals, which implies that a Box-Cox transformation might be appropriate.
# 
# ### **Tourist arrivals**
# 
# We now fit ARIMA and exponential smoothing models to the Australian tourist arrivals dataset.
# 
aus_fit <- aus_arrivals |>
    filter(Origin == "Japan") |>
    model(arima = ARIMA(Arrivals),
          ets = ETS(Arrivals))
aus_fit
# 
# Here, we see that ARIMA() selects a seasonal ARIMA(0,1,1)(1,1,1)4 model, while ETS() fits a Holt-Winters model with multiplicative noise and seasonality.
# 
aus_arrivals |>
    filter(Origin == "Japan") |>
    stretch_tsibble(.init = 50, .step = 1) |>
    model(arima = ARIMA(Arrivals ~ pdq(0, 1, 1) + PDQ(1, 1, 1, period = 4)),
          ets = ETS(Arrivals ~ error("M") + trend("A") + season("M"))) |>
    forecast(h = 5) |>
    accuracy(aus_arrivals) |>
    select(.model, MASE, RMSSE)
# 
# # LECTURE
# 
# ## 1. Unit root tests
# 
# Load the sunspots data via the following code snippet.
# 
sunspots <- readRDS("../_data/cleaned/sunspots.rds")
sunspots %>% autoplot()
# 
# The time series looks stationary.
# 
# a.  Before running the tests, do you expect the ADF test to have a small or big p-value?
# 
#     we expect the ADF test to have a relatively small p-value (we reject the null hypothesis that a unit root exists),
# 
# b.  Before running the tests, do you expect the KPSS test to have a small or big p-value?
# 
#     We expect KPSS test to have a relatively large p-value (we do not reject the null hypothesis that the time series is stationary).
# 
# c.  Run the ADF test using `adf.test` and interpret the results.
# 
    sunspots$Sunspots |> adf.test()
# 
#     We indeed get a small p-value.
# 
# d.  Run the KPSS test using `kpss.test` and interpret the results.
# 
    sunspots$Sunspots |> kpss.test()
# 
#     We indeed get a relatively large p-value. We do not reject the null hypothesis.
# 
# e.  What do the warnings in the test print out mean?
# 
#     The warnings are that the true p-value is different from the printed p-value. This is because the printed p-values are computed by interpolating values from a table. (approximation or a bound)
# 
# ## 2. ARIMA and the intercept term
# 
# a.  Fit an ARIMA model to the sunspots data. What model orders were chosen?
# 
    # fit a ARMA model 
    sunspots %>% 
      model(arma = ARIMA(Sunspots ~ pdq(d = 0))) %>% 
      tidy()

# 
#     ARMA(2,1)
# 
#     The constant term is 22.96. This is clearly not the mean of the time series. Instead, it appears in the equation as
# 
#     $$ X_t = 1.48X_{t-1} -0.77X_{t-2} - 0.19W_t + 22.96 $$
# 
# b.  Inspect the constant term parameter. Is it equal to the mean of the time series? If not, how is it related to the mean?
# 
#     The mean is
# 
#     -   $$ \mu = \frac{\alpha}{1-\phi_1-\phi_2} = \frac{22.96}{1 - 1.48 + 0.77} \approx 79.17 
#         $$
# 
# c.  If $(X_t)$ is ARIMA with $d=1$, how is the constant term related to the slope of the forecast curve?
# 
#     ![](images/note11_que2_1.jpg){width="800"}
# 
# d.  If $(X_t)$ is ARIMA with $d=2$, how is the constant term related to the curvature of the forecast curve?
# 
# e.  If $(X_t)$ is ARIMA with $d=2$, what happens when the constant term is equal to 0?
# 
#     ![](images/note11_que2_2.jpg){width="800"}
# 
# ## 3. Stochastic vs deterministic trends
# 
# We have seen two methods of modeling trends:
# 
# (i) Deterministic trend: $X_t = Y_t + \beta_0 + \beta_1 t$, where $Y_t$ is ARMA (stationary)
# 
#     (using decomposition, linear trend model)
# 
# (ii) Stochastic trend: $(I-B)X_t$ is ARMA, i.e. $X_t$ is ARIMA(p, 1, q) for some $p$ and $q$.
# 
#      (using differencing)
# 
# In this question, we explore their differences, using the dataset `aus_airpassengers`.
# 
# a.  Fit a deterministic trend model to the time series. What are the model parameters? Write out the modeling equation(s).
# 
    # Deterministic trend
    det_fit <- aus_airpassengers |>
      # trend to fit a linear trend, +1 to fit a constant term 
      model(deterministic =  ARIMA(Passengers ~ pdq(d = 0) +
                                     trend() + 1))
    det_fit
# 
#     \<LM w/ ARIMA(1,0,0) errors\>: It is a linear model, but the error are ARIMA(1,0,0) model.
# 
    det_fit %>% report() 
# 
#     To look at the niose variance parameter, use *`report()`*. sigma\^2 estimated as 4.343.
# 
#     We can get the model equation:
# 
#     $$
#     X_t = Y_t + \beta_0 + \beta_1 t\\ 
#     X_t = Y_t + 0.9014 + 1.4151t
#     $$
# 
#     Y_t is a AR(1) model
# 
#     $$
#     Y_t = 0.9564Y_{t-1} + W_t
#     $$
# 
#     White noise:
# 
#     $$
#     W_t \sim N(0,4.343)
#     $$
# 
# b.  Fit a stochastic trend model to the time series. What are the model parameters? Write out the modeling equation(s).
# 
    # Stochastic trend
    sto_fit <- aus_airpassengers |>
      # difference = 1, and a constant term 
      model(stochastic =  ARIMA(Passengers ~ pdq(d = 1) + 1))
    sto_fit
    sto_fit %>% report()
# 
#     \<ARIMA(0,1,0) w/ drift\> : $X_t$ is ARIMA(p, 1, q)
# 
#     $$
#     X_t = X_{t-1} + W_t + 1.4191\\
#     = X_{t-2}+W_{t-1}+1.4191+W_t + 1.4191\\
#     =X_0 + 1.4191 t + \sum_{s = 1}^{t}W_s
#     $$
# 
#     let call $Y_t = \sum_{s = 1}^{t}W_s$, Y_t is a random walk
# 
#     $$
#     Y_t = Y_{t-1} + W_t
#     $$
# 
#     Then
#     $$
#     X_t = X_0 + 1.4191t + Y_t\\W_t \sim N(0, 4.271)
#     $$
# 
# c.  Compare the two sets of modeling equations.
# 
#     Deterministic trend$$X_t = Y_t + 0.9014 + 1.4151t$$
# 
#     $Y_t$ is a AR(1) model
# 
#     $$Y_t = 0.9564Y_{t-1} + W_t\\W_t \sim N(0,4.343)$$
# 
#     Stochastic trend:
# 
#     $$X_t = X_0 + 1.4191t + Y_t$$
# 
#     $Y_t$ is a random walk
# 
#     $$W_t \sim N(0, 4.271)$$
# 
#     The slope and the variance is similar. ( short term) But the long term may be not. The stochastic trend model allows the "noise" around the trend to be non-stationary, i.e. be a random walk, hence, the time series can deviate far from the trend, even though that is the time series mean.
# 
# d.  How do their forecasts differ?
# 
    plt1 <- det_fit |> 
      forecast(h = 30) |>
      autoplot(aus_airpassengers)

    plt2 <- sto_fit |> 
      forecast(h = 30) |>
      autoplot(aus_airpassengers)

    grid.arrange(plt1, plt2)
# 
#     The point forecast are straight line. But forecast horizon are difference.
# 
#     Deterministic trend: the noise is ARMA(stationary), so the forecast distribution is the marginal distribution of ARMA model. So the width of the prediction interval converges to the fixed width.
# 
#     Stochastic trend: the noise is a random walk( non stationary), so the width of the prediction interval will not converge. as the forecast horizon increases, more uncertain.
# 
# e.  When should you use a stochastic trend instead of a deterministic trend?
# 
    aus_airpassengers |>
      model(stochastic = ARIMA(Passengers ~ pdq(d = 1) + 1),
            deterministic = ARIMA(Passengers ~ trend() + 1 + pdq(d = 0))) |>
      forecast(h = 50) |>
      autoplot(aus_airpassengers, level = 95)
# 
# Should use stochastic trend when we are not sure that the trend is reliable. ( uncertain about the future)
# 
# ## 4. Seasonal ARIMA
# 
# a.  Consider the seasonal naive model $X_t = X_{t-m} + W_t$. Does this have a unit root?
# 
#     Yes, the AR polynomial is
# 
#     $$ \phi(z) = 1-z^m = (1-z)(1+z+\cdots z^{m-1}). $$
# 
#     Hence, $z=1$ is a root (alternatively, we could have just checked $\phi(1) - 0$.
# 
# b.  After taking a first (non-seasonal) difference, what equation does the differenced time series satisfy? Does it have a unit root? Is it stationary?
# 
#     If $Y_t = (I-B)X_t$,
# 
#     We know that $Y_t$ satisfies a AR equation. $\zeta(B)Y_t = W_t$, where $\zeta(z)(1-z) = \phi(z)$
# 
#     Can check that $\zeta(z) = 1+z^2+...+z^{m-1}$
# 
#     We plug in 1, $\zeta(1) = n$, so it does not have unit root.
# 
#     $(Y_t)$ satisfies $Y_t = Y_{t-1} + \cdots + Y_{t-m+1} + W_t$.
# 
#     However, its roots are all complex numbers with absolute value equal to 1. Hence, $(Y_t)$ is not stationary.
# 
# c.  To verify your conclusions in b), simulate from such a model and plot the differenced time series.
# 
    wn <- rnorm(200)
    x <- NULL
    for (i in 1:200) {
      prev_val <- ifelse(i > 4, x[[i - 4]], 0)
      x <- c(x, prev_val + wn[[i]])
    }
    snaive_dat <-
      tibble(t = 1:200,
             x = x) |>
      as_tsibble(index = t)
    snaive_dat |> 
      mutate(y = difference(x, 1)) |> autoplot(y)
# 
# d.  Argue why seasonal non-stationarity should not be tested using unit roots.
# 
#     Even if the seasonal AR polynomial has a unit root, this would imply that the entire AR polynomial has a unit root. We hence cannot distinguish between whether we have "seasonal" non-stationarity or "non-seasonal" non-stationarity using unit roots alone.
# 
# e.  What does the `fable` package use to decide seasonal differences? Check the documentation of the function `unitroot_nsdiffs()`.
# 
    unitroot_nsdiffs(aus_airpassengers$Passengers)
# 
# already stationary
# 
# ## 5. Unit root tests at scale
# 
# The `aus_livestock` dataset contains time series of meat production in Australia for different animals.
# 
# a.  Use `features` together with `unitroot_kpss`, `unitroot_ndiffs`, and `unitroot_nsdiffs` to simultaneously obtain test statistic values, etc. for all time series for `Calves`.
# 
#     The `feasts` package allows us to compute these tests efficiently at scale.
# 
    aus_livestock |>
        filter(Animal == "Calves") |>
        features(Count, c(unitroot_kpss, unitroot_ndiffs, unitroot_nsdiffs))
# 
#     Note that `aus_livestock` is a quarterly time series dataset recording the number of each type of livestock animal in Australia, stratified by state. Here, we focus on the time series measuring the number of calves and have computed the KPSS test statistic and p-value. `unitroot_ndiffs` calculates the appropriate number of first differences based on this value, while `unitroot_nsdiffs` calculates the appropriate number of seasonal differences based on the "strength of seasonality" statistic.
# 
#     Let us do a time plot to get further intuition for these numbers. For ease of interpretation, we only plot values between January 2005 and January 2015 and for the log counts in 3 states: Queensland, Tasmania, and Victoria.
# 
# b.  Plot the log transform of the time series corresponding to Queensland, Tasmania, and Victoria between 2005 Jan and 2015 Jan.
# 
    #| fig-cap: The log counts of the number of calves in Queensland, Tasmania and Victoria.
    #| label: fig-arima-unitroot-plots
    aus_livestock |>
        filter(Animal == "Calves", State %in% c("Victoria", "Tasmania", "Queensland")) |>
        filter_index("2005 Jan" ~ "2015 Jan") |>
        select(Month, State, Count) |>
        autoplot(log(Count))
