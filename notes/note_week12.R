# ---
# title: "Week 12 Note"
# format: pdf
# editor: visual
# ---
# 
# # **15  State Space Models**
# 
# ## Set up
# 
#| message: false
#| warning: false
library(fpp3)
library(tidyverse)
library(slider)
library(gridExtra)
library(KFAS)
# 
# # PRE recorded video
# 
# examples of time series data for which a state space model is particularly useful.
# 
library(astsa)
gtemp_dat <- 
  tibble(
    t = 1850:2023,
    ocean = gtemp_ocean/sd(gtemp_ocean),
    land = gtemp_land/sd(gtemp_land)
  ) %>% 
  as_tsibble(index = t) %>% 
  pivot_longer(cols = c("ocean", "land"), 
                 names_to = "Source", 
                 values_to = "Temperature")
gtemp_dat |> autoplot(Temperature) + xlab("Year")
# 
# The two time series, which we denote by (Yt1) and (Yt2), are not independent, but when suitably normalized, can be thought of as noisy measurements of an underlying “true” temperature of the Earth (Xt). Modeling (Xt) as a random walk with drift, we get the following pair of equations.
# 
pelt |>
    pivot_longer(cols = c("Hare", "Lynx"), 
                 names_to = "Animal", 
                 values_to = "Count") |> 
    autoplot(Count)
# 
biomarker <- 
    tibble(Day = 1:length(WBC), 
           WBC = WBC, 
           PLT = PLT, 
           HCT = HCT) |> 
    as_tsibble(index = Day) |> 
    mutate(across(everything(), ~replace(., . == 0, NA)))

plt1 <- biomarker |> autoplot(WBC) + 
    geom_point(aes(x = Day, y = WBC))
plt2 <- biomarker |> autoplot(PLT) + 
    geom_point(aes(x = Day, y = PLT))
plt3 <- biomarker |> autoplot(HCT) + 
    geom_point(aes(x = Day, y = HCT))
grid.arrange(plt1, plt2, plt3, nrow = 3)
# 
# # Lecture 1. Finding code
# 
# Look at the CRAN task views page to find what packages are available for state space models.
# 
# <https://cran.r-project.org/>
# 
# <https://cran.r-project.org/web/views/>
# 
# ## 2. Local linear trend model
# 
# A local linear trend model is defined by the equations $$
# X_t = X_{t-1} + \alpha + W_t,
# $$ $$
# Y_t = X_t + V_t.
# $$ We can interpret this as the time series $(Y_t)$ comprising a "trend" component, modeled as a random walk with drift, and a random component $(V_t)$.
# 
# a.  How can one write this in the form of a Gaussian linear state space model?
# 
#     Can write
# 
#     $$ \left[ \begin{matrix} X_t \\ \alpha \end{matrix} \right] = \left[ \begin{matrix} 1 & 1 \\ 0 & 1 \end{matrix} \right] \left[ \begin{matrix} X_{t-1} \\ \alpha \end{matrix} \right] + \left[ \begin{matrix} W_t \\ 0 \end{matrix} \right] $$
# 
#     $$ Y_t = \left[ \begin{matrix} 1 & 0 \end{matrix} \right]\left[ \begin{matrix} X_t \\ \alpha \end{matrix} \right] + V_t $$
# 
# b.  Simulate a time series of length 50 from this model and plot the results.
# 
    set.seed(5209)
    n = 50
    alpha <- 0.2
    lt_dat <- tibble(
      t = 1:n,
      wt = rnorm(n),
      vt = rnorm(n),
      xt = cumsum(wt) + alpha * (t-1), #cumulative sum of a numeric vector
      yt = xt + vt
      ) 

    lt_dat %>% 
      ggplot(aes(x = t)) +
      geom_line(aes(y = xt, color = "Xt")) +
      geom_line(aes(y = yt, color = "Yt")) +
      scale_color_manual(
        name = "Legend", 
        values = c("Xt" = "black", "Yt" = "blue")
      )

    lt_dat |> 
        as_tsibble(index = t) |>
        pivot_longer(cols = c(xt, yt), names_to = c("Series")) |>
        autoplot(value)
# 
#     We don not know Xt, we only can know Y_t.
# 
# c.  Use the functions `SSModel()` and `SSMcustom()` in the `KFAS` package to specify a state space model. Read the documentation on how to do this.
# 
#     <https://cran.r-project.org/web/packages/KFAS/index.html>
# 
#     Reference manual: <https://cran.r-project.org/web/packages/KFAS/KFAS.pdf>
# 
#     tutorial: <https://cran.r-project.org/web/packages/KFAS/vignettes/KFAS.pdf>
# 
#     Model function:
# 
#     `R> model_gaussian <- SSModel(deaths / population ~ -1 +
#     + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1,
#     + P1inf = P1inf),
#     + H = Ht)`
# 
#     T: tranformation matrix, Z: measurement matrix
# 
    # Parameters
    # tranformation matrix
    Phi <- matrix(c(1,0,1,1),nrow = 2)
    # Measurement matrix 
    A <- matrix(c(1,0),nrow = 1)
    # Covariance of state
    Q <- matrix(c(1, 0, 0, 0), nrow = 2)
    R <- 1
    # Initial stat(guess)
    a1 <- matrix(c(0,alpha), nrow =2)

    lt_model <- SSModel(as.ts(lt_dat$yt) ~ SSMcustom(
      T = Phi, Z = A, Q = Q, R = diag(c(1, 1)), 
      a1 = matrix(c(lt_dat$xt[1], alpha), nrow = 2)) - 1, 
      H = R
    )
    # -1: no constant term 

    # Fit the model
    lt_out <- KFS(lt_model)
    lt_out
# 
# d.  Use `KFS()` to performing filtering, smoothing, and one-step-ahead forecasting for $(X_t)$. Read the documentation to understand the output of the function.
# 
#     We need to extract the value of the model.
# 

    lt_out$P %>% dim()
    # Construct a tibble with all the required data
    lt_dat_new <-
      tibble(lt_dat,
             # Because the data is 2 dimention, but only the 1st col is useful
             # do one-step-ahead forecast, so 51 forecast value 
             forecast = lt_out$a[-(n+1), 1],
             # Filtered value 
             filtered = lt_out$att[, 1],
             # Smoothed value
             smoothed = lt_out$alphahat[, 1],
             # Variance 
             # 1st row, 1st col, remove the last 
             forecast_var = lt_out$P[1, 1, ][-(n+1)],
             filtered_var = lt_out$Ptt[1, 1, ],
             smoothed_var = lt_out$V[1, 1, ]
             )
# 
# e.  Plot the three time series against the true $(X_t)$ values and the observed values $(Y_t)$. Also plot level 0.95 confidence bands.
# 
#     Forecast value
# 
    # Forecast value 

    lt_dat_new |>
      ggplot(aes(x = t)) +
      # observed value: points, mapped to "Observed" in the legend
      geom_point(aes(y = yt, color = "Observed")) +
      # true value: line, mapped to "True" in the legend
      geom_line(aes(y = xt, color = "True")) + 
      # forecast value: line, mapped to "Forecast" in the legend
      geom_line(aes(y = forecast, color = "Forecast")) + 
      # confidence region of forecast (ribbon); do not add it to the legend
      geom_ribbon(aes(ymin = forecast - 1.96 * sqrt(forecast_var),
                      ymax = forecast + 1.96 * sqrt(forecast_var)),
                  fill = "blue", alpha = 0.2, show.legend = FALSE) +
      scale_color_manual(name = "Series", 
                         values = c("Observed" = "black", 
                                    "True" = "black", 
                                    "Forecast" = "blue")) +
      ylab("") + 
      ggtitle("Forecast")
# 
#     Filtered value :
# 
    lt_dat_new |>
      ggplot(aes(x = t)) +
      # Observed values as points, mapped to "Observed" in the legend
      geom_point(aes(y = yt, color = "Observed")) +
      # True values as a line, mapped to "True" in the legend
      geom_line(aes(y = xt, color = "True")) + 
      # Filtered values as a line, mapped to "Filtered" in the legend
      geom_line(aes(y = filtered, color = "Filtered")) + 
      # Confidence region for the filtered values (ribbon) - not added to the legend
      geom_ribbon(aes(ymin = filtered - 1.96 * sqrt(filtered_var),
                      ymax = filtered + 1.96 * sqrt(filtered_var)), 
                  fill = "blue", alpha = 0.2, show.legend = FALSE) +
      # Customizing the legend with a title and manual color mapping
      scale_color_manual(name = "Series", 
                         values = c("Observed" = "black", 
                                    "True" = "black", 
                                    "Filtered" = "blue")) +
      ylab("") + 
      ggtitle("Filtered")
# 
#     Smoothed value :
# 
    lt_dat_new |>
      ggplot(aes(x = t)) +
      # Observed values as points mapped to "Observed" in the legend
      geom_point(aes(y = yt, color = "Observed")) +
      # True values as a line mapped to "True" in the legend
      geom_line(aes(y = xt, color = "True")) + 
      # Smoothed values as a line mapped to "Smoothed" in the legend; set color blue in scale
      geom_line(aes(y = smoothed, color = "Smoothed")) + 
      # Confidence ribbon for smoothed values (excluded from legend)
      geom_ribbon(aes(ymin = smoothed - 1.96 * sqrt(smoothed_var),
                      ymax = smoothed + 1.96 * sqrt(smoothed_var)),
                  fill = "blue", alpha = 0.2, show.legend = FALSE) +
      scale_color_manual(name = "Series", 
                         values = c("Observed" = "black", 
                                    "True" = "black", 
                                    "Smoothed" = "blue")) +
      ylab("") + 
      ggtitle("Smoothed")
# 
# f.  What are the differences between the three time series?
# 
#     Across all three plots, we see that the forecast, filtered, and smoothed values are closer than the observations $Y_t$ to the true values $X_t$.
# 
#     Furthermore, **filtered** values make use of more information and are thus more accurate than forecasts.
# 
#     Similarly, **smoothed** values make use of the most information, and are marginally more accurate than filtered values. ( tighter confidence interval)
# 
#     "accurate": the error is smaller.
# 
#     "precise": the confidence band becomes smaller
# 
# ## 3. Global temperature
# 
# Consider the two time series `gtemp_land` and `gtemp_ocean` discussed in the lecture video. Load, and normalize these time series via the following code snippet.
# 
library(astsa)
temp_dat <- tibble(ocean = gtemp_ocean / sd(gtemp_ocean), 
                   land = gtemp_land / sd(gtemp_land)) 
# 
# a.  What is an appropriate state space model for these time series?
# 
#     An appropriate choice of model is
# 
#     $$ \left[ \begin{matrix} X_t \\ \alpha \end{matrix} \right] = \left[ \begin{matrix} 1 & 1 \\ 0 & 1 \end{matrix} \right] \left[ \begin{matrix} X_{t-1} \\ \alpha \end{matrix} \right] + \left[ \begin{matrix} W_t \\ 0 \end{matrix} \right] $$ $$ \left[ \begin{matrix} Y_{t1} \\ Y_{t2} \end{matrix} \right] =  \left[ \begin{matrix} 1 \\ 1 \end{matrix} \right]X_t +  \left[ \begin{matrix} V_{t1} \\ V_{t2} \end{matrix} \right]. $$
# 
# b.  What are the parameters to be estimated?
# 
#     Parameters to be estimated are the the initial state$X_0$, the state noise covariance $Q$, and the observation noise covariance $R$. To indicate that these are estimated, we fill them in using `NA`.
# 
# c.  Fit the parameters of the model using `fitSSM()`.
# 
    init_state <- matrix(c(NA, NA), nrow = 2)
    Phi <- matrix(c(1, 0, 1, 1), nrow = 2)
    Q <- matrix(c(NA, 0, 0, 0), nrow = 2)
    A <- matrix(c(1, 1, 0 , 0), nrow = 2)
    R <- diag(c(NA, NA))

    temp <- temp_dat |> as.matrix()
    gtemp_model <- SSModel(
      temp ~ SSMcustom(
        T = Phi, Z = A, Q = Q, R = diag(c(1, 1))) - 1, 
      H = R)

    # update function : how to update the parameter of the model
    gtemp_update <- function(pars, model) {
      model["Q"] <- matrix(c(exp(pars[1]), 0, 0, 0), nrow = 2)
      model["H"] <- diag(c(exp(pars[2]), exp(pars[3])))
      model["a1"] <- matrix(c(exp(pars[4]), exp(pars[5])), nrow = 2)
      model
    }

    gtemp_fit <- fitSSM(gtemp_model, updatefn = gtemp_update, inits = rep(0, 5), method = "BFGS")
# 
# d.  Use the fitted model to perform smoothing, and plot the resulting values, together with a level 0.95 confidence band and the two original time series.
# 
    gtemp_out <- KFS(gtemp_fit$model)

    tibble(Year = 1850:2023, temp_dat, 
           smoothed = gtemp_out$alphahat[, 1],
           smoothed_var = gtemp_out$V[1, 1, ]) |>
      ggplot(aes(x = Year)) +
      # Smoothed series with a solid line and associated legend entry
      geom_line(aes(y = smoothed, color = "Smoothed")) +
      # Land series: points and dashed line, same legend entry "Land"
      geom_point(aes(y = land, color = "Land")) + 
      geom_line(aes(y = land, color = "Land"), linetype = "dashed") + 
      # Ocean series: points and dashed line, same legend entry "Ocean"
      geom_point(aes(y = ocean, color = "Ocean")) +
      geom_line(aes(y = ocean, color = "Ocean"), linetype = "dashed") + 
      # Confidence ribbon for the smoothed series (excluded from legend)
      geom_ribbon(aes(ymin = smoothed - 1.96 * sqrt(smoothed_var),
                      ymax = smoothed + 1.96 * sqrt(smoothed_var)),
                  fill = "blue", alpha = 0.2, show.legend = FALSE) + 
      scale_color_manual(name = "Series", 
                         values = c(
                           "Smoothed" = "blue", "Land" = "orange",
                           "Ocean" = "green")) +
      ylab("Temperature Deviation")
# 
#     The variance are independent, orange line and green line do not overlap too much.
# 
# ## 4. Biomarker monitoring
# 
# Load the biomarker dataset using the following code snippet.
# 
biomarker <- 
    tibble(Day = 1:length(WBC), 
           WBC = WBC, 
           PLT = PLT, 
           HCT = HCT) |> 
    as_tsibble(index = Day) |> 
    mutate(across(everything(), ~replace(., . == 0, NA)))
# 
# a.  What is an appropriate state space model for these time series?
# 
# b.  What are the parameters to be estimated?
# 
# c.  Fit the parameters of the model using `fitSSM()`.
# 
# d.  Use the fitted model to perform filtering, and plot the resulting values, together with a level 0.95 confidence band and the two original time series.
# 
# e.  Interpret the fitted parameter values for $\Phi$.
# 
# f.  Why do our results look different compared to those in Figure 6.6 in Shumway and Stoffer (page 315)?
# 
# An appropriate model is
# 
# $$ \left[ \begin{matrix} X_{t1} \\ X_{t2} \\ X_{t3} \end{matrix} \right] = \left[ \begin{matrix} \phi_{11} & \phi_{12} & \phi_{13} \\ \phi_{21} & \phi_{22} & \phi_{23} \\ \phi_{31} & \phi_{32} & \phi_{33} \end{matrix} \right] \left[ \begin{matrix} X_{t-1,1} \\ X_{t-1,2} \\ X_{t-1,3} \end{matrix} \right] +  \left[ \begin{matrix} W_{t1} \\ W_{t2} \\ W_{t3} \end{matrix} \right]. $$ $$ \left[ \begin{matrix} Y_{t1} \\ Y_{t2} \\ Y_{t3} \end{matrix} \right] = A_t\left[ \begin{matrix} X_{t1} \\ X_{t2} \\ X_{t3} \end{matrix} \right] +  \left[ \begin{matrix} V_{t1} \\ V_{t2} \\ V_{t3} \end{matrix} \right]. $$
# 
biomarker_dat <- biomarker |> as_tibble() |> select(-Day) |> 
    as.matrix()
T <- matrix(NA, 3, 3) # state transition
Z <- diag(1, 3) # observation matrix
R <- diag(1, 3) # noise embedding
Q <- diag(NA, 3) # state noise
H <- diag(0, 3) # obs noise
a1 <- biomarker_dat[1, ] # initial state

biomarker_model <- SSModel(biomarker_dat ~ 
                           SSMcustom(Z = Z, T = T, R = R, 
                                     Q = Q, a1 = a1) - 1, H = H)
biomarker_update <- function(pars, model) {
  model["T"] <- matrix(pars[1:9], nrow = 3)
  model["Q"] <- diag(exp(pars[10:12]))
  model
}

T0 <- c(1, 0, 0, 0, 1, 0, 0, 0, 1)
Q0 <- rep(0, 3)
inits <- c(T0, Q0)

biomarker_fit <- fitSSM(biomarker_model, 
                        updatefn = biomarker_update,
                        inits = inits, method = "BFGS")
biomarker_out <- KFS(biomarker_fit$model, filtering = "state", 
                     smoothing = "state")
# 
plt1_dat <-
  tibble(Day = 1:91,
         WBC = biomarker$WBC,
         filtered = biomarker_out$att[, 1],
         filtered_var = biomarker_out$Ptt[1, 1, ])

plt1_dat |>
  ggplot(aes(x = Day)) +
  geom_point(aes(y = WBC), color = "black") +
  geom_line(aes(y = filtered), color = "blue") +
  geom_ribbon(aes(ymin = filtered - 1.96 * sqrt(filtered_var),
                  ymax = filtered + 1.96 * sqrt(filtered_var)), 
              fill = "blue", alpha = 0.2)
# 
# ## PLT
# 
plt2_dat <-
  tibble(Day = 1:91,
         PLT = biomarker$PLT,
         filtered = biomarker_out$att[, 2],
         filtered_var = biomarker_out$Ptt[2, 2, ])

plt2_dat |>
  ggplot(aes(x = Day)) +
  geom_point(aes(y = PLT), color = "black") +
  geom_line(aes(y = filtered), color = "blue") +
  geom_ribbon(aes(ymin = filtered - 1.96 * sqrt(filtered_var),
                  ymax = filtered + 1.96 * sqrt(filtered_var)), 
              fill = "blue", alpha = 0.2)
# 
# ## HCT
# 
plt3_dat <-
  tibble(Day = 1:91,
         HCT = biomarker$HCT,
         filtered = biomarker_out$att[, 3],
         filtered_var = biomarker_out$Ptt[3, 3, ])

plt3_dat |>
  ggplot(aes(x = Day)) +
  geom_point(aes(y = HCT), color = "black") +
  geom_line(aes(y = filtered), color = "blue") +
  geom_ribbon(aes(ymin = filtered - 1.96 * sqrt(filtered_var),
                  ymax = filtered + 1.96 * sqrt(filtered_var)), 
              fill = "blue", alpha = 0.2)
# 
# We may view the fitted transition matrix $\Phi$ as follows:
# 
biomarker_out$model$T
# 
# To interpret this, recall that the bottom row implies that $$
# X_{t,3} = -0.859 X_{t-1,1} + 1.673 X_{t-1, 2} + 0.821 X_{t-1, 3} + W_t,
# $$ so that the `HCT` value depends positively on the previous values of `PLT` and `HCT`, but negatively on the previous value of `WBC`.
# 
# The results are different because : 1) we have different model assumption. ( observation noise = 0) 2) ETS, max likelihood, ...
