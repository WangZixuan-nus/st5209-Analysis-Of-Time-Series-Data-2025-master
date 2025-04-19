# ---
# title: "Week 10 Demonstration"
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
# 
# # **12  ARMA Models**
# 
# # PRE recorded video
# 
# ### **12.1.3 Similarities with AR(p) models**
# 
#  sample trajectories of AR(p) and MA(q) models
# 
# $$
# \text{MA(1)model:}X_t=W_t+0.9W_{t−1}\\\text{AR(1)model:}Y_t=0.5Y_{t−1}+W_t
# $$
# 
set.seed(5209)
plot_data <-
    tibble(t = 1:80,
           wn = rnorm(80),
           #MA(1):Xt=Wt+0.9W{t−1}​
           X = arima.sim(model = list(ma = 0.9),
                         n = 80, innov = wn),
           #AR(1):Y_t=0.5Y_{t−1}+W_t
           Y = arima.sim(model = list(ar = 0.5), 
                         n = 80, innov = wn)
    ) |>
    as_tsibble(index = t)
plt1 <- plot_data |>
    autoplot(X) + ylab("X") + xlab("MA(1)")
plt2 <- plot_data |>
    autoplot(Y) + ylab("Y") + xlab("AR(1)")
grid.arrange(plt1, plt2, nrow = 2)
# 
# Diagnosing order using ACF/PACF
# 
# $$
# \text{AR(2):}(I−0.9B)^2Xt=W_t,\\\text{MA(2)}:Yt=(I−0.9B)^2W_t.
# $$
# 
#AR(2)
set.seed(5209)
n <- 200
plot_data <-
    tibble(t = 1:n,
           X = arima.sim(model = list(ar = c(1.8, -0.81)), 
                         n = n)
    ) |>
    as_tsibble(index = t)
plt1 <- plot_data |> ACF(X) |>
    autoplot()
plt2 <- plot_data |> PACF(X) |>
    autoplot()
grid.arrange(plt1, plt2, nrow = 2)

# 
#MA(2):Yt=(I−0.9B)^2W_t
set.seed(5209)
n <- 200
plot_data <-
    tibble(t = 1:n,
           Y = arima.sim(model = list(ma = c(-1.8, 0.81)),
                         n = n)
    ) |>
    as_tsibble(index = t)
plt3 <- plot_data |> ACF(Y) |>
    autoplot()
plt4 <- plot_data |> PACF(Y) |>
    autoplot()
grid.arrange(plt3, plt4, nrow = 2)
# 
library(gridExtra)
library(grid)

grid.arrange(
  plt1, plt2, plt3, plt4, 
  nrow = 2,
  top = textGrob(
    "plots for an AR(2) model (top) and MA(2) model (bottom).", 
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

# 
# The plots for the AR2 model are on the top. Note that it has a slow decay for its ACF and a cut off for its PACF.
# 
# The plots for the MA2 model are on the bottom. It has a cut off for its ACF and a slow decay for its Pacf.
# 
# # LECTURE:
# 
# ## 1. Pelt dataset
# 
# Consider the `pelt` dataset. We have previously seen that the Lynx time series seems to have cyclic patterns with approximately 10 year cycles. in this question, we explore how ARMA can be used to provide better forecasts for this time series.
# 
pelt |>
  autoplot(Lynx)
# 
# a.  Compute an ACF plot for the time series and compare it with the lag plot we did earlier.
# 
#     ```{r}
#     pelt |>
#       gg_lag(Lynx, geom = "point", lags = 1:16)
# 
#     pelt |>
#       ACF(Lynx) |> autoplot()
#     ```
# 
#     The time seiries have cycles. lag10: strong positive correlation.
# 
#     ACF tends to summarize the lag plot. Period of cycles match the period of ACF plot.
# 
#     MA model and AR model, have some cutoff patterns, but the plot does not show that pattern. (If the time series have cycles, it may not the MA(q) model)
# 
#     Since the ACF plot does not have a cut-off, this is not an MA model.
# 
# b.  Inspect the ACF and PACF plots for the time series. Guess what ARMA model might be appropriate for the time series.
# 
#     ```{r}
#     pelt |>
#       PACF(Lynx) |>
#       autoplot()
# 
#     # Alternatively
#     # pelt |> ACF(type = "partial") |> autoplot()
#     ```
# 
#     Lag 1, lag 2: spike. after lag 2, cutoff –\> AR(2) model
# 
#     The PACF seems to have a cut-off at lag 2, giving some evidence that AR(2) may be appropriate. However, there still seems to be some remaining sinusoidal pattern at larger lags, so we could also try AR(3) or ARMA(2, 1).
# 
# c.  Fit an ARMA model, allowing for automatic model selection. What is the order of the model selected?
# 
#     ```{r}
#     # ARMA
#     lynx_fit <- pelt |>
#       model(arma = ARIMA(Lynx ~ pdq(d = 0)))
#     lynx_fit
#     ```
# 
#     \<ARIMA(2,0,1) Select p = 2, q = 1.
# 
#     We obtain an ARMA(2, 1) model with a constant term.
# 
# d.  What are the model parameters?
# 
#     ```{r}
#     lynx_fit |> tidy()
#     ```
# 
#     $$X_t = 10165.36+1.49X_{t-1}-0.85X_{t-2}+W_t-0.34W_{t-1}$$
# 
# e.  What are the roots of the AR polynomial? Does this agree with the periodicity you observe?
# 
#     ```{r}
#     lynx_fit |> gg_tsresiduals()
#     ```
# 
#     the residual plot shows that the model fits well. $$
#     \phi(z) = 1 - \phi_1 z-\phi_2 z^2\\
#     =1-1.49z+0.85z^2
#     $$
# 
#     To calculate the roots, we use the function `polyroot()` :
# 
#     ```{r}
#     # write the coefficients
#     polyroot(c(1, -1.4851139, 0.8468299))
#     ```
# 
#     Writing the first root as $z = re^{i\theta}$, it has absolute value
# 
#     $$ r = \sqrt{0.8768667^2 + 0.6418563^2} \approx 1.0867 > 1 $$
# 
#     Furthermore, the argument $\theta$ (angle) is given by
# 
#     $$ \theta = \arctan(0.6418563 / 0.8768667) \approx 0.63187 $$
# 
#     ```{r}
#     theta <- atan(0.6418563 / 0.8768667)
#     theta
#     ```
# 
#     General formula of the ACF:
# 
#     $$
#     \rho_X(h) = c r^{-h}\cos(h\theta+c)
#     $$
# 
#     c is a constant
# 
#     The period of the time series is then $2\pi/\theta = 9.944 \approx 10$, which agrees with the ACF and lag plots.
# 
#     ```{r}
#     period <- 2 * pi / theta; period
#     ```
# 
# f.  Plot the forecast for the model.
# 
#     ```{r}
#     # ARMA model forecast
#     lynx_fc <- lynx_fit |> forecast(h = 50)
#     plt1 <- lynx_fc |> autoplot(pelt); plt1
#     ```
# 
# g.  Fit a Holt-Winters method with seasonal period = 10.
# 
#     ```{r}
#     ets_fit <- pelt |>
#       # seasonal component, period = 10
#       model(ets = ETS(Lynx ~ season("A", period = 10))) 
#     plt2 <- ets_fit |>
#       forecast(h = 50) |>
#       autoplot(pelt);plt2
#     ```
# 
#     ```{r}
#     grid.arrange(plt1, plt2)
#     ```
# 
# h.  What are the qualitative differences between the two forecasts curves?
# 
#     The ARMA model's forecast tends towards the **mean** of the time series. Its **forecast interval width converges to a constant**. It is good to forecast for a while. For longer time horizons, we know less of the prediction. Because there is always noise to generation of the X_t. we lose all the information.
# 
#     On the other hand, the ETS model's forecast maintains constant seasonality. Its **forecast interval width grows** over time.
# 
# i.   Compare the accuracy of the two methods.
# 
#     ```{r}
#     # time series cross validation
#     pelt |>
#       stretch_tsibble(.init = 20, .step = 10) |>
#       model(arma = ARIMA(Lynx ~ pdq(d = 0)),
#             ets = ETS(Lynx ~ season("A", period = 10))) |>
#       forecast(h = 10) |>
#       accuracy(pelt) |>
#       select(.model, MASE, RMSSE)
#     ```
# 
# ARMA has smaller MASE and RMSSE. So ARMA is better model. \
# plot the forecast for the last= 25
# 
# true data " ~1925
pelt |>
  filter_index(. ~ "1910") |>
  model(arma = ARIMA(Lynx ~ pdq(d = 0)),
        ets = ETS(Lynx ~ season("A", period = 10))) |>
  forecast(h = 25) |>
  autoplot(pelt, level = NULL)
# 
# ARMA allows model cycle. While ETS seems to better forecast the overall "shape"(seasonality) of the time series, its forecast can be completely out of phase with the true time series, leading to huge errors.
# 
# Sometimes the ETS fits bad, sometimes fits well. There are huge errors. There is no fix cycles pattern.
# 
# ## 2. Time series decomposition and ARMA
# 
# Load the diabetes dataset as follows:
# 
diabetes <- read_rds("../_data/cleaned/diabetes.rds")
diabetes |> autoplot()
# 
# a.  Form a time series decomposition forecasting model using SES to model the seasonally adjusted time series and the seasonal naive method to model the seasonal component. Perform a residual analysis.
# 
#     ```{r}
#     diabetes |>
#       model(no_arma = decomposition_model(STL(log(TotalC)),
#                                 # seasonal component
#                                 SNAIVE(season_year),
#                                  # seasonally adjusted
#                                 TSLM(season_adjust ~ trend()))) |>
#       gg_tsresiduals()
#     ```
# 
#     ACF plot exceed the dash line.
# 
# b.  Form a time series decomposition forecasting model using SES to model the trend, the seasonal naive method to model the seasonal component, and an ARMA model to model the remainder component. Perform a residual analysis.
# 
#     ```{r}
#     diabetes |>
#       model(decomposition_model(STL(log(TotalC)),
#                                 ETS(season_adjust ~ season("N")),
#                                 SNAIVE(season_year))) |>
#       gg_tsresiduals()
#     ```
# 
# c.  Does applying the ARMA model to the remainder component improve the fit?
# 
#     ```{r}
#     diabetes |>
#       model(decomposition_model(STL(log(TotalC)),
#                                 ETS(trend ~ season("N")),
#                                 SNAIVE(season_year),
#                                 ARIMA(remainder ~ pdq(d = 0) + PDQ(0, 0, 0)))) |>
#       gg_tsresiduals()
# 
#     ```
# 
#     It seems that even if we model the remainder component with ARMA, we still do not get a very good fit. Takeaway: This is not a magic bullet, sometimes it doesn't work.
# 
# ## 3. PACF
# 
# a.  Use `ARMAacf()` to compute the PACF for the AR model $X_t = 0.5X_{t-1} + 0.1X_{t-2} + W_t$.
# 
#     ```{r}
#     ARMAacf(ar = c(0.5, 0.1), lag.max = 10, pacf = TRUE)
#     ```
# 
#     for any lag \> 2, PACF = 0
# 
#     ```{r}
#     pacf_vals <- ARMAacf(ar = c(0.5, 0.1), lag.max = 10, pacf = TRUE)
#     plot(1:10, pacf_vals, type = "h", main = "PACF of AR(2)", xlab = "Lag", ylab = "PACF")
#     abline(h = 0, col = "red")
#     ```
# 
# b.  Explain why PACF: $\alpha(2) = 0.1$ but $\alpha(1) \neq 0.5$.
# 
# c.  Show that the BLP of $X_t$ given $X_2,\ldots,X_{t-1}$ satisfies $\text{Cov}(X_t - \hat X_t, X_k) = 0$ for $k=2,\ldots,t-1$.
# 
# d.  (optional) Using this orthogonality condition, understand the geometric interpretation of partial correlation [here](https://en.wikipedia.org/wiki/Partial_correlation#Geometrical).
# 
# e.  (optional) Prove the regression coefficient interpretation of the PACF, i.e. equation (12.4) in the notes.
# 
#     We have learnt that that $\alpha(2)$ is equal to the last AR coefficient, since $p=2$. However, $\alpha(1) = \rho(1) \neq 0.5$. It is the correlation between $X_t$ and $X_{t-1}$.
# 
#     Let $\hat X_t$ denote the BLP of $X_t$ given $X_2,\ldots,X_{t-1}$. We can write $\hat X_t = X_{2:t-1}^T\hat\beta$. We make use of the orthogonality condition of BLP to see that
# 
#     $$ \nabla_{\beta} \mathbb{E}[(X_t - X_{2:t-1}^T\beta)^2]|_{\beta = \hat\beta} =0. $$
# 
#     Expanding our the left hand side, we get
# 
#     $$ \mathbb{E}[(X_t - X_{2:t-1}^T\hat\beta)X_{2:t-1}^T] = 0 $$
# 
#     This equation is equivalent to saying that $\text{Cov}(X_t - \hat X_t, X_k) = 0$ for $k=2,\ldots,t-1$.
# 
#     To prove the regression coefficient interpretation, first recall that we need to prove:
# 
#     $$ \beta_X = \rho_{X,Y|Z}\cdot \sqrt{\frac{\text{Var}\lbrace Y - \hat Y\rbrace}{\text{Var} \lbrace X - \hat X\rbrace}}. $$
# 
#     where $\beta_X$ is the coefficient of $X$ in the regression of $Y$ on $(X,Z)$ jointly.
# 
#     For simplicity, assume that $Z$ is 1-dimensional, and then $X, Y, Z$ all have mean zero. Then $\hat X = \alpha Z$ and $\hat Y = \gamma Z$ and the regression coefficients of $Y$ given $X, Z$ are the unique values $\beta_X$ and $\beta_Z$ satisfying
# 
#     $$ \mathbb{E}[(Y - \beta_X X - \beta_Z Z)X] = 0, \quad \mathbb{E}[(Y - \beta_X X - \beta_Z Z)Z] = 0 $$
# 
#     Following the hint after (12.14) in the notes, we guess that $\tilde Y =\hat Y + \rho_{X,Y|Z}\cdot \sqrt{\frac{\text{Var}\lbrace Y - \hat Y\rbrace}{\text{Var} \lbrace X - \hat X\rbrace}}(X - \hat X)$ is the BLP. To check this, we observe
# 
#     $$ \mathbb{E}[(Y-\tilde Y)Z] = \mathbb{E}[(Y-\hat Y)Z] + \rho_{X,Y|Z}\cdot \sqrt{\frac{\text{Var}\lbrace Y - \hat Y\rbrace}{\text{Var} \lbrace X - \hat X\rbrace}}\mathbb{E}[(X - \hat X)Z] = 0 $$
# 
#     $$ \begin{split} \mathbb{E}[(Y-\tilde Y)X] & = \mathbb{E}[(Y-\tilde Y)(X - \hat X)]\\ & = \mathbb{E}\left[\left((Y-\hat Y) - \rho_{X,Y|Z}\cdot \sqrt{\frac{\text{Var}\lbrace Y - \hat Y\rbrace}{\text{Var} \lbrace X - \hat X\rbrace}}(X-\hat X)\right)(X - \hat X)\right] \\ & = \mathbb{E}\left[\left((Y-\hat Y) - \frac{\text{Cov}\lbrace Y - \hat Y,X- \hat X\rbrace}{\text{Var} \lbrace X - \hat X\rbrace}(X-\hat X)\right)(X - \hat X)\right] \\ & = 0 \end{split} $$
# 
#     where the 2nd equality follows from the definition of $\rho_{X,Y|Z}$ and the last equality follows from the definition of a 1D regression coefficient.
# 
#     Finally, note that $\tilde Y$ is of the form $\tilde Y = \beta_X X + \beta_Z Z$, where
# 
#     $$ \rho_{X,Y|Z}\cdot \sqrt{\frac{\text{Var}\lbrace Y - \hat Y\rbrace}{\text{Var} \lbrace X - \hat X\rbrace}}. $$
# 
#     as we wanted.
# 
# ## 4. AIC
# 
# a.  What goes wrong when we choose $p$ and $q$ to be too big?
# 
#     When $p$ and $q$ are too small, then the model will not be a good fit to the data, parameter estimates will be biased, and prediction accuracy will be suboptimal.
# 
#     When $p$ and $q$ are too big, then the variance in the parameter estimates will be too big (see example in lecture video, or in Shumway and Stoffer, 4th Ed, Example 3.35).
# 
#     We just want p and q big enough to fit the model well and not overfit the model.
# 
# b.  (optional) What is the expectation of the log likelihood?
# 
# c.  (optional) Given an ARMA($p_0$,$q_0$) model, what is the expectation of AIC for the choice $p \geq p_0$ and $q \geq q_0$?
# 
# d.  (optional) Given an ARMA($p_0$,$q_0$) model, what is the expectation of AIC for the choice $p < p_0$ or $q < q_0$?
# 
#     For (b) - (c), refer to Appendix B in lecture notes.
# 
#     When $p < p_0$ or $q < q_0$, then this is not covered by Appendix B. In this case, the expectation is dominated by the bias incurred.
# 
# e.  What does this imply about using AIC for model selection?
# 
# As such, the expected AIC will be minimized at the correct model (choice of $p$ and $q$). It makes sense to use AIC for model selection.
