# ---
# title: "Week 9 Demonstration"
# format: pdf
# editor: visual
# ---
# 
# ## Set up
# 
#| message: false
#| warning: false
#| paged-print: false
library(fpp3)
library(tidyverse)
library(astsa)
library(slider)
library(gridExtra)
# 
# ## 1. Best linear predictor
# 
# Let $X_1,X_2,\ldots,X_n$ be a collection of random variables. For any random variable $Z$, let $Q[Z]$ denote its best linear prediction given $X_1,\ldots,X_n$. Show that $Q$ is a linear operator, i.e. $Q[\alpha X + \beta Y] = \alpha Q[X] + \beta Q[Y]$ for any random variables $X$ and $Y$, and constants $\alpha,\beta$.
# 
# ![](images/note9q1BLP.jpg){width="1080"}
# 
# ------------------------------------------------------------------------
# 
# ## 2. Recursive forecasting
# 
# Fit an AR(2) model on the `gtemp_both` dataset filtered to years prior to 1941. Forecast the mean temperature deviation in 1945 using direct forecasting, and via recursive forecasting, showing that they are the same.
# 
library(astsa)
# Turn into tsibble
gtemp <- gtemp_both |>
  as_tsibble() |>
  rename(Year = index,
         Temp = value)
gtemp |> autoplot()
# 
# Filter time series, only focus on the first part of the data
gtemp_train <- gtemp |>
  filter_index(. ~ "1940")

# Normal fitting process
gtemp_ar2_fit <- gtemp_train |>
  model(ar2 = AR(Temp ~ order(2)))

# Forecast
gtemp_ar2_fit |>
  forecast(h = 5)
# 
gtemp_ar2_coefs <- gtemp_ar2_fit |> tidy() |> pull(estimate)
gtemp_ar2_fit |> tidy()
# 
# Recursive forecasting:
# 
# Create dataset with lagged values
gtemp_ar2 <- gtemp_train |>
  mutate(lag1 = lag(Temp),
         lag2 = lag(lag1))
gtemp_ar2
# 
# \~supervise learning
# 
# Recursive forecasting
for (i in 1:10) {
  year <- 1940 + i
  idx <- nrow(gtemp_ar2)
  lag1 <- gtemp_ar2$Temp[[idx]]
  lag2 <- gtemp_ar2$lag1[[idx]]
  pred <- gtemp_ar2_coefs[[1]] + gtemp_ar2_coefs[[2]] * lag1 + gtemp_ar2_coefs[[3]] * lag2
  gtemp_ar2 <- gtemp_ar2 |> add_row(Year = year, Temp = pred,
                                    lag1 = lag1, lag2 = lag2)
}

gtemp_ar2 |> tail(10)
# 
# Comparing the 1945 forecast with that obtained using `model()`, we see they are indeed the same.
# 
# ## 3. Nonlinear autoregressive models
# 
# Fit a nonlinear AR model using k nearest neighbors on the `globtemp` dataset. Compute its sum of squared residuals and compare it to that of a linear AR model. How about its test error?\
# 
#| message: false
#| warning: false
library(caret)
#caret is a package implemented some machine learning method
# 
# Create a dataset with columns containing lagged values
gtemp_knn <- gtemp_train |>
  mutate(lag1 = lag(Temp),
         lag2 = lag(lag1),
         lag3 = lag(lag2),
         lag4 = lag(lag3)) |>
  filter_index("1854" ~ .)
# Drop the missing values to aviod errors

# Fit k nearest neighbors model
gtemp_knn_fit <- train(Temp ~ lag1 + lag2 + lag3 + lag4, 
                       data = gtemp_knn, method = "knn")
# 
# Training errors:
ar2_resids <- gtemp_ar2_fit |>
  augment() |>
  pull(.resid)
sse <- mean(ar2_resids[3:length(ar2_resids)]**2); sse
# 
# Predict values:
knn_preds <- predict(gtemp_knn_fit, newdata = gtemp_knn)
# True values:
true_obs <- gtemp_knn$Temp

print(paste("AR(2) Training MSE:", 
            sprintf("%.4f", mean(ar2_resids[4:length(ar2_resids)] ** 2))))
print(paste("KNN Training MSE:", 
            sprintf("%.4f", mean((true_obs - knn_preds) ** 2))))
# 
# KNN has a **smaller** training MSE value, comparing to the AR(2) model.
# 
# Let us now perform recursive forecasting for KNN. For next 10 years.
# 
for (i in 1:10) {
  idx <- nrow(gtemp_knn)
  new_data <- tibble(lag1 = gtemp_knn$Temp[[idx]],
                     lag2 = gtemp_knn$lag1[[idx]],
                     lag3 = gtemp_knn$lag2[[idx]],
                     lag4 = gtemp_knn$lag3[[idx]])
  pred <- predict(gtemp_knn_fit, newdata = new_data)
  new_data <- new_data |> cbind(Year = 1940 + i, Temp = pred)
  gtemp_knn <- gtemp_knn |> add_row(new_data)
}
# 
true_obs <- gtemp |>
  filter_index("1941" ~ "1950") |>
  pull(Temp)

ar2_preds <- gtemp_ar2 |> tail(10) |> pull(Temp)
knn_preds <- gtemp_knn |> tail(10) |> pull(Temp)

print(paste("AR(2) Test MSE:", 
            sprintf("%.4f", mean((true_obs - ar2_preds) ** 2))))
print(paste("KNN Test MSE:", 
            sprintf("%.4f", mean((true_obs - knn_preds) ** 2))))
# 
# Test error for the AR(2) model is smaller. KNN may be overfitting.(bigger error)
# 
# ## 4. AR(1)
# 
# Recall the calculations for AR(1) in Section 11.3 of the notes.
# 
# a.  Show directly that the AR(1) forecast is a best linear predictor.
# 
# b.  Show that the estimator we introduced was a method of moments estimator.
# 
# c.  What is the conditional maximum likelihood estimate for AR(1)?
# 
# ------------------------------------------------------------------------
# 
# The forecast equation for AR(1) is
# 
# $$
# \hat X_{n+h|n} = \phi^h X_n
# $$
# 
# For part a), to check that it is the best linear predictor, we just need to show that the gradient conditions hold, i.e. that
# 
# $$
# \nabla_{\beta} \mathbb{E}\lbrace (X_{n+h} - \beta^T X_{n:1})^2\rbrace \big|_{\beta = (\phi^n,0,\ldots,0)} = 0.
# $$
# 
# Now, the (negative) gradient at our desired solution is given by
# 
# $$
# \begin{split}
# \mathbb{E}\lbrace(X_{n+h}-\phi^n X_n)X_{1:n}\rbrace
# \end{split}
# $$
# 
# Since
# 
# $$
# X_{n+h} = \sum_{j=0}^{h-1}\phi^jW_{n+h-j} + \phi^h X_n.
# $$
# 
# Plugging this into the above expression and cancelling $\phi^h X_n$, we see that all coordinates in the gradient are of the form
# 
# $$
# \mathbb{E}\left\lbrace \sum_{j=0}^{h-1}\phi^jW_{n+h-j}X_i\right\rbrace
# $$
# 
# for some $1 \leq i \leq n$. By assumption of causality, the white noise terms with index larger than $n$ are independent of $X_1,\ldots,X_n$, so this expectation is equal to 0.
# 
# The estimator defined in the notes was $\hat\phi = \hat\rho(1)=\hat\gamma(1)/\hat\gamma(0)$. This is exactly equal to $\hat\Gamma_1^{-1}\hat\gamma(1)$.
# 
# To get the conditional maximum likelihood, we write the negative log conditional likelihood as
# 
# $$
# -\log p_{\phi,\sigma^2,\mu}(x_2,x_3,\ldots,x_n|x_1) = \frac{n}{2}\log(2\pi \sigma^2) + \sum_{j=2}^n \frac{((x_j-\mu) - \phi (x_{j-1}-\mu))^2}{2\sigma^2}
# $$
# 
# Rerranging the sum of squares term gives
# 
# $$
# \sum_{j=2}^n (x_j - \phi x_{j-1} - (1-\phi)\mu)^2.
# $$
# 
# Differentiating this with respect to $\mu$, we get
# 
# $$
# -\sum_{j=2}^n (x_j - \phi x_{j-1} - (1-\phi)\mu)(1-\phi).
# $$
# 
# Setting to be equal to $0$, and using the MLE value for $\phi$, $\phi = \hat\phi$, we get
# 
# $$
# \hat\mu = \frac{\bar x_{2:n} - \hat\phi\bar x_{1:n-1}}{1-\hat\phi}.
# $$
# 
# Next, differentiating with respect to $\phi$, we get
# 
# $$
# \sum_{j=2}^n (x_j - \bar x_{2:n}) - \phi (x_{j-1} -\bar x_{1:n-1}))(x_{j-1} - \bar x_{1:n-1}).
# $$
# 
# Setting this to be equal to 0 gives
# 
# $$
# \hat\phi = \frac{\sum_{j=2}^n (x_j - \bar x_{2:n})(x_{j-1} - \bar x_{1:n-1})}{\sum_{j=2}^n (x_j - \bar x_{2:n})^2}
# $$
# 
# ## 5. AR(2)
# 
# Consider the AR(2) equation $$
# X_t = 1.5 X_{t-1} - 0.75 X_{t-2} + W_t.
# $$
# 
# a.  What is the autoregressive polynomial?
# 
# b.  What are its roots?
# 
# c.  How can we use the roots to compute the period of its ACF? (optional)
# 
# ------------------------------------------------------------------------
# 
# The AR polynomial is
# 
# $$
# \phi(z) = 1 - 1.5z +0.75z^2
# $$
# 
# The roots are
# 
# $$
# \frac{1.5 \pm \sqrt{(1.5)^2 - 4\cdot 0.75}}{2\cdot 0.75} = 1 \pm i/\sqrt{3}.
# $$
# 
# Since the roots are complex, the ACF is a damped sinusoid.
# 
# The general form is
# 
# $$
# cz_1^h + \bar c \bar z_1^h = a|z_1|^{-h}\cos(h\theta+b)
# $$
# 
# where $a$ and $b$ are constants depend on initial conditions, and
# 
# $$
# \begin{split}
# \theta & = \arctan(Im(z_1)/Re(z_1)) \\
# & = \arctan(1/\sqrt{3}) \\
# & = \pi/6.
# \end{split}
# $$
# 
# Hence, the period of the sinusoid is 12 units.
