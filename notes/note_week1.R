# ---
# title: "Week 1 note"
# author: "Wang Zixuan"
# format: pdf
# editor: visual
# ---
# 
# # **2  Data Wrangling**
# 
# # **3  Visualizations**
# 
# # pre-reading
# 
knitr::opts_chunk$set(echo = TRUE)
# 
#| message: FALSE 
library(fpp3) 
library(tidyverse)
# help(package="fpp3")
# 
# ## **Tsibbles**
# 
example1 <- tsibble(
  year = 2015:2019, 
  y = c(123, 39, 78, 52, 110),   
  index=year 
) 
example1 
str(example1)  
global_economy 
key(global_economy) 
# 
# Creating Tsibbles
# 
tbl <- tibble(
  qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30)
)
as_tsibble(tbl, key = group, index = qtr)

# 
# ## Visualizations
# 
#Time plots
aus_airpassengers
#It records the number of air passengers in millions on Australian air 
#carriers between 1970 and 2016.
aus_airpassengers |>
  autoplot(.vars = Passengers) +
  geom_point() +
  labs(y = "Passengers (millions)",
       x = "Year")
# It is arguable that the trend appears piecewise linear, with a more
#positive gradient after 1990.

# 
# **table contains multiple time series**
# 
# the number of lines is the number of unique keys.
# 
aus_arrivals |> autoplot(.vars = Arrivals)
# 
# overploting
# 
# So one is advised to first filter for only the most relevant keys before plotting
# 
aus_arrivals|>   filter(Origin=="Japan") |>   autoplot(.vars=Arrivals)
# 
# #### Seasonality terminology
# 
# seasonal plot : A seasonal plot is similar to a time plot, except that we chop up thee e time series into period lengths.
# 
plt1 <- aus_arrivals |>
  filter(Origin == "Japan", Quarter <= yearquarter("1995 Q4")) |>
  gg_season(y = Arrivals, labels = "right") +
  lims(y=c(9000, 230000))
plt1
# 
plt2 <- aus_arrivals |>
  filter(Origin == "Japan", Quarter > yearquarter("1995 Q4")) |>
  gg_season(y = Arrivals, labels = "right") +
  lims(y=c(9000, 230000))
plt2
# 
library(gridExtra)
grid.arrange(plt1, plt2, nrow = 2)
# 
# Many time series have seasonalities of multiple periods. We can choose what period to use for `gg_season()` by setting the `period` argument, e.g. to “day”, “week”, or “year”.
# 
# Patterns repeat on a daily bases
# 
#For instance, consider the dataset vic_elec, which measures the half-hourly electricity demand for the state of Victoria in Australia. 
#vic_elec
vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victor.ia")
# 
# Week:
# 
# We see that the time series also displays weekly seasonality, thais, patterns that repeat on a weekly basis.
# 
vic_elec |> gg_season(Demand, period = "week") +
theme(legend.position="none") +
labs(y="MWh", title="Electricity demand: Victoria")
# 
# Year
# 
vic_elec |> gg_season(Demand, period = "year") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")
# 
# ## **3.3 Seasonal subseries plots**
# 
aus_arrivals |> filter(Origin == "Japan") %>% 
  gg_subseries(y = Arrivals) 
# 
# # Lecture
# 
# ## Set up
# 
# We always load the following packages.
# 
#| message: FALSE
library(fpp3)
library(tidyverse)
# 
# ## 1. Downloading and wrangling data
# 
# Download this [dataset](https://www.kaggle.com/datasets/robikscube/hourly-energy-consumption) on energy consumption from Kaggle. Save it into the raw data folder. Load the dataset and try to turn it into a tsibble `elec_consumption`. What error do you get? Try to deal with it.
# 
elec_consumption <- read_csv("../_data/raw/AEP_hourly.csv")
# 
# Turn it into a tsibble. And we recieve an error.
# 
#| eval: FALSE
elec_consumption |> as_tsibble()
#we cant turn it into tsibble using this, since we dont distinct rows identified by key and index.
#We need a time index(unique for every time point)
# 
# Seems like there may be multiple entries with the same datetime. Let us check:
# 
# We can get 8 duplicated rows:
# 
duplicates(elec_consumption)
# 
# The duplicate entries all occur in early November at 2am. This must be because of **daylight savings( on first Sunday of November)**.
# 
# How to deal with the duplicates? Probably the most "correct" way would be to "undo" daylight savings, but there is no built-in functionality for this, so we will just perform a stop-gap and take the **mean** of all measurements with the same datetime.
# 
elec_consumption <- elec_consumption |> 
  group_by(Datetime) |>
  summarise(AEP_MW = mean(AEP_MW)) |>
  as_tsibble(index = Datetime)
#head(elec_consumption)
# 
# ## 2. Time plots
# 
# Make a time plot of `elec_consumption`. You should observe [overplotting](https://www.displayr.com/what-is-overplotting/). In other words, it is hard to see what is going on because too many details are plotted. What can you do to extract more meaningful information? Try to implement your idea.
# 
# Let us now compute a time plot:
# 
elec_consumption |> autoplot()
# 
# reduce some data - shorten to 2 years... use **filter_index**
# 
# There is clearly too much going on here. What can we do to visualize the time series better?
# 
# 1\. Filter the time series so that it is over a shorter time period.
# 
# 2\. Decompose the time series into seasonal, trend, and remainder components (next lesson)
# 
# 3\. Make a seasonal plot.
# 
# 4\. Aggregate the time series so that the duration between measurements is larger (e.g. days instead of hours).
# 
# Let's try to perform a filter
# 
elec_consumption |> 
  filter(Datetime < 2007)
# 
# What went wrong? Turns out inequality operations don't work naturally with datetime objects. We first need to extract the year number using the **`year()`** function.
# 
# the filter is comparing numbers to numbers, but "Datetime" contains years, months and days, not only the year. So we need to **extract the year**
# 
elec_consumption |> 
  filter(year(Datetime) < 2007)
# 
# There's actually a function from `tsibble` called **`filter_index()`** that provides more convenient ways of filtering based on the time index.
# 
?filter_index
# 
elec_consumption |> 
  filter_index(~"2007-01-01")|> 
  autoplot()
# 
# This plot is still quite noisy, so let's make seasonal plots.
# 
# ## 3. Seasonal plots
# 
# Make a seasonal plot of `elec_consumption`. You should experience an error. How can you fix it? What period did **`gg_season()`** automatically select? Try changing the period. Interpret the patterns observed in all your plots.
# 
#| eval: FALSE
elec_consumption |> 
  gg_season()
# 
# Oops, there's an error again. Let's inspect the error message.
# 
#| eval: FALSE
Plot variable not specified, automatically selected `y = AEP_MW`Error in `check_gaps()`:
! data contains implicit gaps in time. You should check your data and convert implicit gaps into explicit missing values using `tsibble::fill_gaps()` if required.
Backtrace:
 1. feasts::gg_season(elec_consumption)
 2. feasts:::check_gaps(data)
# 
# It says that there are gaps in the time index. This is unsurprising, because of daylight savings.
# 
# Missing data: two types, **explicit missing data** and **implicit missing data**
# 
# There is no data while there should be the data in the time intervels.
# 
# It says that there are gaps in the time index. This is unsurprising, because of daylight savings. So we use **fill_gaps().**
# 
#?fill_gaps
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2014),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest
harvest |> fill_gaps()
# 
fill_gaps(harvest, .full = TRUE)
# 
elec_consumption |> 
  fill_gaps() |> 
  gg_season()
# 
elec_consumption |> 
  filter_index(~"2007-01-01") |> 
  fill_gaps() |> 
  gg_season()
# 
# What **period** did `gg_season()` automatically select? It selected **a period of a year** (i.e. 365 \* 24).
# 
# every year, the season patterns are almost the same. It does have yearly seasonality.
# 
# Two peak energy using:
# 
# In August, people use more air-conditioners. peak summer
# 
# peak winter: use heater.
# 
# Manually setting the period to a year gives the same plot. We can try to interpret this seasonality. The highest energy consumption occurs in January and August, corresponding to the middle of Winter and Summer respectively. This is when people make the most of heaters or air-conditioning.
# 
elec_consumption |> 
  filter_index(~"2007-01-01") |> 
  fill_gaps() |> 
  gg_season(period = "month")
# 
elec_consumption |> 
  filter_index(~"2007-01-01") |> 
  fill_gaps() |> 
  gg_season(period = "week")
# 
# There seems to be weekly seasonality too. Energy consumption is lower on the weekends than on weekdays. Note that the fluctuations over each day is part of the daily seasonality, not the weekly seasonality.
# 
elec_consumption |> 
  filter_index("2006-01-01"~"2007-01-01") |> 
  fill_gaps() |> 
  gg_season(period = "day")
# 
# red: the start of the year, pink: the end the the year
# 
# There seems to be daily seasonality. Energy consumption decreases after 9pm, bottoming out around 5am, when people are sleeping. It then increases again during the day. Note also the different patterns for Summer months (blue) and Winter months (pink/orange).
# 
# ## 4. Lag plots
# 
# Make a lag plot of `elec_consumption`. What issues arise? How can you make the plot look like what was shown in the lecture video?
# 
elec_consumption |> gg_lag()
# 
# We also experience some overplotting. We can reduce overplotting by filtering and adding transparency (alpha).
# 
elec_consumption |> 
  filter(year(Datetime) == 2007) |>
  gg_lag(geom = "point", alpha = 0.2)
# 
# y value: x_t
# 
# x value : x_t-1
# 
# There is strong corralation at lag1, but corralation become small when time lag grow bigger.
# 
# ## 5. Finding time series data
# 
# Find some other interesting time series datasets on the internet and experiment with them. Are trend, seasonality, and cycles the only patterns you can identify?
