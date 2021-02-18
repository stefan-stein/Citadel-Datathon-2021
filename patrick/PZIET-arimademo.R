## 1. Clear environment, and read in data
## clear environment
rm(list = ls())
## owid
owid <- read.csv("1_owid/owid-covid-data.csv")
## ecdc
agerangenotificationseu <- read.csv("2_ecdc/agerangenotificationeu.csv")
country_response_measures <- read.csv("2_ecdc/country_response_measures.csv")
admissionrates <- read.csv("2_ecdc/admissionrates.csv")
dailynotificationeu <- read.csv("2_ecdc/dailynotificationeu.csv")
notification <- read.csv("2_ecdc/notification.csv")
testing <- read.csv("2_ecdc/testing.csv")
weeklynotificationeu <- read.csv("2_ecdc/weeklynotificationeu.csv")
## covid tracking
alabama_history <- read.csv("3_covidtracking/alabama-history.csv")
national_history <- read.csv("3_covidtracking/national-history.csv")
## load libraries
library("forecast")
library("tseries")
library("fGarch")
library("rugarch")

icuUK <- owid[owid$location == "United Kingdom", "icu_patients"]
datesUK <- owid[owid$location == "United Kingdom", "date"]
icuUK[is.na(icuUK)] <- 0
ref <- !(icuUK == 0)
datesUK <- droplevels(datesUK[ref])
icuUK <- icuUK[ref]

## Data
## calculate time period
n <- length(icuUK)
l <- 14
for (i in seq(n/2,n)) {
  ref <- 1:i
  ## fit model
  m <- auto.arima(icuUK[ref], 
                  max.p = 10, 
                  max.q = 10)
  ## make forecast
  p <- forecast(m, l)
  plot(p)
  newRef <- (i + 1):(i + l)
  lines(x = newRef, y = icuUK[newRef], col = "red")
}
