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


countrylist <- c("Austria","Belgium","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Lithuania","Luxembourg",
                 "Malta", "Netherlands","Norway","Poland","Portugal","Romania","Slovakia",
                 "Slovenia","Spain","Sweden")
# countrylist <- c("United Kingdom")
## variables to consider
vars <- c("new_cases_per_million", "new_cases")
## loop through countries
for(cou in countrylist){
    ## collect variables of interest
    icu <- owid[owid$location == cou, "icu_patients"]
    X <- as.matrix(owid[owid$location == cou, vars]) ## matrix of external regressors
    dates <- owid[owid$location == cou, "date"]
    ## clean data
    icu[is.na(icu)] <- 0
    ref <- !(icu == 0)
    dates <- droplevels(dates[ref])
    icu <- icu[ref]
    X <- X[ref,]
    X[is.na(X)] <- 0
    
    
    ## fit over rolling period
    m <- arimaRoll(y = icu, cov = X)
}

icuUK <- owid[owid$location == "United Kingdom", "icu_patients"]
newcasesUK <- owid[owid$location == "United Kingdom", "new_cases"]
newcasespermilUK <- owid[owid$location == "United Kingdom", "new_cases_per_million"]
datesUK <- owid[owid$location == "United Kingdom", "date"]
icuUK[is.na(icuUK)] <- 0
ref <- !(icuUK == 0)
datesUK <- droplevels(datesUK[ref])
icuUK <- icuUK[ref]
regrssrs <- matrix(cbind(newcasespermilUK[ref]), ncol = 1)

icuUK <- icuUK[seq(14,length(icuUK))] # to make sure it is ahead of new cases per day
## Data
## calculate time period

arimaRoll <- function(y, cov, l = 10){
    n <- length(y)
    for (i in seq(n/2,n)) {
        ref <- 1:i
        newRef <- (i + 1):(i + l)
        ## fit models
        m <- auto.arima(y[ref], 
                        max.p = 10, 
                        max.q = 10,
                        xreg = cov[ref,])
        ## make forecast
        p <- forecast(m, h = l, xreg = cov[newRef,])
        plot(p)
        lines(x = newRef, y = y[newRef], col = "red")
        m
    }
}

