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


# countrylist <- c("Austria","Belgium","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France",
#                  "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Lithuania","Luxembourg",
#                  "Malta", "Netherlands","Norway","Poland","Portugal","Romania","Slovakia",
#                  "Slovenia","Spain","Sweden")
countrylist <- c("Austria","Belgium","Croatia")

# countrylist <- c("United Kingdom")
## variables to consider
lagInfo <- read.csv("lag_data.csv")

arimaRoll <- function(y, X, h, lags, dts){
    ## function to make rolling arima predictions
    ## y is the variable we are interested in modelling
    ## X is a matrix containing the external regressors
    ## h is the length of the forecast ahead we want to make (i.e. \hat{y}_{t+h})
    ## lags relevant lags
    ## dts is a vector containing the dates
    
    ## adjust external regressors for lags
    lags <- abs(lags) # make sure all are positive 
    maxlag <- max(lags)
    minlag <- min(lags)
    ynew <- y[seq(1+maxlag, length(y))]
    Xnew <- matrix(0, nrow = length(ynew) + min(h, lags), ncol = ncol(X))
    dtsnew <- as.Date(dts[seq(1 + maxlag, length(dts))])
    yr <- format(dtsnew[1], "%Y")
    mth <- format(dtsnew[1], "%m")
    dy <- format(dtsnew[1], "%d")
    
    for(i in seq(ncol(X))){
        ref <- seq(1 + maxlag - lags[i], length(y) + min(h, minlag) - lags[i])
        Xnew[, i] <- X[ref, i]
    }
    n <- length(ynew)
    for (i in seq(n/2,n)) {
        ## determine references w.r.t. lags
        ref <- seq(i) # reference for training
        frcstref <- seq(i + 1, i + h) # reference for forecast
        ## fit models
        yfit <- ts(ynew[ref], frequency = 365, 
                   start = c(as.numeric(yr), as.numeric(mth)))
        m <- auto.arima(ynew[ref], 
                        max.p = 10, 
                        max.q = 10,
                        xreg = Xnew[ref,])
        ## make forecast
        if(i + h > n + minlag){
            frcstref <- seq(i + 1, i + minlag) # reference for forecast
            p <- forecast(m, h = n + minlag - i, xreg = Xnew[frcstref,])
            yy <- seq(from = 1, to = i + minlag, by = 2*h)
            png(paste("arimaplots/",cou,"_",dtsnew[i+minlag], ".png", sep = ""))
            plot(p, showgap = FALSE, xaxt = "n",
                 ylab = "ICU occupancy", main = paste(cou,": ", p$method, sep = ""))
            axis(side = 1, labels = dtsnew[yy], at = yy, cex.lab = 0.75)
            lines(x = frcstref, y = ynew[frcstref], col = "red")
            dev.off()
            next
        }
        p <- forecast(m, h = h, xreg = Xnew[frcstref,])
        yy <- seq(from = 1, to = i + h, by = 2*h)
        png(paste("arimaplots/",cou,"_", dtsnew[i+h], ".png",sep = ""))
        plot(p, showgap = FALSE, xaxt = "n", 
             ylab = "ICU occupancy", main = paste(cou,": ", p$method, sep = ""))
        axis(side = 1, labels = dtsnew[yy], at = yy, cex.axis = 0.75)
        lines(x = frcstref, y = ynew[frcstref], col = "red")
        dev.off()
    }
    invisible(m)
}

## loop through countries
for(cou in countrylist){
    ## collect variables of interest
    icu <- owid[owid$location == cou, "icu_patients"]
    if(sum(!is.na(icu)) < 100){
        cat("Not sufficient ICU data for ",cou,"\n",sep = "")
        next  
    } 
    ## get variable names
    vars <- as.vector(lagInfo[lagInfo$location == cou, "covariate"])
    ## make lag vector
    lvec <- abs(lagInfo[lagInfo$location == cou, "lag"])
    X <- as.matrix(owid[owid$location == cou, vars]) ## matrix of external regressors
    if(any(lvec == 1000)) X <- X[,which(lvec != 1000)]
    dates <- owid[owid$location == cou, "date"]
    ## clean data
    icu[is.na(icu)] <- 0
    ref <- !(icu == 0)
    icu <- icu[ref]
    X <- X[ref,]
    X[is.na(X)] <- 0
    ## fit over rolling period
    m <- arimaRoll(y = icu, X = X, h = 10, lags = lvec, dts = dates) 
    cat(cou," complete \n",sep = "")
}



