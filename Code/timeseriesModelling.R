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
                 "Germany", "Hungary","Iceland","Ireland","Italy","Latvia","Lithuania","Luxembourg",
                 "Malta", "Netherlands","Norway","Poland","Portugal","Romania","Slovakia",
                 "Slovenia","Spain","Sweden")


## Display intial time series
icu <- owid[owid$location == "Austria", "icu_patients"]
icu[is.na(icu)] <- 0
ref <- !(icu == 0)
icu <- icu[ref]
dates <- owid[owid$location == "Austria", "date"]
dtsnew <- as.Date(dates[ref])
yr <- format(dtsnew[1], "%Y")
mth <- format(dtsnew[1], "%m")
dy <- format(dtsnew[1], "%d")
icu <- ts(icu, frequency = 365, start = c(as.numeric(yr), as.numeric(mth)))


{
    plot(icu, x = seq(length(dtsnew)), xaxt = "n", type = "l",
         ylab = "ICU occupancy", xlab = "Date", main = "Austria")
    yy <- seq(from = 1, to = length(dtsnew), by = 15)
    axis(side = 1, labels = dtsnew[yy], at = yy, cex.lab = 0.75)
}


{
    plot(diff(icu), x = seq(length(dtsnew) - 1), xaxt = "n", type = "l",
         ylab = "Differenced ICU occupancy", xlab = "Date", main = "Austria")
    yy <- seq(from = 2, to = length(dtsnew), by = 15)
    axis(side = 1, labels = dtsnew[yy], at = yy, cex.lab = 0.75)
}

{
    plot(diff(diff(icu)), xaxt = "n", ylab = "Twice-Differenced ICU occupancy", main = "Austria")
    yy <- seq(from = 1, to = length(dtsnew), by = 15)
    axis(side = 1, labels = dtsnew[yy], at = yy, cex.lab = 0.75)
}


## find what data is available for countries
for(cou in c("Ireland", "Luxembourg")){
    icu <- owid[owid$location == cou, "icu_patients"]
    hosp <- owid[owid$location == cou, "hosp_patients"]
    cat(cou,"icu number: ", sum(!is.na(icu)),"\n")
    cat(cou,"Hospital number: ", sum(!is.na(hosp)),"\n")
    if(sum(!is.na(icu)) < 100){
        cat("Not sufficient ICU data for ",cou,"\n",sep = "")
        next
    } 
    if(sum(!is.na(hosp)) < 100){
        cat("Not sufficient hospital data for ",cou,"\n",sep = "")
        next  
    }
    icu[is.na(icu)] <- 0
    ref <- !(icu == 0)
    icu <- icu[ref]
    hosp <- hosp[ref]
    dates <- owid[owid$location == cou, "date"]
    dtsnew <- as.Date(dates[ref])
    yr <- format(dtsnew[1], "%Y")
    mth <- format(dtsnew[1], "%m")
    dy <- format(dtsnew[1], "%d")
    hosp <- ts(hosp, frequency = 365, start = c(as.numeric(yr), as.numeric(mth)))
    plot(hosp, x = seq(sum(ref)), xaxt = "n", type = "l", 
         main = cou, ylab = "Case numbers", xlab = "Date")
    lines(y = icu, x = seq(sum(ref)), col = "red")
    lines(y = 0.2*hosp, x = seq(sum(ref)), col = "blue")
    yy <- seq(from = 1, to = length(dtsnew), by = 15)
    axis(side = 1, labels = dtsnew[yy], at = yy, cex.axis = 0.75)
    legend("top", legend= c("Hospital COVID cases", "ICU occupancy","Approximation"),
       col=c("black", "red", "blue"), lty= c(1,1,1), cex=0.8, bty = "n")
}



## variables to consider
lagInfo <- read.csv("lag_data.csv")

arimaRoll <- function(y, X, h, lags, dts, ttl, fn){
    ## function to make rolling arima predictions
    ## y is the variable we are interested in modelling
    ## X is a matrix containing the external regressors
    ## h is the length of the forecast ahead we want to make (i.e. \hat{y}_{t+h})
    ## lags relevant lags
    ## dts is a vector containing the dates
    ## ttl is the intial string to go in the plot title
    ## adjust external regressors for lags
    lags <- abs(lags) # make sure all are positive 
    lags[lags < h] <- h # default lags to at least forecast length
    maxlag <- max(lags)
    minlag <- min(lags)
    ynew <- y[seq(1 + maxlag, length(y))]
    Xnew <- matrix(0, nrow = length(ynew) + min(h, lags), ncol = ncol(as.matrix(X)))
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
        if(all(Xnew == 0)){
            m <- auto.arima(ynew[ref], 
                            max.p = 10, 
                            max.q = 10)
            ## make forecast
            if(i + h > n + minlag){
                ## if we are forecasting unobserved days
                frcstref <- seq(i + 1, i + minlag) # reference for forecast
                p <- forecast(m, h = n + minlag - i)
                yy <- seq(from = 1, to = i + minlag, by = 2*h)
                png(paste(fn, cou,"_",dtsnew[i+minlag], ".png", sep = ""))
                plot(p, showgap = FALSE, xaxt = "n",
                     ylab = "ICU occupancy", main = paste(ttl,": ", p$method, sep = ""))
                axis(side = 1, labels = dtsnew[yy], at = yy, cex.lab = 0.75)
                lines(x = frcstref, y = ynew[frcstref], col = "red")
                dev.off()
                next
            }
            p <- forecast(m, h = h)
            yy <- seq(from = 1, to = i + h, by = 2*h)
            png(paste(fn, cou, "_", dtsnew[i+h], ".png",sep = ""))
            plot(p, showgap = FALSE, xaxt = "n", 
                 ylab = "ICU occupancy", main = paste(ttl,": ", p$method, sep = ""))
            axis(side = 1, labels = dtsnew[yy], at = yy, cex.axis = 0.75)
            lines(x = frcstref, y = ynew[frcstref], col = "red")
            dev.off()
        } else{
            m <- auto.arima(ynew[ref], 
                            max.p = 10, 
                            max.q = 10,
                            xreg = Xnew[ref,])
            ## make forecast
            if(i + h > n + minlag){
                ## if we are forecasting unobserved days
                frcstref <- seq(i + 1, i + minlag) # reference for forecast
                p <- forecast(m, h = n + minlag - i, xreg = Xnew[frcstref,])
                yy <- seq(from = 1, to = i + minlag, by = 2*h)
                png(paste(fn, cou,"_",dtsnew[i+minlag], ".png", sep = ""))
                plot(p, showgap = FALSE, xaxt = "n",
                     ylab = "ICU occupancy", main = paste(ttl,": ", p$method, sep = ""))
                axis(side = 1, labels = dtsnew[yy], at = yy, cex.lab = 0.75)
                lines(x = frcstref, y = ynew[frcstref], col = "red")
                dev.off()
                next
            }
            p <- forecast(m, h = h, xreg = Xnew[frcstref,])
            yy <- seq(from = 1, to = i + h, by = 2*h)
            png(paste(fn, cou, "_", dtsnew[i+h], ".png",sep = ""))
            plot(p, showgap = FALSE, xaxt = "n", 
                 ylab = "ICU occupancy", main = paste(ttl,": ", p$method, sep = ""))
            axis(side = 1, labels = dtsnew[yy], at = yy, cex.axis = 0.75)
            lines(x = frcstref, y = ynew[frcstref], col = "red")
            dev.off()


        }
    }
    invisible(m)
}

## loop through countries
for(cou in countrylist){
    ## collect variables of interest
    icu <- owid[owid$location == cou, "icu_patients"]
    if(sum(!is.na(icu)) < 100){
        cat("Not sufficient ICU data for ", cou, " rather use adjusted hospital data.", "\n",sep = "")
        hosp <- owid[owid$location == cou, "hosp_patients"]
        hosp[is.na(hosp)] <- 0
        ref <- !(hosp == 0)
        hosp <- hosp[ref]
        icu <- 0.2 * hosp
        ttl <- paste("Crude ", cou, sep = "")
    } else{
        ttl <- paste(cou, sep = "")
    }
    ## get variable names
    vars <- as.vector(lagInfo[lagInfo$location == cou, "covariate"])
    ## make lag vector
    lvec <- abs(lagInfo[lagInfo$location == cou, "lag"])
    X <- as.matrix(owid[owid$location == cou, vars]) ## matrix of external regressors
    if(any(lvec == 1000)){
        X <- as.matrix(X[,which(lvec != 1000)])
        lvec <- lvec[lvec != 1000]
    } 
    dates <- owid[owid$location == cou, "date"]
    ## clean data
    icu[is.na(icu)] <- 0
    ref <- !(icu == 0)
    icu <- icu[ref]
    X <- X[ref,]
    X[is.na(X)] <- 0
    ## fit over rolling period
    fn <- "arimaplots/"
    if(which(cou == countrylist) > which(countrylist == "Italy")) fn <- "arimaplots2/"
    m <- arimaRoll(y = icu, X = X, h = 10, lags = lvec, dts = dates, ttl = ttl, fn = fn) 
    cat(cou," complete \n",sep = "")
}

## Example residual analysis for Austria
cou <- "Austria"
h <- 10

icu <- owid[owid$location == cou, "icu_patients"]
vars <- as.vector(lagInfo[lagInfo$location == cou, "covariate"])
lvec <- abs(lagInfo[lagInfo$location == cou, "lag"])
X <- as.matrix(owid[owid$location == cou, vars]) ## matrix of external regressors
dates <- owid[owid$location == cou, "date"]

## clean data
icu[is.na(icu)] <- 0
ref <- !(icu == 0)
y <- icu[ref]
X <- X[ref,]
X[is.na(X)] <- 0


lags <- abs(lvec) # make sure all are positive 
lags[lags < h] <- h # default lags to at least forecast length
maxlag <- max(lags)
minlag <- min(lags)
ynew <- y[seq(1 + maxlag, length(y))]
Xnew <- matrix(0, nrow = length(ynew) + min(h, lags), ncol = ncol(X))
dtsnew <- as.Date(dates[seq(1 + maxlag, length(dates))])
yr <- format(dtsnew[1], "%Y")
mth <- format(dtsnew[1], "%m")
dy <- format(dtsnew[1], "%d")
for(i in seq(ncol(X))){
    ref <- seq(1 + maxlag - lags[i], length(y) + min(h, minlag) - lags[i])
    Xnew[, i] <- X[ref, i]
}

i <- which(dtsnew == "2020-07-26")
ref <- seq(i) # reference for training
frcstref <- seq(i + 1, i + h) # reference for forecast
## fit models
yfit <- ts(ynew[ref], frequency = 365, 
           start = c(as.numeric(yr), as.numeric(mth)))

m <- auto.arima(ynew[ref], 
                max.p = 10, 
                max.q = 10,
                xreg = Xnew[ref,])
## look at residuals
plot(m$residuals, ylab = "Residuals", main = "Residuals over time")
acf(m$residuals, main = "Residual ACF")


str(m)
