
###########################################################################
# Packages ----------------------------------------------------------------
###########################################################################

library(brms)
library(mgcv)
library(tidyverse)
library(lubridate)
library(here)


###########################################################################
# Data --------------------------------------------------------------------
###########################################################################


allcountries <- read_csv("owid-covid-data.csv",
                         col_types = paste(c(rep("c",3),"D",rep("d",29),"c",rep("d",18)),collapse = ""))


countrylist <- c("Austria","Belgium","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Lithuania","Luxembourg",
                 "Malta", "Netherlands","Norway","Poland","Portugal","Romania","Slovakia",
                 "Slovenia","Spain","Sweden")


###########################################################################
# Environment -------------------------------------------------------------
###########################################################################

setwd("/storage/u")
SGE_ID <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# SGE_ID <- 1
set.seed(SGE_ID)


###########################################################################
# Parameters --------------------------------------------------------------
###########################################################################

### parameters
steps_in_day <- 10 # no. timesteps in one day (INTEGER)
timestep <- 1/steps_in_day # time step at a daily level
no_knots <- 10 # number of knots for the Bayesian GAM


country <- countrylist[SGE_ID]
countrycases= allcountries[allcountries$location==country,c('date','new_cases')]

case_data = data.frame("date" = countrycases$date,
                       "daycount" = countrycases$new_cases,
                       "days_in"=seq(0,nrow(countrycases)-1))

fits_br <- brm(data = case_data,
               family = negbinomial(),
               daycount~s(days_in,bs="tp",k=no_knots),
               iter=4000,
               control=list(adapt_delta=0.99,max_treedepth=20))


save(fits_br, file=paste0("brmsfit_",country,".RData"))

