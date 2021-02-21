

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


country <- "Luxembourg"

# Special case: Luxembourg ------------------------------------------------

test <- allcountries%>%
  filter(location == country)%>%
  select(date, new_cases)%>%
  # 189 days remain
  filter(date <= '2020-08-30')%>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases))%>%
  mutate(positive_proportion = new_cases / sum(ifelse(new_cases > 0, new_cases, 0)),
         total_cases = sum(new_cases),
         new_new_cases = ifelse(new_cases < 0, round(total_cases/189),
                                round(positive_proportion * 188/189 * total_cases)))

countrycases <- test%>%select(date, new_new_cases)%>%setNames(c("date", "new_cases"))%>%
  rbind(
    allcountries%>%
      filter(location == country)%>%
      select(date, new_cases)%>%
      filter(date > '2020-08-30')
  )



###########################################################################
# Environment -------------------------------------------------------------
###########################################################################

setwd("/storage/u")
# SGE_ID <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# set.seed(SGE_ID)
set.seed(6)

###########################################################################
# Parameters --------------------------------------------------------------
###########################################################################

### parameters
steps_in_day <- 10 # no. timesteps in one day (INTEGER)
timestep <- 1/steps_in_day # time step at a daily level
no_knots <- 10 # number of knots for the Bayesian GAM

case_data = data.frame("date" = countrycases$date,
                       "daycount" = countrycases$new_cases,
                       "days_in"=seq(0,nrow(countrycases)-1))


fits_br <- brm(data = case_data,
               family = negbinomial(),
               daycount~s(days_in,bs="tp",k=no_knots),
               iter=4000,
               control=list(adapt_delta=0.99,max_treedepth=20))


save(fits_br, file=paste0("brmsfit_",country,".RData"))


