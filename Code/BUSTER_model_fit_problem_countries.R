
# BUSTER Problem countries ------------------------------------------------

# Solution for the problem countries except for Luxembourg


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

# problematic countries (except Luxembourg)
countrylist <- countrylist[c(4,8,9,15,17,19,23,27)]


###########################################################################
# Environment -------------------------------------------------------------
###########################################################################

setwd("/storage/u")
SGE_ID <- as.numeric(Sys.getenv("SGE_TASK_ID"))
set.seed(SGE_ID)


###########################################################################
# Parameters --------------------------------------------------------------
###########################################################################

### parameters
steps_in_day <- 10 # no. timesteps in one day (INTEGER)
timestep <- 1/steps_in_day # time step at a daily level
no_knots <- 10 # number of knots for the Bayesian GAM


country <- countrylist[SGE_ID]

countrycases <- allcountries%>%
  filter(location == country)%>%
  select(date, new_cases)%>%
  mutate(week = cut.Date(date, "2 week"))%>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases))%>%
  group_by(week)%>%
  mutate(num_neg = sum(max(-new_cases, 0)),
         neg_days = sum(new_cases < 0),
         two_week_total = sum(new_cases),
         two_week_proportion = new_cases / two_week_total,
         # this is the contribution to the overall positive cases, if new_cases > 0.
         # If new_cases < 0, this one is irrelevant.
         positive_two_week_proportion = new_cases / sum(ifelse(new_cases > 0, new_cases, 0)),
         new_new_cases = ifelse(new_cases < 0, round(two_week_total / 14),
                                ifelse(neg_days > 0, round(positive_two_week_proportion * (14 - neg_days)/ 14 * two_week_total),
                                       new_cases)),
         new_two_week_total = sum(new_new_cases))%>%
  ungroup()%>%
  select(date, new_new_cases)%>%
  setNames(c("date", "new_cases"))

case_data = data.frame("date" = countrycases$date,
                       "daycount" = countrycases$new_cases,
                       "days_in"=seq(0,nrow(countrycases)-1))


fits_br <- brm(data = case_data,
               family = negbinomial(),
               daycount~s(days_in,bs="tp",k=no_knots),
               iter=4000,
               control=list(adapt_delta=0.99,max_treedepth=20))


save(fits_br, file=paste0("brmsfit_",country,".RData"))


