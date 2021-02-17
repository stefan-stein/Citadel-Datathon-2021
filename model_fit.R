source('Source.R')

### parameters

steps_in_day <- 10 # no. timesteps in one day (INTEGER)
timestep <- 1/steps_in_day # time step at a daily level
no_knots <- 10 # number of knots for the Bayesian GAM


file_in = 'owid-covid-data.csv'
allcountries = read_csv("owid-covid-data.csv")
countrylist  = unique(allcountries$location,use.names=FALSE)
countrylist  = countrylist[!countrylist %in% c("International","World")]

for (country in countrylist){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases')]
  case_data = data.frame("date" = countrycases$date, "daycount" = countrycases$new_cases ,"days_in"=seq(0,nrow(countrycases)-1))
  casesdf[[country]] = countrycases
}

for (country in countrylist){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases')]
  case_data = data.frame("date" = countrycases$date, "daycount" = countrycases$new_cases ,"days_in"=seq(0,nrow(countrycases)-1)
  fits_br <- brm(data = case_data,
                 family = negbinomial(),
                 daycount~s(days_in,bs="tp",k=no_knots),
                 iter=4000,
                 control=list(adapt_delta=0.99,max_treedepth=20))
  filename <- here('Fits',paste0("brmsfit_",country,".Rdata"))
  save(fits_br,file=filename)
}





