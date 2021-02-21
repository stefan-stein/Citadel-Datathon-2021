countryfits = list.files("Output_data\\sample_curves")

country = "Latvia"
allcountries = read_csv("owid-covid-data.csv")
allcountries = data.frame(allcountries)
countryfits = list.files("Output_data\\sample_curves")
dir.create(file.path(here('Output_data/Rt_medians')), showWarnings = FALSE)

for (country in countryfits){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases')]
  first_case_date=as.Date(countrycases[1,1],format='%d/%m/%Y')
  dailyNewCases_samples = read.csv(paste0("Output_data\\sample_curves\\",country,"\\",country,"_dailyRt_samples.csv"))
  dates = seq(first_case_date, by = "day", length.out = nrow(dailyNewCases_samples))
  medians  = data.frame("Date" =dates, "Rt"=apply(dailyNewCases_samples, 1, median, na.rm = T))
  medians = head(medians,-2) #remove simulated data
  undercontrol = medians[which(medians$Rt<1)[1]:nrow(medians),] #after initial suppression 
  plausiblemaxR = max(undercontrol$Rt,na.rm =  TRUE) #plausible max R is max of value after initial supression 
  medians = medians[which(medians$Rt<plausiblemaxR)[1]:nrow(medians),]
  medians$Date = as.Date(medians$Date,format = "%Y-%m-%d")
  write.csv(medians,file = paste0("Output_data\\Rt_medians\\",country,'Rt_medians.csv'),
            row.names=FALSE)
}
dir.create(file.path(here('Output_data/It_medians')), showWarnings = FALSE)
for (country in countryfits){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases')]
  first_case_date=as.Date(countrycases[1,1],format='%d/%m/%Y')
  dailyNewCases_samples = read.csv(paste0("Output_data\\sample_curves\\",country,"\\",country,"_dailyRt_samples.csv"))
  dates = seq(first_case_date, by = "day", length.out = nrow(dailyNewCases_samples))
  medians  = data.frame("Date" =dates, "Rt"=apply(dailyNewCases_samples, 1, median, na.rm = T))
  medians = head(medians,-2) #remove simulated data
  undercontrol = medians[which(medians$Rt<1)[1]:nrow(medians),] #after initial suppression 
  plausiblemaxR = max(undercontrol$Rt,na.rm =  TRUE) #plausible max R is max of value after initial supression 
  medians = medians[which(medians$Rt<plausiblemaxR)[1]:nrow(medians),]
  medians$Date = as.Date(medians$Date,format = "%Y-%m-%d")
  write.csv(medians,file = paste0("Output_data\\Rt_medians\\",country,'Rt_medians.csv'),
            row.names=FALSE)
}
dir.create(file.path(here('Output_data/Beta_medians')), showWarnings = FALSE)
for (country in countryfits){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases')]
  first_case_date=as.Date(countrycases[1,1],format='%d/%m/%Y')
  dailyNewCases_samples = read.csv(paste0("Output_data\\sample_curves\\",country,"\\",country,"_dailyBeta_samples.csv"))
  dates = seq(first_case_date, by = "day", length.out = nrow(dailyNewCases_samples))
  medians  = data.frame("Date" =dates, "Rt"=apply(dailyNewCases_samples, 1, median, na.rm = T))
  write.csv(medians,file = paste0("Output_data\\Beta_medians\\",country,'Beta_medians.csv'),
            row.names=FALSE)
}

















plot(medians$Date,medians$Rt)
medians












countrycases= allcountries[allcountries$location=='Sweden',c('date','new_cases','icu_patients')]
plot(as.Date(countrycases$date,format='%d/%m/%Y'),countrycases$icu_patients)
country
summary(allcountries$icu_patients)
