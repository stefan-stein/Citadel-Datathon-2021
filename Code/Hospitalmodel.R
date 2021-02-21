pacf (x, lag = length(x) - 1, pl = TRUE, ...)

allcountries = read_csv("owid-covid-data.csv",col_types = paste(c(rep("c",3),"D",rep("d",29),"c",rep("d",18)),collapse = ""))
country = "Austria"

countrycases= allcountries[allcountries$location==country,c('date','new_cases','icu_patients','positive_rate'),]
countrycases = countrycases[is.na(countrycases$icu_patients)==FALSE,]
pacf(countrycases$icu_patients,30)
acf(countrycases$icu_patients)
plot(countrycases$icu_patients)
lm(icu_patients~lag(new_cases,k=2),data = countrycases)
plot(countrycases$icu_patients,log(lag(countrycases$new_cases,k=2)))



JHCovid19States <- countrycases %>%
  dplyr::mutate(death_03da = zoo::rollmean(new_cases, k = 3, fill = NA),
                death_05da = zoo::rollmean(new_cases, k = 5, fill = NA),
                death_07da = zoo::rollmean(new_cases, k = 7, fill = NA),
                death_15da = zoo::rollmean(new_cases, k = 15, fill = NA),
                death_21da = zoo::rollmean(new_cases, k = 21, fill = NA)) %>% 
  dplyr::ungroup()


ggplot() +
  geom_point(data=JHCovid19States, mapping=aes(x=seq(nrow(JHCovid19States)), y=log(new_cases)))+
  geom_step(data=JHCovid19States, mapping=aes(x=seq(nrow(JHCovid19States)), y=icu_patients))

plot(JHCovid19States$icu_patients,log(JHCovid19States$new_cases))
plot(JHCovid19States$icu_patients)
plot(log(JHCovid19States$new_cases))
