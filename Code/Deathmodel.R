library('pracma')
allcountries = read.csv("owid-covid-data.csv")
allcountries$date = as.Date(allcountries$date,format = "%d/%m/%Y")

countrydf = data.frame(Country=character(),modelcoef = double())

k=1
for (country  in countriestoplot){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases','total_cases','positive_rate','new_deaths_smoothed','new_tests_smoothed','population')]
  countrycases = countrycases[which(countrycases$total_cases==countrycases$new_cases):nrow(countrycases),]
  
  countrycases['populationtested'] = countrycases$population/countrycases$new_tests_smoothed
  
  ggplot() +
    geom_point(data=countrycases, mapping=aes(x=date, y=0.1*cases_07da,col="exp(pos_rate*22)"))+
    geom_point(data=countrycases, mapping=aes(x=date, y=new_deaths_smoothed,col="deaths"))
  
  ggplot() +
    geom_point(data=countrycases, mapping=aes(x=date, y=new_deaths_smoothed,col="deaths"))+
    geom_point(data=countrycases, mapping=aes(x=date, y=exp(22*smooth_pos_rate),col="exp(pos_rate*22)"))
  
  ggplot() +
    geom_point(data=countrycases, mapping=aes(x=date, y=new_deaths_smoothed,col="deaths"))+
    geom_point(data=countrycases, mapping=aes(x=date, y=populationtested/100,col="exp(pos_rate*22)"))
  
  
  countrycases['smooth_pos_rate'] = movavg(countrycases$positive_rate,n=7,type='s')
  countrycases = countrycases[is.na(countrycases$smooth_pos_rate)==FALSE,]
  if(nrow(countrycases)==0) next

  ggplot() +
    geom_point(data=countrycases, mapping=aes(x=new_deaths_smoothed, y=exp(22*smooth_pos_rate),col="exp(pos_rate*30"))
  summary(exponential.model)
  exponential.model <- lm(log(new_deaths_smoothed)~  lag(smooth_pos_rate,n=7)),data = countrycases )
  countrydf[k,1] = country
  countrydf[k,2]= log(exponential.model$coefficients[1])
  k=k+1
}
write.csv(countrydf,file = "deathmodelcoef.csv",row.names = FALSE)
clusters = read.csv("clusters.csv")

countrydf = merge(countrydf,clusters,by.x   = 'Country',by.y = 'country')
ddply(countrydf, .(cluster), summarise, Sumx1 = mean(modelcoef), Sumx2 = sd(modelcoef))
