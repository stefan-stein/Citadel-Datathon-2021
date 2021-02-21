mobility = read.csv( "Global_Mobility_Report.csv")
Euromob = mobility[mobility$country_region %in% countriestoplot,] #restrict to our European countries
Euromob  =Euromob[Euromob$sub_region_1=="",]  # restrict to nationial data
Euromob = Euromob[c("country_region","date","transit_stations_percent_change_from_baseline","retail_and_recreation_percent_change_from_baseline","workplaces_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline")]
Euromob$date = as.Date(Euromob$date,format = "%Y-%m-%d")
write.csv(Euromob,file = "Google_mobility.csv",row.names=FALSE)





allcountries = read.csv("owid-covid-data.csv")
allcountries$date = as.Date(allcountries$date,format = "%d/%m/%Y")

file = paste("Output_data\\Rt_medians\\",country,"Rt_medians.csv",sep="")
rtcount = read.csv(file)
rtcount$Date = as.Date(rtcount$Date,format = "%Y-%m-%d")

country = 'Ireland'
countrymob = Euromob[Euromob$country_region ==country,]
countrycases= allcountries[allcountries$location==country,c('date','new_cases','total_cases','stringency_index','reproduction_rate')]
countrycases = countrycases[which(countrycases$total_cases==countrycases$new_cases):nrow(countrycases),]
countrycases = merge(countrycases, rtcount,by.x = 'date',by.y='Date')
 
undercontrol = countrycases[which(countrycases$Rt<1)[1]:nrow(countrycases),]
plausibleR = max(undercontrol$Rt,na.rm =  TRUE)
countrycases = countrycases[which(countrycases$Rt<plausibleR)[1]:nrow(countrycases),]
countrycases = merge(countrycases,countrymob,by = 'date')
countrycases$stringency_index =na.locf(na.locf(countrycases$stringency_index), fromLast = TRUE)







ggplot() +
  geom_point(data=countrycases, mapping=aes(x=date, y=(transit_stations_percent_change_from_baseline+100)/50,col="trans"))+
  geom_point(data=countrycases, mapping=aes(x=date, y=(retail_and_recreation_percent_change_from_baseline+100)/50,col='Ret'))+
  geom_point(data=countrycases, mapping=aes(x=date, y=(workplaces_percent_change_from_baseline+100)/50,col='Work'))+
  geom_point(data=countrycases, mapping=aes(x=date, y=Rt,col='SEIR R(t)'))+
  geom_hline(yintercept=1)+
  ggtitle(paste(country,"R(t) and stringency"))+
  ylab("R(t)")+
  xlab("Date")+
  scale_y_continuous(breaks = c(0.5,seq(1,5,1)))+
  theme(plot.title = element_text(hjust = 0.5))
countrycases$residential_percent_change_from_baseline


