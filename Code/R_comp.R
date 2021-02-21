library(zoo)
dir.create(file.path(here('Plots/Rt_medians_lockdown')), showWarnings = FALSE)
dir.create(file.path(here('Plots/Rt_comparison')), showWarnings = FALSE)
countryfits = list.files("Output_data\\Rt_medians")
countriestoplot = str_remove(countryfits,"Rt_medians.csv")
for (country in countriestoplot){
  file = paste("Output_data\\Rt_medians\\",country,"Rt_medians.csv",sep="")
  rtcount = read.csv(file)
  rtcount = head(rtcount,-1)
  rtcount$Date = as.Date(rtcount$Date,format = "%Y-%m-%d")
  allcountries = read.csv("owid-covid-data.csv")
  allcountries$date = as.Date(allcountries$date,format = "%d/%m/%Y")
  countrycases= allcountries[allcountries$location==country,c('date','new_cases','total_cases','stringency_index','reproduction_rate')]
  countrycases = countrycases[which(countrycases$total_cases==countrycases$new_cases):nrow(countrycases),]
  countrycases = merge(countrycases, rtcount,by.x = 'date',by.y='Date')
  countrycases[nrow(countrycases),'Rt']=NA  
  undercontrol = countrycases[which(countrycases$Rt<1)[1]:nrow(countrycases),]
  plausibleR = max(undercontrol$Rt,na.rm =  TRUE)
  
  countrycases = countrycases[which(countrycases$Rt<plausibleR)[1]:nrow(countrycases),]
  
  

  countrycases$stringency_index =na.locf(na.locf(countrycases$stringency_index), fromLast = TRUE)
  countrycases[which(countrycases$stringency_index==max(countrycases$stringency_index)),]
  

  lockdown_dates = countrycases[which(countrycases$stringency_index==max(countrycases$stringency_index)),]
  lockdown_start  = min(lockdown_dates$date)
  lockdown_end = max(lockdown_dates$date)
  
  #lockdown_period = countrycases[countrycases$date>lockdown_start&countrycases$date<lockdown_end+7,]

  # ggplot() +
  #   geom_point(data=countrycases, mapping=aes(x=date, y=Rt,col='SEIR R(t)',group="R(t) with SIR"))+
  #   geom_point(data=countrycases, mapping=aes(x=date, y=reproduction_rate,col='SIR R(t)'))+
  #   geom_hline(yintercept=1)+
  #   ggtitle(paste(country,"R(t) with SEIR and SIR"))+
  #   ylab("R(t)")+
  #   xlab("Date")+
  #   scale_y_continuous(breaks = c(0.5,seq(1,5,1)))+
  #   theme(plot.title = element_text(hjust = 0.5))
  # ggsave(paste("Plots/Rt_comparison/",country,".png"))
  ggplot() +
    geom_point(data=countrycases, mapping=aes(x=date, y=Rt,col='SEIR R(t)',group="R(t) with SIR"))+
    geom_step(data=countrycases, mapping=aes(x=date, y=stringency_index/50,col="Rescaled stringency"))+
    geom_hline(yintercept=1)+
    ggtitle(paste(country,"R(t) and stringency"))+
    ylab("R(t)")+
    xlab("Date")+
    scale_y_continuous(breaks = c(0.5,seq(1,5,1)))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste("Plots/Rt_medians_lockdown/",country,".png"))
  # # ggplot() +
  #   geom_point(data=countrycases, mapping=aes(x=date, y=c(NA,diff(Rt,1)),col='SEIR R(t)',group="R(t) with SIR"))+
  #   geom_step(data=countrycases, mapping=aes(x=date, y=(stringency_index-75)/1000))+
  #   ggtitle("Ireland R(t)")+  
  #   ylab("R(t)")+
  #   xlab("Date")
  #   scale_y_continuous(breaks = c(0.5,seq(1,5,1)))+
  #   theme(plot.title = element_text(hjust = 0.5))
}    
countrycases
