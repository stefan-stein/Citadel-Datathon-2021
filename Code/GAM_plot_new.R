source('Source.R')

countryfits = list.files("Output_data\\sample_curves")
alreadydone = list.files("Plots")
newcountries = countryfits[countryfits %in% alreadydone == FALSE] 
allcountries = read.csv("owid-covid-data.csv")
allcountries$date = as.Date(allcountries$date,format = "%d/%m/%Y")
nsim = 1 #number of simulated values
for (country in newcountries){
  dir.create(file.path(here('Plots',country)))
  data_dir <- here('Output_data/sample_curves', country)
  dailyNewCases_samples  = read.csv(paste0(data_dir,'/',country,'_dailyNewCases_samples.csv'),row.names=NULL)
  #apply(dailyNewCases_samples,1,quantile,prob=c(0.025,0.25,0.5,0.75,0.975)) %>%
  countrycases= allcountries[allcountries$location==country,c('date','new_cases','total_cases')]
  countrycases = countrycases[which(countrycases$total_cases==countrycases$new_cases):nrow(countrycases),]
  case_data = data.frame("date" = countrycases$date, "daycount" = countrycases$new_cases ,"days_in"=seq(0,nrow(countrycases)-1))
  firstdate = countrycases$date[1] #dates are listed chrnologically
  
 
  apply(dailyNewCases_samples,1,function(x){quantile(x[!is.na(x)],prob=c(0.025,0.25,0.5,0.75,0.975))})%>%
    t() %>%
    as_tibble() %>%
    mutate(time = row_number(),
           average = dailyNewCases_samples %>% t() %>% colMeans()) %>%
    rename('q025' = `2.5%`,
           'q25' = `25%`,
           'q50' = `50%`,
           'q75' = `75%`,
           'q975' = `97.5%`) %>%
    ggplot(aes(x = time, y = q50)) +
    geom_point(data = case_data,
               aes(x = days_in, y = daycount),
               size = 0.6) +
    geom_line(color = '#ED1C24',
              size = 0.5) +
    geom_ribbon(aes(ymin = q25, ymax = q75),
                fill = '#ED1C24',
                alpha = 0.4) +
    geom_ribbon(aes(ymin = q025, ymax = q975),
                fill = '#ED1C24',
                alpha = 0.2) +
    labs(x = 'Time (Days)',
         y = 'New conifmed cases',
         title = paste('New confirmed cases of Covid-19 in ',country,' from ',firstdate)) +
    theme(plot.title = element_text(size = 12, face = "bold") )
  
  
  ggsave(here('Plots',country, paste('daily_cases_', country, '.pdf', sep = '')),
         width = 6, height = 4,
         units = 'in', device = cairo_pdf)
  
  dir_in <- here('Output_data/sample_curves', country, '/')
  files <- list.files(dir_in)
  plot_names <- c('Beta', 'CcF', 'Exposed',
                  'Force of Infection', 'Infected',
                  'Daily New Cases', 'Recovered',
                  'R(t)', 'Susceptible')
  
  for(ii in 1:length(files)){
    file <- files[ii]
    variable <- str_remove(file,'_samples.csv')
    data <- read.csv(text=paste0(head(readLines(paste0(dir_in,file)), -nsim), collapse="\n")) #remove simulated data from end
    #apply(data,1,quantile,prob=c(0.025,0.25,0.5,0.75,0.975)) %>%
    apply(data,1,function(x){quantile(x[!is.na(x)],prob=c(0.025,0.25,0.5,0.75,0.975))})%>%
      t() %>%
      as_tibble() %>%
      mutate(time = row_number(),
             average = data %>% t() %>% colMeans()) %>%
      rename('q025' = `2.5%`,
             'q25' = `25%`,
             'q50' = `50%`,
             'q75' = `75%`,
             'q975' = `97.5%`) %>%
      ggplot(aes(x = time, y = average)) +
      geom_line(color = '#ED1C24',
                size = 0.75) +
      geom_ribbon(aes(ymin = q25, ymax = q75),
                  fill = '#ED1C24',
                  alpha = 0.4) +
      geom_ribbon(aes(ymin = q025, ymax = q975),
                  fill = '#ED1C24',
                  alpha = 0.2) +
      labs(x = 'Time (Days)',
           y = plot_names[ii],
           title = paste(plot_names[ii], 'in',country,'from',firstdate, sep = ' ')) +
           theme(plot.title = element_text(size = 12, face = "bold"))
    ggsave(here('Plots',country,paste(variable,'.pdf', sep ='')), width = 6, height = 4,
           units = 'in', device = cairo_pdf)
  }
}
plot_names ='Beta'
