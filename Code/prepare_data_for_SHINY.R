
library(tidyverse)
library(naniar)


# Prepare data for SHINY --------------------------------------------------



# Stringency and positive_rate --------------------------------------------



owid_covid_data <- read_csv("Datathon Materials/1_owid/owid-covid-data.csv",
                            col_types = paste(c(rep("c",3),"D",rep("d",29),"c",rep("d",18)),collapse = ""))

agerange_data <- read_csv("Datathon Materials/2_ecdc/agerangenotificationeu.csv")

country_list <- agerange_data%>%pull(country)%>%
  unique()


europe <- owid_covid_data%>%
  filter(location %in% country_list)

europe%>%pull(location)%>%unique()%>%length()

find_last_non_missing <- function(country, dt){
  europe%>%
    select(location, date, stringency_index)%>%
    filter(location == country, date < dt)%>%
    filter(!is.na(stringency_index))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(stringency_index)
  
}

find_last_non_missing("Austria", '2020-12-25')

stringency <- europe%>%
  select(location, date, stringency_index)%>%
  filter(date >= as.Date('2020-04-01'), date <= as.Date('2020-12-31'))%>%
  rowwise()%>%
  mutate(new_stringency = ifelse(!is.na(stringency_index), stringency_index, 
                                 find_last_non_missing(location, date)))%>%
  ungroup()%>%
  select(-stringency_index)%>%
  pivot_wider(names_from = date, values_from = new_stringency)

vis_miss(stringency)

write_csv(stringency, "stringency.csv")



find_last_non_missing <- function(country, dt){
  europe%>%
    select(location, date, positive_rate)%>%
    filter(location == country, date < dt)%>%
    filter(!is.na(positive_rate))%>%
    arrange(desc(date))%>%
    slice(1)%>%
    pull(positive_rate)
  
}

pos_rate <- europe%>%
  select(location, date, positive_rate)%>%
  filter(date >= as.Date('2020-04-01'), date <= as.Date('2020-12-31'))%>%
  rowwise()%>%
  mutate(new_pos_rate = ifelse(!is.na(positive_rate), positive_rate, 
                                 find_last_non_missing(location, date)))%>%
  ungroup()%>%
  select(-positive_rate)%>%
  pivot_wider(names_from = date, values_from = new_pos_rate)

vis_miss(pos_rate)

write_csv(pos_rate, "positive_rate.csv")




# R values ----------------------------------------------------------------


path <- "Citadel-Datathon-2021/Keelan/Rt_medians/"


country_list <- c("Austria","Belgium","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Lithuania","Luxembourg",
                 "Malta", "Netherlands","Norway","Poland","Portugal","Romania","Slovakia",
                 "Slovenia","Spain","Sweden")
# Ireland is not there
# country_list <- country_list[-14]


df <-   
lapply(country_list,
       function(c) read_csv(paste0(path,c,"Rt_medians.csv"))%>%
                              mutate(location = c))%>%
  bind_rows()


test <- df%>%
  mutate(Rt = ifelse(Rt == 0.5, NA, Rt))%>%
  filter(Date >= as.Date('2020-04-01'), Date <= as.Date('2020-12-31'))%>%
  pivot_wider(names_from = Date, values_from = Rt)

vis_miss(test)

write_csv(test, "Rt.csv")




# Mobility ----------------------------------------------------------------

library(tidyverse)
Google_mobility <- read_csv("Datathon Materials/Google_mobility.csv")

library(zoo)

Google <- Google_mobility%>%
  mutate(transit_stations_percent_change_from_baseline = 1 + transit_stations_percent_change_from_baseline/100,
         retail_and_recreation_percent_change_from_baseline = 1 + retail_and_recreation_percent_change_from_baseline/100,
         workplaces_percent_change_from_baseline = 1 + workplaces_percent_change_from_baseline/100,
         grocery_and_pharmacy_percent_change_from_baseline = 1 + grocery_and_pharmacy_percent_change_from_baseline/100)%>%
  group_by(country_region)%>%
  #mutate(rolling_transit = rollmean(zoo(transit_stations_percent_change_from_baseline, date), k=7, fill = NA, align = "right"))
  mutate(rolling_transit=rollapply(transit_stations_percent_change_from_baseline,7,mean,align='right',fill=NA),
         rolling_retail_and_recreation = rollapply(retail_and_recreation_percent_change_from_baseline,7,mean,align='right',fill=NA),
         rolling_workplaces = rollapply(workplaces_percent_change_from_baseline,7,mean,align='right',fill=NA),
         rolling_grocery_and_pharmacies = rollapply(grocery_and_pharmacy_percent_change_from_baseline,7,mean,align='right',fill=NA))%>%
  select(country_region, date, starts_with("rolling"))

write_csv(Google, "mobility.csv")
min(Google[,3:6], na.rm = T)
