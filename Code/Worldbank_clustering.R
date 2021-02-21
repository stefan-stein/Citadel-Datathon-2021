
# Clustering --------------------------------------------------------------

library(tidyverse)
library(cluster)
library(factoextra)
library(readxl)



# Data --------------------------------------------------------------------

# age range
agerange_data <- read_csv("Datathon Materials/2_ecdc/agerangenotificationeu.csv")
# gini
API_SI_POV_GINI_DS2_en_excel_v2_2055827 <- read_excel("Citadel-Datathon-R/Clustering/API_SI/API_SI.POV.GINI_DS2_en_excel_v2_2055827.xls", 
                                                      skip = 3)
# gdp
API_NY_GDP_PCAP_CD_DS2_en_excel_v2_2055610 <- read_excel("itadel-Datathon-R/Clustering/API_NY/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_2055610.xls", 
                                                         skip = 3)

country_list <- agerange_data%>%pull(country)%>%
  unique()

# for warning time
owid_covid_data <- read_csv("Datathon Materials/1_owid/owid-covid-data.csv",
                            col_types = paste(c(rep("c",3),"D",rep("d",29),"c",rep("d",18)),collapse = ""))%>%
  filter(location %in% country_list)


# GINI --------------------------------------------------------------------

gini <- 
  API_SI_POV_GINI_DS2_en_excel_v2_2055827%>%
  mutate(country = ifelse(`Country Name` == "Czech Republic", "Czechia",
                          ifelse(`Country Name` == "Slovak Republic", "Slovakia", `Country Name`)))%>%
  filter(country %in% country_list)%>%
  mutate(GINI = ifelse(!is.na(`2018`), `2018`,
                       ifelse(!is.na(`2017`), `2017`,
                              ifelse(!is.na(`2016`), `2016`, `2015`))))%>%
  select(country, GINI)


# GDP ---------------------------------------------------------------------

gdp <- 
  API_NY_GDP_PCAP_CD_DS2_en_excel_v2_2055610%>%
  mutate(country = ifelse(`Country Name` == "Czech Republic", "Czechia",
                          ifelse(`Country Name` == "Slovak Republic", "Slovakia", `Country Name`)))%>%
  filter(country %in% country_list)%>%
  mutate(GDP = `2019`)%>%
  select(country, GDP)


# WARNING TIME ------------------------------------------------------------
# 
# warning_time <- owid_covid_data%>%
#   group_by(location)%>%
#   summarise(first_case = min(date, na.rm = T))%>%
#   ungroup()%>%
#   mutate(day_zero = min(first_case),
#          warning_time = first_case - day_zero)%>%
#   select(location, warning_time)%>%
#   setNames(c("country", "warning_time"))


outbreak_day <- owid_covid_data%>%
  select(location, date, new_cases)%>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases))%>%
  group_by(location)%>%
  mutate(cum_cases = cumsum(new_cases))%>%
  filter(cum_cases >= 100)%>%
  mutate(outbreak = min(date, na.rm = TRUE))%>%
  select(location, outbreak)%>%
  slice(1)

warning_time <- owid_covid_data%>%
  left_join(outbreak_day, by = "location")%>%
  mutate(day_zero = min(outbreak),
         warning_time = outbreak - day_zero)%>%
  select(location, warning_time)%>%
  group_by(location)%>%
  slice(1)%>%
  setNames(c("country", "warning_time"))
  


# SUSCEPTIBILITY ----------------------------------------------------------

total_cases <- sum(agerange_data$new_cases)

susceptible <- agerange_data%>%
  group_by(age_group)%>%
  summarise(cases_per_group = sum(new_cases),
         fraction_of_cases = cases_per_group / total_cases)%>%
  left_join(
    agerange_data%>%
      group_by(country,age_group)%>%slice(1)%>%
      group_by(age_group)%>%
      summarise(total_size = sum(population))%>%
      mutate(proportion = total_size / sum(total_size)),
    by = "age_group"
  )


susceptible <- agerange_data%>%
  group_by(age_group)%>%
  summarise(cases_per_group = sum(new_cases),
            fraction_of_cases = cases_per_group / total_cases)%>%
  select(age_group, fraction_of_cases)%>%
  setNames(c("age_group", "fraction")) %>%
  mutate(type = "cases") %>%
  rbind(
    agerange_data%>%
      group_by(country,age_group)%>%slice(1)%>%
      group_by(age_group)%>%
      summarise(total_size = sum(population))%>%
      mutate(fraction = total_size / sum(total_size))%>%
      select(age_group, fraction) %>%
      mutate(type = "population")
  )

age_weights <- susceptible%>%filter(type == "cases")%>%select(-type)

ggplot(susceptible) +
  geom_point(aes(x = age_group, y = fraction, col = type))+
  labs(title = "Difference in susceptibility to COVID and proportion in the population by age group")

# AGERANGE ----------------------------------------------------------------


age_distribution <- 
  agerange_data%>%group_by(country, age_group)%>%
  slice(1)%>%
  group_by(country)%>%
  mutate(total_population = sum(population))%>%
  ungroup()%>%
  mutate(population_percentage = population/total_population)%>%
  select(country, age_group, population_percentage)%>%
  pivot_wider(names_from = age_group, values_from = population_percentage)


# Data --------------------------------------------------------------------

df <- age_distribution%>%
  left_join(gini)%>%
  left_join(gdp)%>%
  left_join(warning_time)%>%
  mutate(warning_time = as.numeric(warning_time))%>%
  mutate(`<15yr` = `<15yr` * age_weights$fraction[1],
         `15-24yr` = `15-24yr` * age_weights$fraction[2],
         `25-49yr` = `25-49yr` * age_weights$fraction[3],
         `50-64yr` = `50-64yr` * age_weights$fraction[4],
         `65-79yr` = `65-79yr` * age_weights$fraction[5],
         `80+yr` = `80+yr` * age_weights$fraction[6])%>%
  column_to_rownames("country")%>%
  scale()

rm(list = setdiff(ls(), "df"))

# Clustering --------------------------------------------------------------

set.seed(123)
# Elbow method
fviz_nbclust(df, kmeans, method = "wss")
# Silhuette method
fviz_nbclust(df, kmeans, method = "silhouette")

k4 <- kmeans(df, centers = 4, nstart = 25)
fviz_cluster(k4, data = df, repel = TRUE)


l <- k4$cluster%>%
  as.data.frame()%>%
  rownames_to_column("country")%>%
  setNames(c("country", "cluster"))%>%
  arrange(cluster)

write_csv(l, file = "clusters.csv")
