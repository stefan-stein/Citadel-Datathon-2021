import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

sns.set_style("whitegrid")
## --------- MERGING APPLE MOBILITY DATA WITH OWID DATA --------------------
df = pd.read_csv('owid-covid-data.csv')
europe_df = df.loc[df.continent == "Europe"]

europe_df = europe_df.drop(["continent", "iso_code"], axis=1)

## countries we have age data from
country_list = ["Austria",'Belgium','Croatia','Cyprus','Czechia','Denmark',
                'Estonia','Finland','France','Germany','Greece','Hungary',
                'Iceland','Ireland','Italy','Latvia','Lithuania','Luxembourg',
                'Malta', 'Netherlands','Norway','Poland','Portugal','Romania',
                'Slovakia','Slovenia','Spain','Sweden']

europe_df = europe_df[europe_df['location'].isin(country_list)] 
europe_df = europe_df.set_index("date")

mobility_df = pd.read_csv("applemobilitytrends-2021-02-15.csv")

europe = europe_df.location.unique()
  
europe_mobility_df = mobility_df[mobility_df['region'].isin(europe)]
europe_mobility_df = europe_mobility_df.drop(["geo_type", 
                                             "sub-region",
                                              "country",
                                             "alternative_name"], axis=1)
europe_mobility_df = europe_mobility_df.rename(columns={"region": "location"})

## get dataframes from europe mobility data 
for transport_mode in ["walking", "driving", "transit"]:

    europe_mob_df = europe_mobility_df[
             europe_mobility_df["transportation_type"] == transport_mode
                      ].copy()
    europe_mob_df = europe_mob_df.drop(["transportation_type"], axis=1)

    europe_mob_df = europe_mob_df.melt(id_vars="location", 
                            var_name="date", 
                            value_name=transport_mode)

    mask = (europe_mob_df['date'] < "2021-01-06")
    europe_mob_df = europe_mob_df.loc[mask]
    if transport_mode == "walking":
        europe_walking_df = europe_mob_df.set_index(["date", "location"])
    elif transport_mode == "driving":
        europe_driving_df = europe_mob_df.set_index(["date", "location"])
    elif transport_mode == "transit":
        europe_transit_df = europe_mob_df.set_index(["date", "location"])

## plot driving mobility
driving_df = europe_driving_df.driving
driving_df = driving_df.unstack()
driving_df.plot(figsize=(10,10))
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.title("Driving Mobility")
#plt.savefig("driving_mobility.png",bbox_inches='tight' )

## plot transit mobility
transit_df = europe_transit_df.transit
transit_df = transit_df.unstack()
transit_df.plot(figsize=(10,10))
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.title("Transit Mobility")
#plt.savefig("transit_mobility.png",bbox_inches='tight' )

merged_df = europe_df.join(europe_transit_df, on=["date", "location"])
merged_df = merged_df.join(europe_driving_df, on=["date", "location"])
merged_df = merged_df.join(europe_walking_df, on=["date", "location"])

merged_df.to_csv("owid_mobility_europe_data_long.csv")

### ------------ LAGGED CORRELATION WITH ICU PATIENTS ----------------------
df = pd.read_csv('owid_mobility_europe_data_long.csv')

europe_df = df[df['location'].isin(country_list)]
europe_df = europe_df.set_index("date")

def plot_lag_corr_icu(df, country_name):
    max_lag = 30
    for var in ["new_cases_per_million", 
                "positive_rate", 
                "driving",
                "transit"]: 
        lag_corr = []
        lag = []
        for k in range(-max_lag,max_lag+1):
            lag_df = df.shift(k)
            lag_corr.append(df[var].corr(lag_df["icu_patients_per_million"]))
            lag.append(k)
        plt.figure()
        plt.plot(lag, lag_corr)    
        plt.xlim(-max_lag,max_lag)
        plt.ylabel("Correlation")
        plt.xlabel("Lag (days)")
        if var == "new_cases_per_million":
            plt.title("{}: New Cases (per million)".format(country_name))
        elif var == "positive_rate":
            plt.title("{}: Positive Rate".format(country_name))
        elif var == "transit":
            plt.title("{}: Transit Mobility".format(country_name))
        elif var == "driving":
            plt.title("{}: Driving Mobility".format(country_name))
        #plt.savefig("{}_icu_{}".format(country_name,var))

def lag_corr_icu(df, country_name, variable):
    max_lag = 35
    lag_corr = []
    lag = []
    for k in range(-max_lag,max_lag+1):
        lag_df = df.shift(k)
        lag_corr.append(df[variable].corr(lag_df["icu_patients_per_million"]))
        lag.append(k)
    max_corr = max(lag_corr)
    min_corr = abs(min(lag_corr))
    if max_corr > min_corr:
        opt_lag = np.argmax(np.array(lag_corr)) - max_lag
    else:
        opt_lag = np.argmin(np.array(lag_corr)) - max_lag
    return opt_lag

## ICU lagged correlation for covariates:
for country in country_list:
    country_df = europe_df[europe_df["location"] == country]

    plot_lag_corr_icu(country_df, country)

## create lagged data file
lag_data = {'location': [],
           'covariate': [],
           'lag': []}
for country in country_list:
    country_df = europe_df[europe_df["location"] == country]
    
    for variable in ["new_cases_per_million", 
                     "positive_rate"]:
        
        opt_lag = lag_corr_icu(country_df, country, variable)
        if opt_lag <= -7 and opt_lag > -35:
            lag_data['location'].append(country)
            lag_data['covariate'].append(variable)
            lag_data['lag'].append(opt_lag)
        
lag_df = pd.DataFrame(lag_data, columns = ['location','covariate','lag'])
lag_df.to_csv("lag_data.csv")


## Averaged over Europe lagged correlation to ICU:
plot_lag_corr_icu(europe_df, "Europe")



## -------------- EUROPE APPLE MOBILITY DATA AND NEW CASES ---------------


## world mobility
df = pd.read_csv('owid-covid-data.csv')
df = df.set_index("date")
europe_df = df.loc[df.continent == "Europe"]
europe_df = df[df['location'].isin(country_list)]

mobility_df = pd.read_csv("applemobilitytrends-2021-02-15.csv")

world = df.location.unique()
  
mobility_df = mobility_df[mobility_df['region'].isin(world)]

europe = europe_df.location.unique()
mobility_df = mobility_df[~mobility_df['region'].isin(europe)]

mobility_df = mobility_df.drop(["geo_type", 
                                "sub-region",
                                "country",
                                "alternative_name"], axis=1)
mobility_df = mobility_df.rename(columns={"region": "location" })


## get dataframes from mobility data 
for transport_mode in ["walking", "driving", "transit"]:

    mob_df = mobility_df[mobility_df["transportation_type"] == transport_mode].copy()
    mob_df = mob_df.drop(["transportation_type"], axis=1)

    mob_df = mob_df.melt(id_vars="location", 
                            var_name="date", 
                            value_name=transport_mode)
 
    mask = (mob_df['date'] < "2021-01-06")
    mob_df = mob_df.loc[mask]
    if transport_mode == "walking":
        walking_df = mob_df.set_index(["date", "location"])
    elif transport_mode == "driving":
        driving_df = mob_df.set_index(["date", "location"])
    elif transport_mode == "transit":
        transit_df = mob_df.set_index(["date", "location"])


driving_df = driving_df.driving
driving_df = driving_df.unstack()
driving_df.plot(figsize=(10,10))
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.title("Non-European Driving Mobility")
plt.xticks(rotation=45)
#plt.savefig("noneuropean_driving_mobility.png",bbox_inches='tight' )

transit_df = transit_df.transit
transit_df = transit_df.unstack()
transit_df.plot(figsize=(10,10))
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.title("Non-European Transit Mobility")
plt.xticks(rotation=45)
#plt.savefig("noneuropean_transit_mobility.png",bbox_inches='tight' )



## -------------------- EUROPE APPLE MOBILITY -----------------

mobility_df = pd.read_csv("applemobilitytrends-2021-02-15.csv")

europe = europe_df.location.unique()
  
europe_mobility_df = mobility_df[mobility_df['region'].isin(europe)]

europe_mobility_df = europe_mobility_df.drop(["geo_type", 
                                             "sub-region",
                                              "country",
                                             "alternative_name"], axis=1)
europe_mobility_df = europe_mobility_df.rename(columns={"region": "location" })

## get dataframes from europe mobility data 
for transport_mode in ["walking", "driving", "transit"]:

    europe_mob_df = europe_mobility_df[europe_mobility_df["transportation_type"] == transport_mode].copy()
    europe_mob_df = europe_mob_df.drop(["transportation_type"], axis=1)

    europe_mob_df = europe_mob_df.melt(id_vars="location", 
                            var_name="date", 
                            value_name=transport_mode)

    #mask = (europe_mob_df['date'] >= "2020-06-01") & (europe_mob_df['date'] < "2020-12-01")
    #europe_mob_df = europe_mob_df.loc[mask] 
    mask = (europe_mob_df['date'] < "2021-01-06")
    europe_mob_df = europe_mob_df.loc[mask]
    if transport_mode == "walking":
        europe_walking_df = europe_mob_df.set_index(["date", "location"])
    elif transport_mode == "driving":
        europe_driving_df = europe_mob_df.set_index(["date", "location"])
    elif transport_mode == "transit":
        europe_transit_df = europe_mob_df.set_index(["date", "location"])


def plot_driving_transit(country_df):
    for mode in ["driving", "transit"]:
        for stat in ["positive_rate", 
                     "new_deaths_per_million",
                     "new_cases_per_million",
                     "stringency_index",
                    "reproduction_rate"]:
            fig, ax1 = plt.subplots()
            ax2 = ax1.twinx()
            sns.lineplot(x="date", y=stat, data=country_df, ax=ax2, alpha=.5)
            sns.lineplot(x="date", y=mode, data=country_df, ax=ax1)
            if mode == "driving":
                ax1.set(xlabel='Date', ylabel='Driving Mobility')          
            else:
                ax1.set(xlabel='Date', ylabel='Transit Mobility')
            if stat == "positive_rate":
                ax2.set( ylabel='Positive Rate') 
            elif stat == "new_deaths_per_million":
                ax2.set( ylabel='Deaths per Million') 
            elif stat == "new_cases_per_million":
                ax2.set( ylabel='Cases per million')
            elif stat == "stringency_index":
                ax2.set( ylabel='Stringency')
            else:
                ax2.set(ylabel="Reproduction Number")
            plt.setp(ax1.get_xticklabels(), rotation=45)
            ax1.set_xticks(["2020-04-01",
                            "2020-05-01",
                            "2020-06-01",
                            "2020-07-01",
                            "2020-08-01",
                            "2020-09-01",
                            "2020-10-01",
                            "2020-11-01",
                            "2020-12-01",
                            "2021-01-01"])
            fig.tight_layout()
            
            #plt.savefig("{}_{}.png".format(mode,stat),bbox_inches='tight' )

merged_df = europe_df.join(europe_transit_df, on=["date", "location"])
merged_df = merged_df.join(europe_driving_df, on=["date", "location"])
merged_df = merged_df.join(europe_walking_df, on=["date", "location"])

country_df = merged_df
country = "Europe"
country_df = country_df.reset_index()
mask = (country_df['date'] <= "2020-12-31")
country_df = country_df.loc[mask]
plot_driving_transit(country_df)

## plot response measures and mobility
response_df = pd.read_csv('2_ecdc/country_response_measures.csv')

response_df = response_df[response_df['Country'].isin(country_list)]
lockdown_df = response_df.set_index("date_start")

start_df = response_df.drop(["date_end"], axis=1)
start_df = start_df.dropna(axis=0)
#start_df = start_df.set_index("date_start")


end_df = response_df.drop(["date_start"], axis=1)
end_df = end_df.dropna(axis=0)
#end_df = end_df.set_index("date_end")

def plot_mobility_response(country_df, country):
    mode = "driving"
    stat = "new_cases_per_million"
    fig, ax1 = plt.subplots()
    ax2 = ax1.twinx()
    sns.lineplot(x="date", y=stat, data=country_df, ax=ax2, alpha=.5)
    sns.lineplot(x="date", y=mode, data=country_df, ax=ax1)
    
    ax1.set(xlabel='Date', ylabel='Driving Mobility')          
    ax2.set( ylabel='Cases (per million)')

    plt.setp(ax1.get_xticklabels(), rotation=45)
    ax1.set_xticks(["2020-03-01",
                    "2020-04-01",
                    "2020-05-01",
                    "2020-06-01",
                    "2020-07-01",
                    "2020-08-01",
                    "2020-09-01",
                    "2020-10-01",
                    "2020-11-01",
                    "2020-12-01",
                    "2021-01-01"])
    fig.tight_layout()
    plt.title(country)
    start_country_df = start_df[start_df["Country"] == country].copy()
    dates = start_country_df.date_start.unique()
    start_country_df = start_country_df.set_index("date_start")
    for date in dates:
        plt.axvline(date,color='r', linestyle="-", alpha=0.5)

    end_country_df = end_df[end_df["Country"] == country].copy()
    dates = end_country_df.date_end.unique()
    end_country_df = end_country_df.set_index("date_end")
    for date in dates:
        plt.axvline(date,color='g', linestyle="--", alpha=0.5)
    #plt.savefig('{}_driving_cases.png'.format(country),bbox_inches='tight')


country_list_dr = ["Croatia", "France", "Finland", "Sweden"]

# for country in country_list_dr:
#     country_df = europe_df[europe_df["location"] == country]
    
#     plot_mobility_response(country_df, country)

## -------------------- EUROPE GOOGLE MOBILITY -------------------

google_df = pd.read_csv('Google_mobility.csv')
df = pd.read_csv('owid_mobility_europe_data_long.csv')

europe_df = df[df['location'].isin(country_list)]
europe_df = europe_df.set_index("date")

R_data = {'reproduction_number': [],
          'location': [],
          'date':[]}

for country in country_list: ## country_list
    R_df = pd.read_csv('Rt_medians/{}Rt_medians.csv'.format(country))
    dates = R_df.Date.unique()
    R_df = R_df.set_index("Date")
    
    for date in dates:
        R_data['reproduction_number'].append(R_df['Rt'][date])
        R_data['location'].append(country)
        R_data['date'].append(date)
        
R_df = pd.DataFrame(R_data, columns=['date','location','reproduction_number'])

mask = (R_df['date'] < "2021-01-05")
R_df = R_df.loc[mask]
R_df = R_df.set_index(["date", "location"])

merged_df = europe_df.join(R_df, on=["date", "location"]) 


for country in country_list:
    country_df = google_df[google_df["country_region"] == country].copy()
    
    country_df['Transit'] = country_df.iloc[:,2].rolling(window=7).mean()
    country_df['Retail/Recreation'] = country_df.iloc[:,3].rolling(window=7).mean()
    country_df['Workplaces'] = country_df.iloc[:,4].rolling(window=7).mean() 
    #country_df['Grocery/Pharmacy'] = country_df.iloc[:,5].rolling(window=7).mean()
    
    list_mob = ['transit_stations_percent_change_from_baseline',
                'retail_and_recreation_percent_change_from_baseline',
                'workplaces_percent_change_from_baseline',
                'grocery_and_pharmacy_percent_change_from_baseline'
                ]

    country_df = country_df.drop(list_mob, axis=1)
    
    country_df = country_df.set_index("date")
    
    median_df = country_df.median(axis=1)
    
    fig, ax1 = plt.subplots(figsize=(10,5))
    ax2 = ax1.twinx()
    sns.lineplot(data=median_df, ax=ax2, alpha=.5,color="green", label="Mobility")
    
    country_df = merged_df[merged_df["location"] == country].copy()
    
    country_df["stringency_50"] = np.divide(country_df["stringency_index"], 50)
    
    sns.lineplot(x="date", y="stringency_50", data=country_df, 
                 ax=ax1, color="blue", label="Stringency")
    sns.lineplot(x="date", y="reproduction_number", data=country_df, 
                 ax=ax1,color="red", label="R")
    ax1.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    ax2.set( ylabel='Mobility') 

    ax1.set(ylabel="Reproduction Number/Rescaled Stringency")
    plt.setp(ax1.get_xticklabels(), rotation=45)
    ax1.set_xticks(["2020-04-01",
                    "2020-05-01",
                    "2020-06-01",
                    "2020-07-01",
                    "2020-08-01",
                    "2020-09-01",
                    "2020-10-01",
                    "2020-11-01",
                    "2020-12-01",
                    "2021-01-01"])
    fig.tight_layout()
    plt.title(country)
    #plt.savefig('{}_R_stringency_mobility.png'.format(country),bbox_inches='tight')


##plt.show()