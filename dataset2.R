# Tom Alig
# COVID-19 Data
# March 27, 2020

library(tidyverse)
library(scales)
library(lubridate)
library(gridExtra)
#library(wppExplorer)
library(openintro)

source("my_covid_scripts.R")

# Turn off scientific notation
options(scipen=999)


#####
# Establish cutoff time of day
t <- Sys.time()
t2 <- lubridate::hour(t) + lubridate::minute(t)/60

# Data from Johns Hopkins are updated daily around 7:59pm eastern time
#  Set up date to be if time of day is less than 8:05pm eastern time, then use yesterday's date, else use today's date
use.this.date <- (ifelse(t2 < (20 + 5/60), (Sys.Date() - 1), Sys.Date()))
use.this.date <- as.Date(use.this.date, origin = "1970-01-01")

# Get the dates we want to scrape the correct data
dates <- seq(as.Date("2020-01-22"), use.this.date, by  = 1)

# Format the dates in the format of the html used by Johns Hopkins
dates2 <- format(dates, "%m-%d-%Y")

# Scrape the data
mytest <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', dates2, ".csv")

# Get data into a data frame
trythis <- lapply(mytest, read.csv)
cvdata <- do.call(bind_rows, trythis)

# copy the df
cvdata2 <- cvdata

# There have been various column names used by Johns Hopkins. Next step is to get them common
#  For province/state, country/region, Last Update, Lat, Long
cvdata2 <- cvdata2 %>% 
  mutate(PS = ifelse(is.na(Province_State), ?..Province.State, 
                      Province_State)) %>% 
  mutate(PS2 = ifelse(is.na(PS), Province.State, PS)) %>% 
  mutate(CR = ifelse(is.na(Country_Region), Country.Region, 
                     Country_Region)) %>% 
  mutate(LU = ifelse(is.na(Last_Update), Last.Update, 
                     Last_Update)) %>% 
  mutate(lats = ifelse(is.na(Latitude), Lat, 
                     Latitude),
       longs = ifelse(is.na(Longitude), Long_,
                      Longitude))

# copy the df
cvdata3 <- cvdata2

#cvdata3 %>% 
#  filter(Country.Reg == "US",
#         Prov.State == "King County, WA" | Prov.State == "Washington") %>% 
#
#  View()

# Now only keep the columns we want to keep
cvdata3 <- cvdata3 %>% 
  select(PS2, CR, Admin2, LU, Confirmed, Deaths, Recovered, lats, longs) 

# Rename most columns
cvdata3 <- cvdata3%>% 
  dplyr::rename(Prov.State = PS2,
         Country.Reg = CR,
         City = Admin2,
         Date = LU,
         Lat = lats,
         Long = longs) 

# copy the df
cvdata4 <- cvdata3

# Add new columns for Active cases
cvdata4 <- cvdata4 %>% 
  mutate(Active = Confirmed - Deaths - Recovered) 

# Separate info in Date column so we have a column with only date info
cvdata4 <- cvdata4 %>% 
  separate(Date, c("date", "other"), sep = "([\\ \\T])") %>% 
  select(-other)

#copy the df
cvdata5 <- cvdata4
dim(cvdata5)

# Get common date format for date column
cvdata5 <- cvdata5 %>% 
  mutate(date = ifelse(date == "1/22/2020", "2020-01-22", 
                       ifelse(date == "1/31/2020", "2020-01-31",
                              ifelse(date == "2/1/2020", "2020-02-01",
                                     ifelse(date == "3/8/2020", "2020-03-08",
                                            ifelse(date == "3/22/2020", "2020-03-22",
                                                   date))))))

# more dates that need to change
cvdata5.dates_need_to_change <- cvdata5 %>% 
  mutate(date = ifelse(date == "1/23/20", "2020-01-23",
                       ifelse(date == "1/24/20", "2020-01-24",
                              ifelse(date == "1/25/20", "2020-01-25",
                                     ifelse(date == "1/26/20", "2020-01-26",
                                            ifelse(date == "1/27/20", "2020-01-27",
                                                   ifelse(date == "1/28/20", "2020-01-28",
                                                          ifelse(date == "1/29/20", "2020-01-29",
                                                                 ifelse(date == "1/30/20", "2020-01-30",
                                                                        ifelse(date == "3/8/20", "2020-03-08",
                                                                               ifelse(date == "3/22/20", "2020-03-22",
                                                                                      ifelse(date == "2/23/20", "2020-02-23",
                                                                                             ifelse(date == "3/21/20", "2020-03-21",
                                                                                                    ifelse(date == "3/20/20", "2020-03-20",
                                                                                                           ifelse(date == "3/19/20", "2020-03-19",
                                                                                                                  ifelse(date == "3/18/20", "2020-03-18",
                                                                                                                         ifelse(date == "3/16/20", "2020-03-16",
                                                                                                                                ifelse(date == "3/14/20", "2020-03-14",
                                                                                                                                       ifelse(date == "3/13/20", "2020-03-13",
                                                                                                                                              ifelse(date == "3/12/20", "2020-03-12",
                                                                                                                                                     
                                                                        date)))))))))))))))))))) 
                            

# copy the df
cvdata6 <- cvdata5.dates_need_to_change

# South Korea data is dirty for March 13. This will fix
cvdata6 <- cvdata6 %>% 
  mutate(date = replace(date, 
                        Confirmed == 7979 & Deaths == 66 & Recovered == 510 & Country.Reg == "Korea, South",
                        "2020-03-13")) 

# change format of date to be Date instead of Character
cvdata6 <- cvdata6 %>% 
  mutate(newdate = as.Date(date, origin = "1970-01-01")) %>% 
  mutate(date = newdate) %>% 
  select(-newdate) 


# In Prov.State column, Johns Hopkins had originally included (city, state abbrev). They have since
#  changed the formatting. We need to get the formats to be common
cvdata6.us <- cvdata6 %>% 
  filter(Country.Reg == "US") %>% 
  mutate(PS2 = Prov.State) %>% 
  separate(Prov.State, c("blah", "blah2"), sep = "([\\,])") %>% 
  mutate(blah2.5 = trimws(blah2, "l")) %>% 
  mutate(blah4 = abbr2state(blah2.5)) %>% 
  mutate(PS3 = ifelse(is.na(blah4), PS2, blah4),
         State = PS3) %>% 
  select(State, Country.Reg, City, date, Confirmed, Deaths, Recovered, Lat, Long,
         Active) 

# Find most recent date in data set
maxdate <- max(cvdata6$date)

# Set up US data frame by state
bystate <- cvdata6.us %>% 
  filter(Country.Reg == "US") %>% 
  group_by(State, date) %>% 
  summarize(us.conf = sum(Confirmed),
            us.death = sum(Deaths),
            us.rec = sum(Recovered),
            us.act = sum(Active)) %>% 
  arrange(desc(us.conf), .by_group = TRUE) %>% 
  ungroup()

curr.us.counts <- bystate %>% 
  filter(date == maxdate) %>% 
  summarize(tot.conf = sum(us.conf),
            tot.deaths = sum(us.death),
            tot.rec = sum(us.rec),
            tot.active = sum(us.act))

curr.us.counts

curr.us.death.rate <- round(((curr.us.counts$tot.deaths/curr.us.counts$tot.conf) * 100), 2)


bystate %>% 
  filter(State == "Washington") %>% 
  View()

# Johns Hopkins originally set up "South Korea" as a nation. They then changed
#  the nomenclature to "Korea, South". Need to get this to be common
cvdata6 <- cvdata6 %>% 
  mutate(c2 = ifelse(Country.Reg == "Korea, South", "South Korea", Country.Reg)) %>%
  select(-Country.Reg) %>% 
  mutate(Country.Reg = c2) %>% 
  select(-c2) %>% 
  select(Country.Reg, everything()) 

# Combine Mainland China and China
cvdata6 <- cvdata6 %>% 
  mutate(cr2 = ifelse(Country.Reg == "China" | Country.Reg == "Mainland China", "China",
                      Country.Reg)) %>% 
  mutate(Country.Reg = cr2) %>% 
  select(-cr2) %>% 
  select(Country.Reg, everything()) 

# Set up worldwide data frame by country
bycountry <- cvdata6 %>% 
  group_by(Country.Reg, date) %>% 
  summarize(ww.conf = sum(Confirmed),
            ww.death = sum(Deaths),
            ww.rec = sum(Recovered),
            ww.act = sum(Active)) %>% 
  arrange(desc(ww.conf), .by_group = TRUE) %>% 
  ungroup() 

bycountry %>% 
  filter(Country.Reg == "South Korea") %>% 
  View()

curr.ww.counts <- bycountry %>% 
  filter(date == maxdate) %>% 
  summarize(tot.conf = sum(ww.conf),
            tot.deaths = sum(ww.death),
            tot.rec = sum(ww.rec),
            tot.active = sum(ww.act))

curr.ww.counts

curr.ww.death.rate <- round(((curr.ww.counts$tot.deaths/curr.ww.counts$tot.conf) * 100), 2)


#################
################
# US States
# |||||
# VVVVV

# Selected States
mystates <- bystate %>% 
  filter(State == "Michigan" | State ==  "Ohio" | State == "Texas" |
           State == "Wisconsin" | State == "California" | State == "New York" |
           State == "Washington") 

# Add population figures, and per capita and per 1mm info, into the data frame
mystates <- mystates %>% 
  mutate(pop = ifelse(State == "Michigan", michpop,
                      ifelse(State == "Ohio", ohpop,
                             ifelse(State == "Texas", txpop,
                                    ifelse(State == "Wisconsin", wipop,
                                           ifelse(State == "California", capop,
                                                  ifelse(State == "New York", nypop,
                                                         wapop))))))) %>% 
  mutate(per.cap.conf = us.conf/pop,
         conf.per1mm = per.cap.conf * mill,
         per.cap.death = us.death/pop,
         deaths.per1mm = per.cap.death * mill,
         per.cap.act = us.act/pop,
         active.per1mm = per.cap.act * mill,
         per.cap.rec = us.rec/pop,
         recov.per1mm = per.cap.rec * mill)

#####
# Confirmed cases, selected US states
# per million
graph.conf.us.mil <- graph.my.data.per1mm(mystates, 
              mystates$conf.per1mm, 
              mystates$State,
              "Confirmed")

graph.conf.us.mil

# total pop
graph.conf.us.raw <- graph.my.data.raw(mystates, 
                                          mystates$us.conf, 
                                          mystates$State,
                                          "Confirmed")

graph.conf.us.raw


#####
# Deaths cases, selected US states
graph.death.us.mil <- graph.my.data.per1mm(mystates, 
                                      mystates$deaths.per1mm, 
                                      mystates$State,
                                      "Death")

graph.death.us.mil

# total pop
graph.death.us.raw <- graph.my.data.raw(mystates, 
                                       mystates$us.death, 
                                       mystates$State,
                                       "Death")

graph.death.us.raw

#########
# Active cases, selected US states
graph.active.us.mil <- graph.my.data.per1mm(mystates, 
                                          mystates$active.per1mm, 
                                          mystates$State,
                                          "Active")

graph.active.us.mil

# total pop
graph.active.us.raw <- graph.my.data.raw(mystates, 
                                        mystates$us.act, 
                                        mystates$State,
                                        "Active")

graph.active.us.raw

#######
# Recovered cases, selected US states
graph.rec.us.mil <- graph.my.data.per1mm(mystates, 
                                            mystates$recov.per1mm, 
                                            mystates$State,
                                            "Recovered")

graph.rec.us.mil

# total pop
graph.rec.us.raw <- graph.my.data.raw(mystates, 
                                         mystates$us.rec, 
                                         mystates$State,
                                         "Recovered")

graph.rec.us.raw

# ^^^^^
# |||||
# US States
#################
################
#################
################
# Selected Countries
# |||||
# VVVVV

# Italy data very dirty
# This is attempt to clean
bycountry <- bycountry %>% 
  mutate(ww.conf = replace(ww.conf, date == "2020-03-11" & Country.Reg == "Italy", 12462),
         ww.conf = replace(ww.conf, date == "2020-03-14" & Country.Reg == "Italy", 21157),
         ww.rec = replace(ww.rec, date == "2020-03-11" & Country.Reg == "Italy", 1045),
         ww.rec = replace(ww.rec, date == "2020-03-14" & Country.Reg == "Italy", 1966),
         ww.death = replace(ww.death, date == "2020-03-11" & Country.Reg == "Italy", 827),
         ww.death = replace(ww.death, date == "2020-03-14" & Country.Reg == "Italy", 1441)) %>%
  mutate(ww.act2 = ww.conf - ww.death - ww.rec) %>% 
  mutate(ww.act = ww.act2) %>% 
  select(-ww.act2) 
                        

# Selected Countries
mycountry <- bycountry %>% 
  filter(Country.Reg == "Italy" | Country.Reg ==  "France" | Country.Reg == "Germany" |
           Country.Reg == "China" | Country.Reg == "US" | Country.Reg == "South Korea" |
           Country.Reg == "Spain") 



# Add population figures, and per capita and per 1mm info, into the data frame
mycountry <- mycountry %>% 
  mutate(pop = ifelse(Country.Reg == "Italy", italypop,
                      ifelse(Country.Reg == "France", frpop,
                             ifelse(Country.Reg == "Germany", gerpop,
                                    ifelse(Country.Reg == "China", chpop,
                                           ifelse(Country.Reg == "US", uspop,
                                                  ifelse(Country.Reg == "South Korea", skpop,
                                                         spainpop))))))) %>% 
  mutate(ww.per.cap.conf = ww.conf/pop,
         ww.conf.per1mm = ww.per.cap.conf * mill,
         ww.per.cap.death = ww.death/pop,
         ww.deaths.per1mm = ww.per.cap.death * mill,
         ww.per.cap.act = ww.act/pop,
         ww.active.per1mm = ww.per.cap.act * mill,
         ww.per.cap.rec = ww.rec/pop,
         ww.recov.per1mm = ww.per.cap.rec * mill)

#####
# Confirmed cases, selected countries
# per million
graph.conf.ww.mil <- graph.my.data.per1mm(mycountry, 
                                          mycountry$ww.conf.per1mm, 
                                          mycountry$Country.Reg,
                                          "Confirmed")

graph.conf.ww.mil

# total pop
graph.conf.ww.raw <- graph.my.data.raw(mycountry, 
                                       mycountry$ww.conf, 
                                       mycountry$Country.Reg,
                                       "Confirmed")

graph.conf.ww.raw

####
# Death cases, selected countries
# per million
graph.death.ww.mil <- graph.my.data.per1mm(mycountry, 
                                          mycountry$ww.deaths.per1mm, 
                                          mycountry$Country.Reg,
                                          "Death")

graph.death.ww.mil

# total pop
graph.death.ww.raw <- graph.my.data.raw(mycountry, 
                                       mycountry$ww.death, 
                                       mycountry$Country.Reg,
                                       "Death")

graph.death.ww.raw

# ^^^^^
# |||||
# /Selected Countries

# next steps
# shiny dashboard
# clean up italy and china numbers around March 8-15
# clean up state of WA numbers on March 8, 9


#################
################
#########################
#########################
