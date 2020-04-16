# Tom Alig
# COVID-19 Data
# March 27, 2020

library(tidyverse)
library(scales)
library(lubridate)
library(gridExtra)
library(openintro)

source("my_covid_scripts.R")

# Turn off scientific notation
options(scipen=999)

#####
# Establish cutoff time of day
#t <- Sys.time()
#t1.5 <- with_tz(t, "UTC")
#gmt.date <- as.Date(t1.5, origin = "1970-01-01", tz = "GMT")

# Data from Johns Hopkins are updated daily around 7:59pm eastern time
#  This is equiv to 11:59pm GMT (during daylight savings time)

gmt.date2 <- as.Date(now("UTC"))
use.this.date <- gmt.date2 - 1
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

str(cvdata4$date)

cvdata4x <- cvdata4


# Dates from Johns Hopkins are in a variety of formats and are not consistently
#   formatted from one day to the next.
#    This will get the dates to be in consistent format

cvdata4x <- cvdata4x %>% 
  mutate(date2 = date,
         date3 = date) %>% 
  separate(date, c("date", "other2", "other3"), sep = "([\\-])") %>%  # winner winner
  separate(date, c("blah1", "blah2", "blah3"), sep = "([\\/])") %>%  # winner winner
  mutate(newmonth = ifelse(blah1 == "2020", other2, blah1)) %>% 
  mutate(newmonth2 = ifelse(is.na(newmonth), blah1, newmonth)) %>% 
  #mutate(newmonth2 = ifelse(newmonth == other2, newmonth, blah1)) %>% 
  #mutate(newday = ifelse(blah2 == "2020", other3, blah2)) %>% 
  mutate(newday = ifelse(is.na(other3), blah2, other3)) %>% 
  #mutate(newday2 = ifelse(is.na(newday), blah2, newday)) %>% 
  select(date2, newmonth2, newday, other2, other3, blah1, blah2, blah3, everything())

# Convert newmonth2 and newday to numeric
cvdata4x$newmonth2 <- as.numeric(as.character(cvdata4x$newmonth2))
cvdata4x$newday <- as.numeric(as.character(cvdata4x$newday))

# verify no NA for month and day
anyNA(cvdata4x$newmonth2)
anyNA(cvdata4x$newday)

# First get month and day to be two digits each
#   Then create new date column
cvdata4x <- cvdata4x %>% 
  mutate(blah11 = sprintf("%02d", newmonth2)) %>% 
  mutate(blah12 = sprintf("%02d", newday)) %>% 
  mutate(newdate99 = paste("2020",
                           blah11,
                           blah12,
                           sep = "-")) %>% 
  select(newdate99, blah11, newmonth2, blah12, newday, everything())

# copy df
cvdata4y <- cvdata4x

names(cvdata4y)

# Keep appropriate columns
cvdata4y <- cvdata4y %>% 
  mutate(date = newdate99) %>% 
  select(Prov.State, Country.Reg, City, date, Confirmed, Deaths,
         Recovered, Active, Lat, Long)

#copy the df
cvdata5 <- cvdata4y
dim(cvdata5)

# copy the df
cvdata6 <- cvdata5

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
  select(State, Country.Reg, City, date, Confirmed, Deaths, Recovered, Active, 
         Lat, Long)

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
# worldwide data EXCLUDING USA
ww.not.us <- cvdata6 %>% 
  filter(Country.Reg != "US") %>% 
  group_by(date) %>% 
  summarize(ww.conf = sum(Confirmed),
            ww.death = sum(Deaths),
            ww.rec = sum(Recovered),
            ww.act = sum(Active)) %>% 
  mutate(region = "rest.of.world") %>% 
  gather(type, cases, ww.conf:ww.death) 

# USA only data
us.only <- cvdata6 %>% 
  filter(Country.Reg == "US") %>% 
  group_by(date) %>% 
  summarize(usa.conf = sum(Confirmed),
            usa.death = sum(Deaths),
            usa.rec = sum(Recovered),
            usa.act = sum(Active)) %>% 
  mutate(region = "USA") %>% 
  gather(type, cases, usa.conf:usa.death)

# Merge worldwide NOT USA, and USA-only
mynewdf <- bind_rows(ww.not.us, us.only)

# Overall Confirmed Cases
overall.confirm.graph <- mynewdf %>% 
  filter(type == "ww.conf" | type == "usa.conf") %>% 
  ggplot(aes(x = date, y = cases, na.rm = TRUE,
           color = region,
           group = region
)) +
  geom_point(size = 3) + 
  geom_line(size = 2) +
  scale_colour_manual(values = c(cb.blue, cb.orange)) +
  scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
  scale_y_continuous(labels = comma) +
  xlab("") +
  ylab("Total") +
  theme(panel.grid.minor = element_blank()) +
  guides(color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  labs(title = "Worldwide Total Number of COVID-19 Confirmed Cases") +
  theme(plot.title = element_text(size = 16))

overall.death.graph <- mynewdf %>% 
  filter(type == "ww.death" | type == "usa.death") %>% 
  ggplot(aes(x = date, y = cases, na.rm = TRUE,
             color = region,
             group = region
  )) +
  geom_point(size = 3) + 
  geom_line(size = 2) +
  scale_colour_manual(values = c(cb.blue, cb.orange)) +
  scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
  scale_y_continuous(labels = comma) +
  xlab("") +
  ylab("Total") +
  theme(panel.grid.minor = element_blank()) +
  guides(color = guide_legend(title = NULL)) +
  theme_bw() +
  theme(legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  labs(title = "Worldwide Total Number of COVID-19 Deaths") +
  theme(plot.title = element_text(size = 16))

#################
################
# US States
# |||||
# VVVVV

# Selected States
mystates <- bystate %>% 
  filter(State == "Michigan" | State ==  "Ohio" | State == "Texas" |
           State == "Wisconsin" | State == "California" | #State == "New York" |
           State == "Washington" | State == "Florida") 

# Add population figures, and per capita and per 1mm info, into the data frame
mystates <- mystates %>% 
  mutate(pop = ifelse(State == "Michigan", michpop,
                      ifelse(State == "Ohio", ohpop,
                             ifelse(State == "Texas", txpop,
                                    ifelse(State == "Wisconsin", wipop,
                                           ifelse(State == "California", capop,
                                                  ifelse(State == "Florida", flpop,
                                                  #ifelse(State == "New York", nypop,
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



#################
################
#########################
#########################
