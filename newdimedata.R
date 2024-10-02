## update on 08/21/2024

# DIME 2000-2022
# FEMA web declaration areas
# CRED 2000-2022

# relevant codes are at 
# "~/Desktop/Yoon/7. 2021Fall/Dissertation Prosepctus/moneypaper/codes.R"
# "/Users/hyoon/Desktop/Yoon2/substitution/sub/sub.Rmd"
# "/Users/hyoon/Desktop/apsa_interdistrict/postapsa.Rmd"

library(tidyverse)
library(RSQLite)
library(sqldf)
library(data.table)
library(readxl)
library(reshape)

#-------------------------------------------------------
# DIME
#-------------------------------------------------------
# this is too big to run again
# sqlite file is in external hard drive (nano sd card)

# portaldb <- dbConnect(SQLite(), "/Users/hyoon/Desktop/Yoon2/substitution/dime 08:21:24/dime_v3_1.sqlite3")
# 
# # recipient
# cand <- dbGetQuery(portaldb, "SELECT election, cycle, fecyear, bonica_rid, bonica_cid, name, party, 
#                                      state, seat, district, ico_status, cand_gender, recipient_cfscore, contributor_cfscore, 
#                                      dwdime, dwnom1, dwnom2, ps_dwnom1, ps_dwnom2, 
#                                      num_givers, total_receipts, total_disbursements, total_indiv_contribs, total_unitemized, total_contribs_from_candidate, 
#                                     prim_vote_pct, pwinner, gen_vote_pct, gwinner, recipient_type 
#                               FROM candDB
#                               WHERE cycle >=2000 AND seat = 'federal:house' AND recipient_type = 'cand'")
# write.csv(cand, "cand.csv")
# 
# # contributor
# cont <- dbGetQuery(portaldb, "SELECT cycle, amount, date, bonica_cid, contributor_name, contributor_type, contributor_city,
#                                     contributor_state, contributor_zipcode, recipient_name, bonica_rid, recipient_party,
#                                     recipient_type, recipient_state, seat, election_type, contributor_district_90s, 
#                                     contributor_district_00s, contributor_district_10s, contributor_cfscore, candidate_cfscore
#                               FROM contribDB
#                               WHERE contributor_type = 'I' AND recipient_type = 'cand'
#                               AND seat = 'federal:house' AND election_type = 'P' AND cycle >= 2000")
# 
# cont <- cont %>% mutate(date = as.Date(cont$date),
#                         con_district = case_when(cont$cycle == 2000 ~ cont$contributor_district_90s,
#                                                  cont$cycle %in% c(2002, 2004, 2006, 2008, 2010) ~ cont$contributor_district_00s,
#                                                  cont$cycle %in% c(2012, 2014, 2016, 2018, 2020, 2022) ~ cont$contributor_district_10s)) %>%
#   select(-c(contributor_type, recipient_type, election_type, contributor_district_90s, contributor_district_00s, contributor_district_10s))
# 
# write.csv(cont, "cont.csv")

cand <- read.csv("/Users/hyoon/Desktop/Yoon2/substitution/dime 08:21:24/cand.csv") %>% select(-X)
cont <- read.csv("/Users/hyoon/Desktop/Yoon2/substitution/dime 08:21:24/cont.csv") %>% select(-c(X, X.1))

state_lookup <- state_lookup %>% mutate(state = tolower(state2abbr(State)))

## add recipient info to contributor data
join <- left_join(cont %>% select(cycle, amount, date, bonica_cid, contributor_city, contributor_state, 
                                  recipient_name, bonica_rid, recipient_party, recipient_state, 
                                  contributor_cfscore, candidate_cfscore, con_district), 
                  cand %>% select(cycle, bonica_rid, name, party, state, district, ico_status, recipient_cfscore,
                                  dwdime, dwnom1, dwnom2, ps_dwnom1, ps_dwnom2, 
                                  total_receipts, total_disbursements, total_indiv_contribs, total_unitemized, total_contribs_from_candidate,
                                  prim_vote_pct, pwinner, gen_vote_pct, gwinner),
                  by = c('cycle', 'bonica_rid')) %>%
  mutate(same = ifelse(.$con_district == .$district, 1, 0)) %>%
  filter(contributor_state %in% state_lookup$state) %>%
  filter(recipient_state %in% state_lookup$state)
#-------------------------------------------------------

#-------------------------------------------------------
# EM-DAT
#-------------------------------------------------------
emdat <- read_excel("/Users/hyoon/Desktop/Yoon2/substitution/sub/public_emdat_custom_request_2024-08-23_a6cf34e3-50c6-4a7e-82c2-6ac4c7f3abdb.xlsx") %>% 
  filter(Country == "United States of America") %>%
  select("DisNo.", "Disaster Subgroup", "Disaster Type", "Disaster Subtype", "Event Name", "Location", 
         "Start Year", "Start Month", "Start Day", "Total Deaths", "No. Injured", "No. Affected", "Total Affected",
         "Total Damage ('000 US$)", "Total Damage, Adjusted ('000 US$)") %>%
  mutate(DisNo. = gsub(".*[-]([^.]+)[-].*", "\\1", .$DisNo.)) 

# extract just states from location data

all_states <- append(state.name, "District of Columbia")

extract_states <- function(elem) {
  extracted_states <- c()
  while (TRUE) {
    state <- str_extract(elem, paste(all_states, collapse = "|"))
    if (is.na(state)) {
      break
    }
    extracted_states <- c(extracted_states, state)
    elem <- str_replace(elem, state, "")
  }
  extracted_states <- paste(extracted_states, collapse = ", ")
  return(extracted_states)
}

emdat <- emdat %>% mutate(Location = sapply(emdat$Location, extract_states)) %>%
  mutate(Location = strsplit(as.character(Location), ",")) %>% unnest(Location) %>%
  mutate(Location = trimws(.$Location)) %>%
  mutate(state = state.abb[match(.$Location, state.name)]) %>% distinct()
#-------------------------------------------------------


#-------------------------------------------------------
# FEMA
#-------------------------------------------------------
fema <- read.csv("/Users/hyoon/Desktop/Yoon2/substitution/sub/FemaWebDeclarationAreas.csv") %>% 
  mutate(designatedDate = as.Date(substr(.$designatedDate, 1, 10)),
         countycode = as.numeric(str_sub(.$placeCode, 3)),
         year = as.integer(substr(.$designatedDate, start=1, stop=4)),
         month = as.integer(substr(.$designatedDate, start=6, stop=7)),
         day = as.integer(substr(.$designatedDate, start=9, stop=10))) %>%
  mutate(countycode = sprintf("%03d", as.numeric(.$countycode))) %>% 
  filter(designatedDate >= "2000-01-01" & designatedDate < "2022-12-12") %>%
  select("disasterNumber", "programTypeDescription", "stateCode", "placeName", "designatedDate", "countycode", "year", "month", "day")
#-------------------------------------------------------


#-------------------------------------------------------
# Merge FEMA and EMDAT
#-------------------------------------------------------
# fema is county level
# emdat is state level, has disaster magnitude

disaster <- left_join(fema, emdat, 
                          by = c("stateCode"="state",
                                 "year"="Start Year",
                                 "month"="Start Month")) %>% 
  drop_na("DisNo.") 

# create variable that is the distance b/w the days
#  %>% mutate(diff = abs(.$"day" - .$"Start Day"))
# group by county, disaster
# keep one that minimizes that variable
# d <- disaster %>% group_by(stateCode, countycode, DisNo., year, month) %>% slice(which.min(diff))

# multiple disasters in state, county, year, month
  
disaster_day <- left_join(fema, emdat, 
                      by = c("stateCode"="state",
                             "year"="Start Year",
                             "month"="Start Month",
                             "day"="Start Day")) %>% drop_na("DisNo.") 
# rows with no DisNo means no match (doesn't exist in EM-DAT)

# there are cases with multiple disasters within a specific month+year for a state-county
# but needs to be aggregated to district level anyways so leaving as is for now
#-------------------------------------------------------


#-------------------------------------------------------
# Map counties to districts
#-------------------------------------------------------
# refer to disaster.rmd / postapsa.rmd
# used census data that maps county to districts
# merged with primary election dates 

# this only has until 2018 (cd 116)
cong <- read.csv("/Users/hyoon/Desktop/Yoon2/11. 2023Fall/apsa_interdistrict/apsadonation/cong.csv") %>% select(-X) %>%
  mutate_at(c("anl.begin", "pri"), funs(as.Date))

# cd117 2021-2023
library(sf)
library(usdata)

shapefile_data <- st_read("/Users/hyoon/Desktop/Yoon2/substitution/cb_2021_us_county_within_cd116_500k/cb_2021_us_county_within_cd116_500k.shp")

state_lookup <- data.frame(
  STATEFP = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
              "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
              "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
              "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
              "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", 
              "56"),
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
            "Colorado", "Connecticut", "Delaware", "District of Columbia", 
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
            "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
            "New Hampshire", "New Jersey", "New Mexico", "New York", 
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
            "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
            "Virginia", "Washington", "West Virginia", "Wisconsin", 
            "Wyoming")
)

pri117 <- read_excel("/Users/hyoon/Desktop/Yoon2/substitution/pridates117.xlsx") %>% 
  mutate(abb = state2abbr(.$state),
         date = as.Date(.$date))

cd117 <- merge(as.data.frame(st_drop_geometry(shapefile_data)), state_lookup, 
               by = "STATEFP", all.x = TRUE) %>% 
  mutate(abb = state2abbr(.$State),
         county = as.integer(.$COUNTYFP),
         cong = paste0(.$abb,.$CD116FP),
         district = as.integer(.$CD116FP),
         cd = "cd117",
         year = 2020,
         anl.begin = as.Date("2020-01-01")) %>%
  select(year, cd, abb, cong, district, county, anl.begin) %>%
  left_join(., pri117 %>% select(abb, date), by=c("abb" = "abb")) %>%
  mutate(pri = date) %>% select(-date) %>% drop_na(abb)

cong <- rbind(cong, cd117)
#-------------------------------------------------------


#-------------------------------------------------------
# Aggregate county level disaster data to district level
#-------------------------------------------------------
# disaster is county level
# cong is district level
# note there are multiple disasters within county, multiple disasters within district (of different counties)
# there are also cases without start dates
# when aggregating, which date to choose for the state level

trial <- left_join(cong %>% mutate(county = as.numeric(.$county),
                                   anl.begin = as.Date(.$anl.begin),
                                   pri = as.Date(.$pri)), 
                   disaster %>% mutate(countycode = as.numeric(.$countycode)),
                   by = c("county"="countycode",
                          "abb"="stateCode")) %>% 
  filter(anl.begin <= designatedDate & pri >= designatedDate) %>%
  mutate(close = .$pri - .$designatedDate) %>%
  distinct(county, abb, year.x, DisNo., .keep_all= TRUE)

# frequency? magnitude? add magnitude or average magnitude?
# just include close disasters (in terms of timing)?
# choose which date?
# can i say disaster in one county is disaster in the district? 

# magnitude, frequency
# add up the disaster damage and for disaster date choose the one closer to primary date
dis_sum <- trial %>% group_by(year.x, cong) %>% summarize(deaths = sum(`Total Deaths`),
                                                          affected = sum(`Total Affected`),
                                                          damages = sum(`Total Damage, Adjusted ('000 US$)`),
                                                          num = n()) 
dis_date <- trial %>% group_by(year.x, cong) %>% arrange(year.x, cong, close) %>% slice(1) %>% select(year.x, cong, close, pri)

dis <- left_join(dis_sum, dis_date, by = c("year.x"="year.x",
                                           "cong"="cong"))

# disaster within a year (close < 365)
dis_ysum <- trial %>% group_by(year.x, cong) %>% filter(close <= 300) %>% summarize(deaths = sum(`Total Deaths`),
                                                                                 affected = sum(`Total Affected`),
                                                                                 damages = sum(`Total Damage, Adjusted ('000 US$)`),
                                                                                 num = n()) 
dis_ydate <- trial %>% group_by(year.x, cong) %>% filter(close <=300) %>% 
  arrange(year.x, cong, close) %>% slice(1) %>% select(year.x, cong, close, pri)

  
dis_y <- left_join(dis_ysum, dis_ydate, by=c("year.x"="year.x",
                                             "cong"="cong"))
#-------------------------------------------------------


#-------------------------------------------------------
# Join disaster and dime
#-------------------------------------------------------
# group by contributor district or just district??
agg <- join %>% group_by(con_district, cycle, party) %>% drop_na(same) %>%
  summarize(count = n(),
            n.in = sum(same==1),
            n.out = sum(same==0),
            amt.in = sum(amount[same==1]),
            amt.out = sum(amount[same==0])) %>%
  mutate(pct.out = n.out/(n.in+n.out)*100,
         pct.amt.out = amt.out/(amt.in+amt.out)*100) %>% filter(!con_district %in% c("", "\\N"))

# dis, dis_y

agg_all <- left_join(agg, dis,
                     by = c("cycle"="year.x",
                            "con_district" = "cong")) %>%
  left_join(., dis_y, 
            by = c("cycle" = "year.x",
                   "con_district" = "cong")) 
#-------------------------------------------------------


#-------------------------------------------------------
# Coding disasters
#-------------------------------------------------------
# top 10% percentile
agg_all <- agg_all %>% mutate(nd_death = ifelse(deaths.x >= quantile(agg_all$deaths.x[is.na(agg_all$num.x)==F], 0.9, na.rm=T), 1, 0),
                              nd_affected = ifelse(affected.x >= quantile(agg_all$affected.x[is.na(agg_all$num.x)==F], 0.9, na.rm=T), 1, 0),
                              nd_damages = ifelse(damages.x >= quantile(agg_all$damages.x[is.na(agg_all$num.x)==F], 0.9, na.rm=T), 1, 0),
                              nd_num = ifelse(is.na(num.x)==T, 0, num.x),
                              nd_death_y = ifelse(deaths.y >= quantile(agg_all$deaths.y[is.na(agg_all$num.y)==F], 0.9, na.rm=T), 1, 0),
                              nd_affected_y = ifelse(affected.y >= quantile(agg_all$affected.y[is.na(agg_all$num.y)==F], 0.9, na.rm=T), 1, 0),
                              nd_damages_y = ifelse(damages.y >= quantile(agg_all$damages.y[is.na(agg_all$num.y)==F], 0.9, na.rm=T), 1, 0),
                              nd_num_y = ifelse(is.na(num.y)==T, 0, num.y)) %>%
  mutate(across(starts_with("nd"), ~replace_na(.x, 0)))
#-------------------------------------------------------


#-------------------------------------------------------
# Descriptive Stats
#-------------------------------------------------------

# trend in out-of-district donations

trend <- join %>% group_by(cycle, party) %>% drop_na(same) %>%
  summarize(count = n(),
            n.in = sum(same==1),
            n.out = sum(same==0),
            amt.in = sum(amount[same==1]),
            amt.out = sum(amount[same==0])) %>%
  mutate(pct.out = n.out/(n.in+n.out)*100,
         pct.amt.out = amt.out/(amt.in+amt.out)*100)

ggplot(trend %>% filter(party %in% c(100, 200)), aes(x=cycle, y=pct.out, color = factor(party))) + 
  geom_point() + geom_line() + ylim(0,100) + theme(legend.position="bottom") +
  xlab("") + ylab("% Out-of-District Donations") +
  scale_color_manual(values=c( "#56B4E9", "#E69F00"), name = "Party", labels = c("Dem", "Rep")) 

# contribution patterns of donors

pattern <- join %>% group_by(contributor_state) %>% filter(party %in% c(100,200)) %>% drop_na(same) %>%
  summarize(mean = mean(contributor_cfscore, na.rm=T),
            in_r = mean(contributor_cfscore[same==1 & party == 200], na.rm=T),
            out_r = mean(contributor_cfscore[same==0 & party == 200], na.rm=T),
            in_d = mean(contributor_cfscore[same==1 & party == 100], na.rm=T),
            out_d = mean(contributor_cfscore[same==0 & party == 100], na.rm=T)) %>%
  mutate(contributor_state = toupper(contributor_state)) %>%
  pivot_longer(cols = -contributor_state, names_to = "group", values_to = "cf_score") %>%
  mutate(party = case_when(group == "mean" ~ "mean",
                           group == "in_r" ~ "r",
                           group == "in_d" ~ "d",
                           group == "out_r" ~ "r",
                           group == "out_d" ~ "d"),
         source = case_when(group == "mean" ~ "mean",
                            group == "in_r" ~ "in",
                            group == "in_d" ~ "in",
                            group == "out_r" ~ "out",
                            group == "out_d" ~ "out"))

# mean_values <- pattern %>%
#   filter(party == "mean") %>%
#   group_by(contributor_state) %>%
#   summarise(mean_cf_score = mean(cf_score)) %>%
#   arrange(mean_cf_score)
# 
# pattern$contributor_state <- factor(pattern$contributor_state, 
#                                     levels = mean_values$contributor_state[order(mean_values$mean_cf_score)])

pattern$legend_group <- interaction(pattern$party, pattern$source)

ggplot(pattern, aes(x = cf_score, y = contributor_state, color = legend_group, shape = legend_group)) + 
  geom_point() + 
  scale_color_manual(values = c("mean.mean" = "darkgrey", 
                                "r.in" = "salmon3", 
                                "r.out" = "skyblue3", 
                                "d.in" = "salmon3", 
                                "d.out" = "skyblue3"),
                     labels = c("Dem (In)", "Rep (In)", "Mean", "Dem (Out)", "Rep (Out)")) + 
  scale_shape_manual(values = c(15, 17, 16, 15, 17),
                     labels = c("Dem (In)", "Rep (In)", "Mean", "Dem (Out)", "Rep (Out)")) + 
  labs(color = "", shape = "") +
  theme(legend.position="bottom") +
  xlab("CF Score") + 
  ylab("Contributor State")

# ggplot(pattern, aes(x=cf_score, y=contributor_state, color=factor(source), shape=factor(party))) + 
#   geom_point() + 
#   scale_color_manual(values = c("#1F77B4", "darkgrey", "#C8102E")) + 
#   scale_shape_manual(values = c(15, 16, 17)) + 
#   xlab("CF Score") + ylab("Contributor State")
#-------------------------------------------------------


#-------------------------------------------------------
# Analysis
#-------------------------------------------------------


#-------------------------------------------------------