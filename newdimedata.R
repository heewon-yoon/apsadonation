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
cong <- read.csv("/Users/hyoon/Desktop/Yoon2/11. 2023Fall/apsa_interdistrict/apsadonation/cong.csv") %>% select(-X)
#-------------------------------------------------------


#-------------------------------------------------------
# Aggregate county level disaster data to state level
#-------------------------------------------------------
# disaster is county level
# cong is district level
# note there are multiple disasters within county

trial <- left_join(cong %>% mutate(county = as.numeric(.$county),
                                   anl.begin = as.Date(.$anl.begin),
                                   pri = as.Date(.$pri)), 
                   disaster %>% mutate(countycode = as.numeric(.$countycode)),
                   by = c("county"="countycode",
                          "abb"="stateCode")) %>% 
  filter(anl.begin <= designatedDate & pri >= designatedDate) %>%
  mutate(close = .$pri - .$designatedDate) %>%
  distinct(county, abb, year.x, DisNo., .keep_all= TRUE)

#-------------------------------------------------------


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
#-------------------------------------------------------


