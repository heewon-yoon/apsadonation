## update on 08/21/2024

# DIME 2000-2022
# FEMA web declaration areas
# CRED 2000-2022

# relevant codes are at 
# "~/Desktop/Yoon/7. 2021Fall/Dissertation Prosepctus/moneypaper/codes.R"
# "/Users/hyoon/Desktop/Yoon2/substitution/sub/sub.Rmd"
# "/Users/hyoon/Desktop/apsa_interdistrict/postapsa.Rmd" --> only with data up until 2016

library(tidyverse)
library(RSQLite)
library(sqldf)
library(data.table)
library(readxl)
library(reshape)
library(usdata)

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

## descriptive stats

# across states

emdat_sum <- emdat %>% group_by(state) %>% summarize(n = n(),
                                                     n_dist = n_distinct(DisNo.),
                                                     tot_aff = sum(`Total Affected`, na.rm=T),
                                                     tot_aff_log = log(tot_aff),
                                                     .groups = "drop") %>%
  mutate(state = tolower(state.name[match(state, state.abb)]))

states_map <- map_data("state")

map_data <- states_map %>%
  left_join(emdat_sum, by = c("region" = "state"))

plot_emdat <- function(data, fill_var, fill_label = "Count", title = "Case Counts by State") {
  # Ensure fill_var is treated as a symbol for aes()
  fill_sym <- rlang::sym(fill_var)
  
  ggplot(data, aes(x = long, y = lat, group = group, fill = !!fill_sym)) +
    geom_polygon(color = "white") +
    coord_fixed(1.3) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
    theme_void() +
    labs(fill = fill_label, title = title)
}

plot_emdat(map_data, fill_var = "n", fill_label = "EMDAT Cases", title = "EMDAT Case Counts by State")
plot_emdat(map_data, fill_var = "n_dist", fill_label = "EMDAT Cases", title = "EMDAT Case Counts by State (distinct cases)")
plot_emdat(map_data, fill_var = "tot_aff", fill_label = "EMDAT Cases", title = "EMDAT Case Counts by State (by magnitude)")
plot_emdat(map_data, fill_var = "tot_aff_log", fill_label = "EMDAT Cases", title = "EMDAT Case Counts by State (by logged magnitude)")


# across years

emdat_year <- emdat %>% group_by(`Start Year`) %>% summarize(n = n(),
                                                             n_dist = n_distinct(DisNo.),
                                                             .groups = "drop")

ggplot(emdat_year, aes(x=`Start Year`, y=n)) + geom_point() + geom_line()
ggplot(emdat_year, aes(x=`Start Year`, y=n_dist)) + geom_point() + geom_line()

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

## descriptive stats

## across states
fem_sum <- fema %>% group_by(stateCode) %>% summarize(n = n(),
                                                      n_dist = n_distinct(disasterNumber),
                                                      .groups = "drop") %>%
  mutate(stateCode = tolower(state.name[match(stateCode, state.abb)])) %>% arrange(desc(n))

map_data_joined <- states_map %>%
  left_join(fem_sum, by = c("region" = "stateCode"))

# all
ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  theme_void() +
  labs(fill = "FEMA Cases", title = "FEMA Case Counts by State") 

# distinct disasters
ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = n_dist)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  theme_void() +
  labs(fill = "FEMA Cases", title = "FEMA Case Counts by State") 

# by county

county_map <- map_data("county")

fema_county <- fema %>% mutate(stateCode = tolower(state.name[match(stateCode, state.abb)]),
                               county = tolower(gsub("\\s*\\([^\\)]+\\)", "", placeName))) %>%
  group_by(stateCode, county) %>% summarize(n = n(),
                                            n_distinct = n_distinct(disasterNumber))

map_data_county <- county_map %>%
  left_join(fema_county, by = c("region" = "stateCode", "subregion" = "county"))

ggplot(map_data_county, aes(long, lat, group = group, fill = n)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(fill = "Total Count") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  theme(legend.position = "bottom")

ggplot(map_data_county, aes(long, lat, group = group, fill = n_distinct)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(fill = "Distinct Count") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  theme(legend.position = "bottom")

## across years

# all
fema %>% group_by(year) %>% summarize(n = n(), .groups = "drop") %>%
  ggplot(aes(x=year, y=n)) + geom_point() + geom_line()

# unique disasters
fema %>% group_by(year) %>% summarize(n = n_distinct(disasterNumber), .groups = "drop") %>%
  ggplot(aes(x=year, y=n)) + geom_point() + geom_line()

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
# Aggregate county level disaster data to district level: collapsed
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

collapsed <- trial %>%
  group_by(year = year.y, cong) %>%
  summarise(
    unique_disasters = n_distinct(disasterNumber),
    total_rows       = n(),
    total_deaths     = sum(`Total Deaths`, na.rm = TRUE),
    total_damage     = sum(`Total Damage, Adjusted ('000 US$)`, na.rm = TRUE),
    total_affected   = sum(`Total Affected`, na.rm = TRUE),
    close_avg        = mean(close, na.rm = TRUE),
    close_min        = min(close, na.rm = TRUE),
    close_max        = max(close, na.rm = TRUE),
    .groups = "drop"
  )
#-------------------------------------------------------


#-------------------------------------------------------
# Join disaster and dime
#-------------------------------------------------------
# join is contributor level data
# need to aggregate to congressional district level
donor_sum <- join %>%
  filter(!is.na(same), district != "") %>%
  group_by(district, cycle, party) %>%
  summarise(
    n_total   = n(),
    n_in      = sum(same == 1),                  
    n_out     = sum(same == 0),
    amt_in    = sum(amount[same == 1], na.rm = TRUE),
    amt_out   = sum(amount[same == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    amt_total   = amt_in + amt_out,
    pct_in_cnt  = if_else(n_total > 0, 100 * n_in  / n_total, NA_real_),
    pct_out_cnt = if_else(n_total > 0, 100 * n_out / n_total, NA_real_),
    pct_in_amt  = if_else(amt_total > 0, 100 * amt_in  / amt_total, NA_real_),
    pct_out_amt = if_else(amt_total > 0, 100 * amt_out / amt_total, NA_real_)
  )

donor <- donor_sum %>% filter(party %in% c(100,200)) %>% filter(cycle > 2000, cycle < 2020) %>%
  left_join(collapsed,
            by = c("cycle" = "year",
                   "district" = "cong")) 
# collapsed (disaster) missing years: 2000, 2019
# donor_sum (contribution) has 2000-2022
#-------------------------------------------------------


#-------------------------------------------------------
# Examine disaster trend
#-------------------------------------------------------
summary_long <- donor %>%
  group_by(cycle) %>%
  summarise(
    across(
      c(unique_disasters, total_rows, total_deaths, total_damage,
        total_affected, close_avg, close_min, close_max),
      list(sum = ~sum(.x, na.rm = TRUE),
           mean = ~mean(.x, na.rm = TRUE)),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"
  ) %>%
  # pivot BOTH sum_ and mean_ totals and parse into stat + metric
  pivot_longer(
    cols = c(starts_with("sum_total"), starts_with("mean_total")),
    names_to = c("stat", "metric"),
    names_pattern = "(sum|mean)_total_(.*)",
    values_to = "value"
  ) %>%
  mutate(
    stat   = ifelse(stat == "sum", "Sum", "Mean"),
    metric = dplyr::recode(metric,
                           rows = "Rows",
                           deaths = "Deaths",
                           affected = "Affected",
                           damage = "Damages",
                           .default = metric)
  )

# trend by cycle/ sum, mean
ggplot(summary_long, aes(x = cycle, y = value, color = stat, group = stat)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Trends by Cycle: Sum vs Mean",
    x = "Cycle",
    y = "Value",
    color = ""
  )

# mean
ggplot(summary_long %>% filter(stat == "Mean"), aes(x = cycle, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Trends by Cycle: Mean",
    x = "Cycle",
    y = "Value"
  )

# sum
ggplot(summary_long %>% filter(stat == "Sum"), aes(x = cycle, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Trends by Cycle: Sum",
    x = "Cycle",
    y = "Value"
  )
#-------------------------------------------------------


#-------------------------------------------------------
# Descriptive Stats
#-------------------------------------------------------

## trend in out-of-district donations

trend <- join %>% filter(!is.na(same), district != "", party %in% c(100,200)) %>%
  group_by(cycle, party) %>% 
  summarize(n_total = n(),
            n_in = sum(same==1),
            n_out = sum(same==0),
            amt_in = sum(amount[same==1], na.rm=T),
            amt_out = sum(amount[same==0], na.rm=T),
            .groups = "drop") %>%
  mutate(amt_total   = amt_in + amt_out,
         pct_in_cnt  = if_else(n_total > 0, 100 * n_in  / n_total, NA_real_),
         pct_out_cnt = if_else(n_total > 0, 100 * n_out / n_total, NA_real_),
         pct_in_amt  = if_else(amt_total > 0, 100 * amt_in  / amt_total, NA_real_),
         pct_out_amt = if_else(amt_total > 0, 100 * amt_out / amt_total, NA_real_))

plot_data <- trend %>%
  mutate(
    party_label = dplyr::recode(
      party,
      `100` = "Democrats",
      `200` = "Republicans"
    )
  ) %>%
  pivot_longer(
    cols = c(pct_out_cnt, pct_out_amt),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = dplyr::recode(
      measure,
      pct_out_cnt = "Count",
      pct_out_amt = "Amount"
    )
  )

trend <- ggplot(plot_data, aes(x = cycle, y = value,
                               color = party_label, linetype = measure)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  scale_color_manual(
    values = c("Democrats" = "#4A90E2",  # blue-ish
               "Republicans" = "#C44E52") # red-ish
  ) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  labs(
    #    title = "Outside-District Donations by Party",
    #    subtitle = "Count share vs Amount share",
    x = "Cycle",
    y = "Percent",
    color = "Party",
    linetype = "Out-of-District"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

trend_pattern <- ggplot(trend, aes(x=cycle, y=pct_out_cnt, color = factor(party))) + 
  geom_point() + 
  geom_line() + 
  ylim(0,100) + 
  xlab("") + 
  ylab("% Out-of-District Donations") +
  scale_color_manual(values=c("#4A90E2", "#C44E52"), 
                     name = "Party",
                     labels = c("Dem", "Rep")) +
  scale_x_continuous(breaks = unique(trend$cycle)) +
  theme_minimal() + 
  theme(legend.position="bottom")

pdf("/Users/hyoon/Desktop/Yoon2/11. 2023Fall/apsa_interdistrict/apsadonation/pattern.pdf", width = 8, height = 6)
print(trend) 
dev.off()
#-------------------------------------------------------


#-------------------------------------------------------
# Coding disasters: donornew
#-------------------------------------------------------
# binary: whether disaster happened or not, top 10 percentile
# intensity: count, severity (log transformed)

donornew <- donor %>%
  # 1) Recode NA -> 0 for disaster metrics + exposure flag + logs
  mutate(
    nd_count   = if_else(is.na(unique_disasters), 0, unique_disasters),
    nd_deaths  = if_else(is.na(total_deaths),   0, total_deaths),
    nd_damage  = if_else(is.na(total_damage),   0, total_damage),
    nd_affect  = if_else(is.na(total_affected), 0, total_affected),
    
    nd_any         = as.integer(nd_count > 0),  # any disaster exposure
    nd_deaths_log  = log1p(nd_deaths),
    nd_damage_log  = log1p(nd_damage),
    nd_affected_log= log1p(nd_affect)
  ) %>%
  # 2) Within-cycle Top 5% thresholds (exclude zeros), then flags
  group_by(cycle) %>%
  mutate(
    thr5_count   = {x <- nd_count [nd_count  > 0]; if (length(x)) quantile(x, 0.95, na.rm=TRUE) else Inf},
    thr5_deaths  = {x <- nd_deaths[nd_deaths > 0]; if (length(x)) quantile(x, 0.95, na.rm=TRUE) else Inf},
    thr5_damage  = {x <- nd_damage[nd_damage > 0]; if (length(x)) quantile(x, 0.95, na.rm=TRUE) else Inf},
    thr5_affect  = {x <- nd_affect[nd_affect > 0]; if (length(x)) quantile(x, 0.95, na.rm=TRUE) else Inf},
    
    nd_top5_count    = as.integer(nd_count   > thr5_count),
    nd_top5_deaths   = as.integer(nd_deaths  > thr5_deaths),
    nd_top5_damage   = as.integer(nd_damage  > thr5_damage),
    nd_top5_affected = as.integer(nd_affect  > thr5_affect)
  ) %>%
  ungroup() %>%
  select(-starts_with("thr5_"))   # drop thresholds, keep flags
#-------------------------------------------------------


#-------------------------------------------------------
# Analysis: first stage
#-------------------------------------------------------
# agg_all is district-cycle-party level (with disaster variables)
# join is contributor level
library(fixest)

## effect of ND on donor composition
# in: negative
# out: indifferent
# share of out: positive

library(purrr)
library(broom)

# ---- 1) Core runner: Y × X grid of feols models ----
run_feols_grid <- function(data, ys, xs,
                           controls = NULL,
                           fe = "district^party + cycle^party",
                           cluster = ~district) {
  # Build all (y, x) pairs
  combos <- expand.grid(y = ys, x = xs, stringsAsFactors = FALSE)
  
  models <- pmap(combos, function(y, x) {
    rhs_terms <- c(x, controls) %>% discard(~ is.null(.x) || .x == "")
    rhs <- if (length(rhs_terms)) paste(rhs_terms, collapse = " + ") else "1"
    
    fml <- as.formula(paste0(y, " ~ ", rhs, " | ", fe))
    fixest::feols(fml, data = data, cluster = cluster)
  })
  
  # Name models like "y ~ x"
  names(models) <- paste(combos$y, combos$x, sep = " ~ ")
  models
}

# ---- 2) Convenience: etable of all models (keep only X terms) ----
etable_grid <- function(models, keep_terms = NULL) {
  if (is.null(keep_terms)) {
    # default: keep all RHS terms that appear in the names after "~ "
    keep_terms <- unique(sub(".*~\\s*", "", names(models)))
  }
  fixest::etable(models, keep = keep_terms, digits = 3)
}

# ---- 3) Convenience: tidy coefficients for all models into one tibble ----
tidy_grid <- function(models) {
  imap_dfr(models, ~ broom::tidy(.x) %>%
             mutate(model = .y)) %>%
    # add parsed Y and X columns from model name "Y ~ X"
    tidyr::separate(model, into = c("Y", "X"), sep = "\\s*~\\s*", remove = FALSE)
}


# ---- 4) Coefficient Plot
# 1) Tidy all models with 95% CIs
tidy_grid_ci <- function(models) {
  imap_dfr(models, ~ broom::tidy(.x, conf.int = TRUE, conf.level = 0.95) %>%
             mutate(model = .y)) %>%
    tidyr::separate(model, into = c("Y", "X"), sep = "\\s*~\\s*", remove = FALSE)
}

# 2) Keep only the coefficients for your X terms and add significance labels
prep_coef_table <- function(models, xs) {
  tidy_grid_ci(models) %>%
    filter(term %in% xs) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "p < .001",
        p.value < 0.01  ~ "p < .01",
        p.value < 0.05  ~ "p < .05",
        p.value < 0.10  ~ "p < .10",
        TRUE            ~ "n.s."
      ),
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ "•",
        TRUE            ~ ""
      )
    )
}

# 3) Plot: one facet per outcome (Y), y-axis = predictor (term), x = estimate
plot_coef <- function(models, xs, y_lab = "Predictor (X)", x_lab = "Estimate") {
  df <- prep_coef_table(models, xs)
  
  ggplot(df, aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high,
                       alpha = sig), height = 0) +
    geom_point(aes(shape = sig), size = 2.4) +
    # (optional) add tiny star labels at the point
    geom_text(aes(label = stars), nudge_x = 0, nudge_y = 0.18, size = 3) +
    facet_wrap(~ Y, scales = "free_y") +
    scale_alpha_manual(values = c("p < .001" = 1, "p < .01" = 0.95,
                                  "p < .05" = 0.9, "p < .10" = 0.75,
                                  "n.s." = 0.5), guide = "none") +
    scale_shape_manual(values = c("p < .001" = 16, "p < .01" = 16,
                                  "p < .05" = 17, "p < .10" = 18,
                                  "n.s." = 1), name = "Significance") +
    labs(x = x_lab, y = y_lab, title = "Coefficient Plot with 95% CIs") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}


# Outcomes 
ys <- c(
  "n_in", "n_out",
  "amt_in", "amt_out",
  "pct_in_cnt", "pct_out_cnt",
  "pct_in_amt", "pct_out_amt")

# Predictors 
xs <- c("nd_count", "nd_any", "nd_deaths", "nd_damage", "nd_affect",       
        "nd_deaths_log", "nd_damage_log", "nd_affected_log",
        "nd_top5_count", "nd_top5_deaths", "nd_top5_damage", "nd_top5_affected")

# Controls
controls <- c()  


## Interaction FE (district x party + cycle x party)

# Run models on donornew
models <- run_feols_grid(
  data = donornew,
  ys   = ys,
  xs   = xs,
  controls = controls,
  fe = "district^party + cycle^party",
  cluster = ~district
)

# table
etable_grid(models, keep_terms = xs)

# tidy df
coef_tbl <- tidy_grid(models) %>%
  mutate(
    signif = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""))

plot_coef(models, xs)

## Normal FE (district + party + cycle)
models_norm <- run_feols_grid(
  data = donornew,
  ys   = ys,
  xs   = xs,
  controls = controls,
  fe = "district + party + cycle",
  cluster = ~district
)

etable_grid(models_norm, keep_terms = xs)

plot_coef(models_norm, xs)

## Normal FE (before 2016)
models_norm16 <- run_feols_grid(
  data = donornew %>% filter(cycle < 2016),
  ys   = ys,
  xs   = xs,
  controls = controls,
  fe = "district + party + cycle",
  cluster = ~district
)

etable_grid(models_norm16, keep_terms = xs)

plot_coef(models_norm16, xs)
#-------------------------------------------------------



#-------------------------------------------------------
# Analysis: second stage
#-------------------------------------------------------

## effect of nd on candidate selection

## IDEOLOGY OF WINNER: winner_data

# extract winner of district, cycle
winner <- join %>% filter(pwinner == "W") %>% filter(!district == "") %>%
  group_by(bonica_rid, district, cycle, party) %>% slice(1) %>% 
  select(cycle, bonica_rid, party, recipient_state, district, recipient_cfscore, dwdime, dwnom1, dwnom2, ps_dwnom1, ps_dwnom2) %>%
  arrange(district, cycle) %>% ungroup(.) 

winner <- left_join(winner, collapsed,
                    by= c("district" = "cong",
                          "cycle"= "year"))  %>%
  filter(party %in% c(100, 200)) %>%
  # simple absolutes of the raw ideology measures
  mutate(
    ab.cfscore = abs(recipient_cfscore),
    ab.dwdime  = abs(dwdime),
    ab.dw1     = abs(dwnom1),
    ab.dw2     = abs(dwnom2)
  ) %>%
  # center each measure by the party-specific median
  group_by(party) %>%
  mutate(
    dist        = recipient_cfscore - median(recipient_cfscore, na.rm = TRUE),
    dist.dwdime = dwdime            - median(dwdime,            na.rm = TRUE),
    dist.dw1    = dwnom1            - median(dwnom1,            na.rm = TRUE),
    dist.dw2    = dwnom2            - median(dwnom2,            na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # absolute distances
  mutate(
    across(
      c(dist, dist.dwdime, dist.dw1, dist.dw2),
      ~ abs(.x),
      .names = "ab.{.col}"
    )
  )

# add disaster

# ---- 1) Base recodes + logs on WINNER ----
winner_data <- winner %>%
  mutate(
    # recode NA -> 0
    nd_count  = if_else(is.na(unique_disasters), 0, unique_disasters),
    nd_deaths = if_else(is.na(total_deaths),     0, total_deaths),
    nd_damage = if_else(is.na(total_damage),     0, total_damage),
    nd_affect = if_else(is.na(total_affected),   0, total_affected),
    
    # exposure (any disaster)
    nd_any = as.integer(nd_count > 0),
    
    # logs (safe for zeros)
    nd_deaths_log   = log1p(nd_deaths),
    nd_damage_log   = log1p(nd_damage),
    nd_affected_log = log1p(nd_affect),
      ) %>%
  # ---- 2) Within-cycle Top 5% flags (exclude zeros) ----
group_by(cycle) %>%
  mutate(
    thr5_count  = {x <- nd_count [nd_count  > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    thr5_deaths = {x <- nd_deaths[nd_deaths > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    thr5_damage = {x <- nd_damage[nd_damage > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    thr5_affect = {x <- nd_affect[nd_affect > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    
    nd_top5_count    = as.integer(nd_count  > thr5_count),
    nd_top5_deaths   = as.integer(nd_deaths > thr5_deaths),
    nd_top5_damage   = as.integer(nd_damage > thr5_damage),
    nd_top5_affected = as.integer(nd_affect > thr5_affect)
  ) %>%
  ungroup() %>%
  select(-starts_with("thr5_"))



## PROBABILITY OF WINNING: prob_anl

library(haven)

pri10 <- read_dta("/Users/hyoon/Desktop/Yoon2/11. 2023Fall/apsa_interdistrict/House primary elections (1956-2010) data.dta") %>% 
  filter(year > 1998) %>%
  mutate(abb = state2abbr(.$state),
         district = substr(.$stcd,3,4),
         lname = sub("_.*", "", .$candidate),
         party = ifelse(.$party == 0, 200, 100)) %>% 
  mutate(cong = paste0(abb, district))

pri18 <- read_dta("/Users/hyoon/Desktop/Yoon2/11. 2023Fall/apsa_interdistrict/house_primary_2012_2018.dta") %>% 
  mutate(abb = state2abbr(.$state),
         district = substr(.$stcd,3,4),
         lname = sub("_.*", "", .$candidate),
         party = ifelse(.$party == 0, 200, 100)) %>% 
  mutate(cong = paste0(abb, district))

pri_vs <- bind_rows(pri10, pri18) %>% select(year, party, candpct, winner, runoff, abb, district, lname, cong)

# combine it with dime dataset

cand1 <- cand %>% mutate(lname = sub(",.*", "", .$name),
                         party = as.double(.$party)) %>% 
  select(cycle, party, lname, district, state, recipient_cfscore, dwdime, dwnom1, dwnom2, 
         ps_dwnom1, ps_dwnom2, prim_vote_pct, pwinner)

prob2 <- left_join(pri_vs, cand1, by=c("year"="cycle",
                                       "party"="party",
                                       "lname"="lname",
                                       "cong"="district")) %>% arrange(year, cong)

prob <- prob2 %>% group_by(year, party, cong) %>% summarize(num = n(),
                                                            prob_cf = sum(candpct*recipient_cfscore)/num,
                                                            prob_dw1 = sum(candpct*dwnom1)/num,
                                                            prob_dw2 = sum(candpct*dwnom2)/num,
                                                            prob_dd = sum(candpct*dwdime)/num,
                                                            prob_pdw1 = sum(candpct*ps_dwnom1)/num,
                                                            prob_pdw2 = sum(candpct*ps_dwnom2)/num) %>% 
  arrange(year, cong, party) %>% ungroup(.)

# haven't done this yet. need to adjust
prob_anl <- left_join(prob, collapsed,
                      by= c("cong", "year")) %>% 
  mutate(
    # recode NA -> 0
    nd_count  = if_else(is.na(unique_disasters), 0, unique_disasters),
    nd_deaths = if_else(is.na(total_deaths),     0, total_deaths),
    nd_damage = if_else(is.na(total_damage),     0, total_damage),
    nd_affect = if_else(is.na(total_affected),   0, total_affected),
    
    # exposure (any disaster)
    nd_any = as.integer(nd_count > 0),
    
    # logs (safe for zeros)
    nd_deaths_log   = log1p(nd_deaths),
    nd_damage_log   = log1p(nd_damage),
    nd_affected_log = log1p(nd_affect),
      ) %>%
  # ---- 2) Within-cycle Top 5% flags (exclude zeros) ----
group_by(year) %>%
  mutate(
    thr5_count  = {x <- nd_count [nd_count  > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    thr5_deaths = {x <- nd_deaths[nd_deaths > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    thr5_damage = {x <- nd_damage[nd_damage > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    thr5_affect = {x <- nd_affect[nd_affect > 0]; if (length(x)) quantile(x, 0.95, na.rm = TRUE) else Inf},
    
    nd_top5_count    = as.integer(nd_count  > thr5_count),
    nd_top5_deaths   = as.integer(nd_deaths > thr5_deaths),
    nd_top5_damage   = as.integer(nd_damage > thr5_damage),
    nd_top5_affected = as.integer(nd_affect > thr5_affect)
  ) %>%
  ungroup() %>%
  select(-starts_with("thr5_"))
#-------------------------------------------------------