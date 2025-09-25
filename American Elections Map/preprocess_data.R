## Importing needed libraries
library(dplyr)
library(leaflet)
library(tidyr)
library(purrr)
library(haven)
library(tibble)
library(openxlsx)

## Loading the raw data
data <- readRDS("AEM_data.rds")
data <- zap_labels(data)
data <- data %>% mutate(VOTES = replace_na(VOTES,0)) %>% filter(VOTES>0)

## Adding District of Columbia's 1964 and 1968 Presidential election
dc_data <- data.frame(
  ICPSRST = rep(98,4),
  STATE = rep('District of Columbia',4),
  ICPSRCTY = rep(0010,4),
  ICPSRNAM = rep('DISTRICT OF COLUMBIA',4),
  YEAR = c(1964,1964,1968,1968),
  ELECT_OFFICE_CODE = rep(1,4),
  ELECT_OFFICE = rep('PRES',4),
  ELECT_TYPE = rep('G',4),
  PARTY_CODE = c(100,200,100,200),
  PARTY_NAME = rep(c('DEMOCRAT','MODERN REPUBLICAN'),2),
  VOTES = c(169796,28801,139566,31012),
  TOTAL = c(198597,198597,170578,170578),
  TOTAL2 = c(198597,198597,170578,170578),
  TOTAL_VOTES = c(198597,198597,170578,170578),
  source = rep(NA,4),
  MONTH = rep(NA,4),
  CONG_NUM = rep(NA,4),
  INDEX = rep(NA,4),
  ELEC_ID = c(1321,1321,1369,1369),
  ELECT_OFFICE_CODE_ORIG = rep(1,4),
  ELECT_OFFICE_ORIG = rep('PRES',4),
  ELECT_TYPE_ORIG = rep('G',4)
)

data <- rbind(data,dc_data)

## Summing votes by party within each county
data_same_party_sum <- data %>%
  group_by(STATE, ICPSRNAM, ICPSRCTY, YEAR, ELECT_OFFICE, PARTY_CODE) %>%
  summarise(VOTES = sum(VOTES, na.rm = TRUE), .groups = "drop")

## Correcting miss-labeled data
data_same_party_sum <- data_same_party_sum %>%
  mutate(
    ELECT_OFFICE = if_else(STATE == "Arkansas" & ELECT_OFFICE == "PRES" & YEAR == 1878,"GOV",ELECT_OFFICE)
  )

## Loading info about parties (self-made) and connection set which enable us to connect the data to geographical info
party_color <- read.xlsx('data/parties.xlsx')
left_join_counties_map <- read.xlsx('data/counties.xlsx',sheet='TO_JOIN')

## Creating an order of parties to later plot easily
party_order <- party_color %>% arrange(ORDER) %>% pull(PARTY_CODE)

## Using party code to look up the party name and assign color
party_lookup <- party_color %>%
  select(PARTY_CODE, PARTY_NAME) %>%
  distinct() %>% deframe()

party_color <- party_color %>%
  select(PARTY_CODE, PARTY_COLOR) %>%
  distinct() %>% deframe()

## Making color pallet for the parties
party_pal <- colorFactor(
  palette = party_color,
  levels = names(party_color)
)

## Initiating the nested lists for our processed data (one for counties and one for states)
county_election_data <- list(PRES = list(), GOV = list(), CONG = list(), SEN = list())
state_election_data <- list(PRES = list(), GOV = list(), CONG = list(), SEN = list())

## Getting all the elections within the data and removing Attorney General elections (only available for 2 years)
elections <- data_same_party_sum %>% select(YEAR,ELECT_OFFICE) %>% distinct %>% filter(ELECT_OFFICE != 'ATGN') %>%
  arrange(YEAR,ELECT_OFFICE)

## Loop to process each election year and type
for (elec_number in 1:nrow(elections)) {
  print(elec_number) # out of 367 (to see progress)
  
  ## Getting the data corresponding to a given year and election type
  year_of_interest <- as.character(elections[elec_number, ]$YEAR)
  election_type <- as.character(elections[elec_number, ]$ELECT_OFFICE)
  
  subset_of_data <- data_same_party_sum %>% filter(YEAR == year_of_interest & ELECT_OFFICE == election_type)
  
  #########################
  ###### COUNTY WISE ######
  #########################
  
  ## Pivoting the table so row is a county and the results of all parties in a given election within the county
  wide_data <- subset_of_data %>%
    pivot_wider(
      id_cols = c(STATE, ICPSRCTY, ICPSRNAM),
      names_from = PARTY_CODE,
      values_from = VOTES
    ) %>% mutate(across(everything(), ~ replace_na(.x, 0)))
  
  ## Indicating the columns with party results
  party_cols <- intersect(names(wide_data), names(party_lookup))
  
  ## Compute total votes, identify the winning party and its percentage and map the party name to its code
  wide_data <- wide_data %>%
    rowwise() %>%
    mutate(
      total_votes = sum(c_across(all_of(party_cols))),
      winner_code = as.character(party_cols[which.max(c_across(all_of(party_cols)))]),
      winner_pct = c_across(all_of(as.character(party_cols[which.max(c_across(all_of(party_cols)))]))) / sum(c_across(all_of(party_cols)))
    ) %>%
    ungroup() %>%
    mutate(
      winner = party_lookup[winner_code]
    )
  
  ## Reordering columns so the most popular parties are at the top in leaflet tooltip
  party_totals <- wide_data %>%
    select(all_of(party_cols)) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "party", values_to = "total") %>%
    arrange(desc(total))
  
  party_cols_ordered <- party_totals$party
  
  ## Joining the data with an info to be able to connect to geographic information
  wide_data <- wide_data %>%
    select(
      -all_of(party_cols),
      all_of(party_cols_ordered),
      everything()
    ) %>%
    left_join(left_join_counties_map, by = c("ICPSRNAM","ICPSRCTY","STATE"))
  
  ## Saving the processed election data for counties
  county_election_data[[election_type]][[year_of_interest]] <- wide_data
  
  #########################
  ###### STATE WISE #######
  #########################
  
  ## Grouping the election data for every state
  state_wise_wide_data <- wide_data %>%
    group_by(STATE) %>%
    summarise(across(all_of(party_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Compute total votes, identify the winning party and its percentage and map the party name to its code
  state_wise_wide_data <- state_wise_wide_data %>%
    rowwise() %>%
    mutate(
      total_votes = sum(c_across(all_of(party_cols))),
      winner_code = as.character(party_cols[which.max(c_across(all_of(party_cols)))]),
      winner_pct = c_across(all_of(as.character(party_cols[which.max(c_across(all_of(party_cols)))]))) / sum(c_across(all_of(party_cols)))
    ) %>%
    ungroup() %>%
    mutate(
      winner = party_lookup[winner_code]
    )
  
  ## Reordering columns so the most popular parties are at the top in leaflet tooltip
  party_totals <- state_wise_wide_data %>%
    select(all_of(party_cols)) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "party", values_to = "total") %>%
    arrange(desc(total))
  
  party_cols_ordered <- party_totals$party
  
  state_wise_wide_data <- state_wise_wide_data %>%
    select(
      -all_of(party_cols),
      all_of(party_cols_ordered),
      everything()
    )
  
  ## Saving the processed election data for states
  state_election_data[[election_type]][[year_of_interest]] <- state_wise_wide_data
}


## Saving the processed data (hashed to it is not tempered with on accident)
save(county_election_data,state_election_data,elections, file = "data/election_data.RData")
save(party_color,party_lookup,party_pal,party_order,file='data/party_data.RData')

## Data used for analysis
data_for_analysis <- data_same_party_sum %>% filter(ELECT_OFFICE != 'ATGN')
data_for_analysis <- data_for_analysis %>% left_join(left_join_counties_map, by = c("ICPSRNAM","ICPSRCTY","STATE")) %>% select(NAMECOUNTY,NAMESTATE,YEAR,ELECT_OFFICE,PARTY_CODE,VOTES)

save(data_for_analysis,file='data/analysis_data.RData')
