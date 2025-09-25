## Importing libraries
library(forcats)
library(xgboost)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(data.table)
library(matrixStats)
library(stringr)
library(ggplot2)
library(patchwork)

## Import data
load("data/party_data.RData")
load("data/election_data.RData")
parties <- read.xlsx('data/parties.xlsx')

## Assign parties to the affiliations (Democratic, Republican, Both and Other)
dem_parties_code <- parties %>% filter(AFFILIATION=='D') %>% select(PARTY_CODE) %>% deframe() %>% as.character()
rep_parties_code <- parties %>% filter(AFFILIATION=='R') %>% select(PARTY_CODE) %>% deframe() %>% as.character()
both_parties_code <- parties %>% filter(AFFILIATION=='B') %>% select(PARTY_CODE) %>% deframe() %>% as.character()
other_parties_code <- parties %>% filter(is.na(AFFILIATION)) %>% select(PARTY_CODE) %>% deframe() %>% as.character()
all_codes <- parties %>% select(PARTY_CODE) %>% deframe() %>% as.character()

## Get all the possible elections
all_elections <- elections %>% mutate(election = paste0(ELECT_OFFICE,"_",YEAR)) %>% select(election) %>% deframe()

## Convert percentage of votes to party inclination
affiliation_category <- function(pct) {
  case_when(
    is.na(pct) ~ NA,
    pct > 0.55 ~ 4, ## Safe Dem
    pct > 0.525 ~ 3, ## Likely Dem
    pct > 0.51 ~ 2, ## Lean Dem
    pct >= 0.5 ~ 1, ## Tilt Dem
    pct >= 0.49 ~ -1, ## Tilt Rep
    pct >= 0.475 ~ -2, ## Lean Rep
    pct >= 0.45 ~ -3, ## Likely Rep
    TRUE ~ -4 ## Save Rep
  )
}

## Function to assign colors to´votes pct 
shade_party_state_pred <- function(party, pct) {
  if (is.na(party)) return('#808080')
  color <- party_color[[party]]
  
  if (pct < 0.51) {
    return(lighten(color, amount = 0.75))
  } else if (pct < 0.525) {
    return(lighten(color, amount = 0.5))
  } else if (pct < 0.55) {
    return(lighten(color, amount = 0.25))
  } else {
    return(color)
  }
}
shade_party_state_pred <- Vectorize(shade_party_state_pred)

####################
#### TRAIN DATA ####
####################

total_data <- NULL

## Algorithm to create the training data
for (year_of_interest in as.character(1968:1968)) {
  for (election_type in c('PRES','GOV','CONG','SEN')) {
    
    ## Condition to ascertain weather the selected election happened
    if (paste0(election_type,'_',year_of_interest) %in% all_elections) {
      
      ## Getting all election that could have happened in last 16 years 
      possible_elections <- apply(expand.grid(c("PRES", "GOV", "CONG", "SEN"),
         (as.numeric(year_of_interest) - 1):(as.numeric(year_of_interest) - 16)), 1, paste, collapse = "_")
      
      ## Selecting the preprocessed data and calculating the number of votes for each party and adjusted total plus getting the percentage of votes between democrats and republicans
      data <- county_election_data[[election_type]][[year_of_interest]]
      data <- data %>%
        mutate(
          year = year_of_interest,
          election_type = election_type,
          dem = rowSums(across(any_of(union(dem_parties_code, both_parties_code))), na.rm = TRUE),
          rep = rowSums(across(any_of(union(rep_parties_code, both_parties_code))), na.rm = TRUE),
          inflated_total = rowSums(across(any_of(all_codes)), na.rm = TRUE) +
            rowSums(across(any_of(both_parties_code)), na.rm = TRUE),
          total = rowSums(across(any_of(all_codes)), na.rm = TRUE)
        ) %>% select(ICPSRCTY,ICPSRNAM,STATE,year,election_type,dem,rep,inflated_total,total) %>%
        mutate(
          dem_pct = dem/(dem+rep),
          weight = (rep+dem) * total/inflated_total, ## Weight for summing up all counties in the state
        )
      
      ## Counting the state election results and getting the state party affiliation
      state_means <- data %>%
        group_by(STATE) %>%
        summarise(wmean = weighted.mean(dem_pct, weight, na.rm = TRUE),
                  factor = (sum(dem)+sum(rep))/sum(inflated_total),
                  .groups = "drop") %>%
        mutate(pct = (wmean-0.5)*factor+0.5) %>%
        mutate(category = affiliation_category(pct) )%>%
        select(STATE,category)
      
      state_means <- tibble(STATE = sort(states$NAME)) %>%
        left_join(state_means, by = "STATE")
      
      ## Calculating the metric as presented in the documentation
      data <- data %>% mutate(
        coef =(dem+rep)/inflated_total * log((0.01+dem/(dem+rep))/(1.01-(dem/(dem+rep))))
      ) %>% select(ICPSRCTY,ICPSRNAM,STATE,year,election_type,coef)
      
      ## Adding the state election results
      data <- bind_cols(data, pivot_wider(state_means,names_from = STATE, values_from = category))
      
      ## Algorithm to calculate the metric for every previous election in the last 16 years for a give county
      for (election in possible_elections) {
        year <- strsplit(election, "_")[[1]][2]
        type <- strsplit(election, "_")[[1]][1]
        
        if (election %in% all_elections) {
          prior_election <- county_election_data[[type]][[year]]
          
          prior_election <- prior_election %>%
            mutate(
              dem = rowSums(across(any_of(union(dem_parties_code, both_parties_code))), na.rm = TRUE),
              rep = rowSums(across(any_of(union(rep_parties_code, both_parties_code))), na.rm = TRUE),
              inflated_total = rowSums(across(any_of(all_codes)), na.rm = TRUE) +
                rowSums(across(any_of(both_parties_code)), na.rm = TRUE)
            ) %>%
            mutate(
              coef =(dem+rep)/inflated_total * log((0.01+dem/(dem+rep))/(1.01-(dem/(dem+rep)))),
              oth_pct = 1-(rep+dem)/inflated_total,
            ) %>% select(ICPSRCTY,ICPSRNAM,STATE,coef)
          
          data <- data %>% left_join(prior_election,by=c('ICPSRCTY','ICPSRNAM','STATE'),suffix = c("", paste0('_',type,as.numeric(year)-as.numeric(year_of_interest))))
          
          
        } else {
          data[[paste0('coef_',type,as.numeric(year)-as.numeric(year_of_interest))]] <- NA
        }
      }
      
      ## Appending the data
      total_data <- rbind(total_data,data)
    }
  }
}

## Adding more geographic information for counties such as Division and Region (helps for shallow decision trees)
total_data <- total_data %>%
  mutate(year = as.numeric(year), STATE = as.factor(STATE),election_type=as.factor(election_type)) %>%
  filter(!is.na(coef)) %>% mutate(
    Division = fct_collapse(STATE,
      'New England' = c('Connecticut','Maine','Massachusetts','New Hampshire','Rhode Island','Vermont'),
      'Mid-Atlantic' = c('New Jersey','New York','Pennsylvania'),
      'East North Central' = c('Illinois','Indiana','Michigan','Ohio','Wisconsin'),
      'West North Central' = c('Iowa','Kansas','Minnesota','Missouri','Nebraska','North Dakota','South Dakota'),
      'South Atlantic' = c('Delaware','Florida','Georgia','Maryland','North Carolina', 'South Carolina',
                           'Virginia','West Virginia','District of Columbia'),
      'East South Central' = c('Alabama','Kentucky','Mississippi','Tennessee'),
      'West South Central' = c('Arkansas','Louisiana','Oklahoma','Texas'),
      'Mountain' = c('Arizona','Colorado','Idaho','Montana','Nevada','New Mexico','Utah','Wyoming'),
      'Pacific' = c('Alaska','California','Hawaii','Oregon','Washington')
    )
  ) %>%
  mutate(
    Region = fct_collapse(Division,
        'Northeast' = c('New England','Mid-Atlantic'),
        'Midwest' = c('East North Central','West North Central'),
        'South' = c('South Atlantic','East South Central','West South Central'),
        'West' = c('Mountain','Pacific'))
  )

## Defining a function to collapse the past election results as many of the election happen only every other year or once per 4 years and this gives more prediction power to our model for shallow decision trees
make_grouped_coefs <- function(data, office, block_size) {
  cols <- grep(paste0("^coef_", office, "-[0-9]+$"), names(data), value = TRUE)
  nums <- as.integer(str_extract(cols, "(?<=-)[0-9]+"))
  
  groups <- split(cols, ((nums - 1) %/% block_size) + 1)
  
  for (i in seq_along(groups)) {
    start <- min(nums[cols %in% groups[[i]]])
    end   <- max(nums[cols %in% groups[[i]]])
    new_name <- paste0(office, "-", start, "/", end)
    
    data[[new_name]] <- rowMeans(data[groups[[i]]], na.rm = TRUE)
  }
  
  data %>% select(-all_of(cols))
}

## Using said function
total_data_plus <- total_data %>% make_grouped_coefs("PRES", 4) %>%
  make_grouped_coefs("GOV", 2) %>% make_grouped_coefs("CONG", 2) %>% make_grouped_coefs("SEN", 2)

## Replacing 35 out of 51 possible state affiliations to make the predictions less dependent on them as in the use of the XGBoost model I imagine only a few states are gonna be collored in
total_data_plus <- total_data_plus %>%
  mutate(
    temp_list = pmap(list(!!!select(., all_of(states$NAME))),
                     function(...) {
                       all_state_values <- c(...)
                       na_states_indxs <- sample(1:51,35)
                       all_state_values[na_states_indxs] <- NA
                       
                       all_state_values
                     })
  ) %>% select(-states$NAME) %>%
  unnest_wider(temp_list, names_sep = "_") %>%
  rename_with(~ states$NAME, starts_with("temp_list_"))

## Adding column this_state to reinforce affiliation of the state the county is in
total_data_plus$this_state <- as.matrix(total_data_plus[, states$NAME])[
  cbind(seq_len(nrow(total_data_plus)), match(total_data_plus$STATE, states$NAME))]

###################
#### TEST DATA ####
###################

## This section is similar as the above but for data later used for testing
year_of_interest = '1970' ## For the year 1972 presidential election we miss the elections between 1968 and 1972. So I artificially moved the Cong, Gov and Sen elections by 2 years
election_type = 'PRES'

possible_elections <- apply(expand.grid(c("PRES", "GOV", "CONG", "SEN"),
    (as.numeric(year_of_interest) - 1):(as.numeric(year_of_interest) - 16)), 1, paste, collapse = "_")

data1972 <- read.xlsx('data/counties.xlsx',sheet='ALL_COUNTIES')
data1972 <- data1972 %>%
  mutate(
    year = year_of_interest,
    election_type = election_type
  )

for (election in possible_elections) {
  year <- strsplit(election, "_")[[1]][2]
  type <- strsplit(election, "_")[[1]][1]
  
  if (election %in% all_elections) {
    prior_election <- county_election_data[[type]][[year]]
    
    prior_election <- prior_election %>%
      mutate(
        dem = rowSums(across(any_of(union(dem_parties_code, both_parties_code))), na.rm = TRUE),
        rep = rowSums(across(any_of(union(rep_parties_code, both_parties_code))), na.rm = TRUE),
        inflated_total = rowSums(across(any_of(all_codes)), na.rm = TRUE) +
          rowSums(across(any_of(both_parties_code)), na.rm = TRUE),
        total = rowSums(across(any_of(all_codes)), na.rm = TRUE)
      ) %>%
      mutate(
        coef =(dem+rep)/inflated_total * log((0.01+dem/(dem+rep))/(1.01-(dem/(dem+rep)))),
        oth_pct = 1-(rep+dem)/inflated_total,
      ) %>% select(ICPSRCTY,ICPSRNAM,STATE,coef,total)
    
    suffix <- paste0("_", type, as.numeric(year) - as.numeric(year_of_interest))
    
    prior_election <- prior_election %>% rename_with(~ paste0(., suffix), -c(ICPSRCTY, ICPSRNAM, STATE))
    
    data1972 <- data1972 %>% left_join(prior_election,by=c('ICPSRCTY','ICPSRNAM','STATE'))
    
    last_col_name <- paste0('total',suffix)
    
    if ('total' %in% names(data1972)) {
      data1972 <- data1972 %>% mutate(total = coalesce(max(total,!!sym(last_col_name)),total,!!sym(last_col_name))) %>%
        select(-c(!!sym(last_col_name)))
    }
    else {
      data1972 <- data1972 %>% mutate(total = !!sym(last_col_name)) %>% select(-c(!!sym(last_col_name)))
    }
    
    
  }
  else {
    data1972[[paste0('coef_',type,as.numeric(year)-as.numeric(year_of_interest))]] <- NA
  }
}

data1972 <- data1972 %>% make_grouped_coefs("PRES", 4) %>%
  make_grouped_coefs("GOV", 2) %>% make_grouped_coefs("CONG", 2) %>% make_grouped_coefs("SEN", 2)

data1972 <- data1972 %>%
  mutate(year = as.numeric(year), STATE = as.factor(STATE),election_type=as.factor(election_type)) %>%
  mutate(
    Division = fct_collapse(STATE,
                            'New England' = c('Connecticut','Maine','Massachusetts','New Hampshire','Rhode Island','Vermont'),
                            'Mid-Atlantic' = c('New Jersey','New York','Pennsylvania'),
                            'East North Central' = c('Illinois','Indiana','Michigan','Ohio','Wisconsin'),
                            'West North Central' = c('Iowa','Kansas','Minnesota','Missouri','Nebraska','North Dakota','South Dakota'),
                            'South Atlantic' = c('Delaware','Florida','Georgia','Maryland','North Carolina', 'South Carolina',
                                                 'Virginia','West Virginia','District of Columbia'),
                            'East South Central' = c('Alabama','Kentucky','Mississippi','Tennessee'),
                            'West South Central' = c('Arkansas','Louisiana','Oklahoma','Texas'),
                            'Mountain' = c('Arizona','Colorado','Idaho','Montana','Nevada','New Mexico','Utah','Wyoming'),
                            'Pacific' = c('Alaska','California','Hawaii','Oregon','Washington')
    )
  ) %>%
  mutate(
    Region = fct_collapse(Division,
                          'Northeast' = c('New England','Mid-Atlantic'),
                          'Midwest' = c('East North Central','West North Central'),
                          'South' = c('South Atlantic','East South Central','West South Central'),
                          'West' = c('Mountain','Pacific'))
  ) %>%
  mutate(across(where(is.logical), as.numeric))

## We don't know the truth value so we just enter 0
data1972['coef'] <- 0

## We don't know the states party inclinations
for (ST in states$NAME) {
  data1972[ST] <- NA
}

## NA's to numeric
data1972 <- data1972 %>% mutate(across(where(is.logical), as.numeric))

## Factoring the election type
data1972[['election_type']] <- factor(data1972[['election_type']],levels = c("CONG", "GOV", "PRES", "SEN"))


### Set up of affiliactions that can be used to similate 1976 presidential elections
for (ST in states$NAME) {
  data1972[ST] <- -4
}

data1972[['Indiana']] <- -3
data1972[['Kansas']] <- -3
data1972[['Montana']] <- -3
data1972[['North Dakota']] <- -3
data1972[['Michigan']] <- -3
data1972[['Connecticut']] <- -3

data1972[['Nevada']] <- -2
data1972[['Washington']] <- -2
data1972[['New Mexico']] <- -2
data1972[['New Jersey']] <- -2

data1972[['Illinois']] <- -1
data1972[['California']] <- -1
data1972[['South Dakota']] <- -1
data1972[['Virginia']] <- -1
data1972[['Oklahoma']] <- -1
data1972[['Maine']] <- -1
data1972[['Iowa']] <- -1
data1972[['Oregon']] <- -1

data1972[['Ohio']] <- 3 ## To make Ohio Democratic (should be 1)
data1972[['Wisconsin']] <- 2 ## To make Wisconsin Democratic (should be 1)
data1972[['Mississippi']] <- 1

data1972[['Hawaii']] <- 2
data1972[['Pennsylvania']] <- 2
data1972[['Texas']] <- 2
data1972[['Missouri']] <- 2
data1972[['New York']] <- 2

data1972[['Florida']] <- 3
data1972[['Delaware']] <- 3
data1972[['Louisiana']] <- 3
data1972[['Maryland']] <- 3
data1972[['Kentucky']] <- 3

data1972[['North Carolina']] <- 4
data1972[['Rhode Island']] <- 4
data1972[['Minnesota']] <- 4
data1972[['Tennessee']] <- 4
data1972[['South Carolina']] <- 4
data1972[['Alabama']] <- 4
data1972[['Massachusetts']] <- 4
data1972[['West Virginia']] <- 4
data1972[['Arkansas']] <- 4
data1972[['Georgia']] <- 4
data1972[['District of Columbia']] <- 4


data1972$this_state <- as.matrix(data1972[, states$NAME])[
  cbind(seq_len(nrow(data1972)), match(data1972$STATE, states$NAME))]

## From state inclination to approximate democratic vote share
states_pct_goal <- data1972[1,states$NAME] %>%
  pivot_longer(
    cols = everything(),
    names_to = "STATE_NAME",
    values_to = "value"
  ) %>% mutate(
    aim_pct = case_when(
      value == 4 ~ 0.575,
      value == 3 ~ 0.537,
      value == 2 ~ 0.517,
      value == 1 ~ 0.505,
      value == -1 ~ 0.495,
      value == -2 ~ 0.483,
      value == -3 ~ 0.463,
      value == -4 ~ 0.425
    )
  ) %>% select(STATE_NAME,aim_pct)

####################
## MODEL TRAINING ##
####################

## Setting up the task used for the XGBoost training and predictions
training_data <- total_data_plus
testing_data <- data1972

training_data <- as.data.table(training_data)
testing_data <- as.data.table(testing_data)

## Setting up features
target_col <- "coef"
ignore_cols <- c('ICPSRCTY','ICPSRNAM','dem','rep','inflated_total','oth_pct','year')
feature_cols <-  setdiff(names(total_data_plus), c(target_col, ignore_cols))

task <- TaskRegr$new(
  id = "county_election_pred",
  backend = training_data,
  target = target_col
)
task$select(feature_cols)

test <- TaskRegr$new(
  id = "county_election_pred_test",
  backend = testing_data,
  target = target_col
)
test$select(feature_cols)

## Defining, training and saving the model
po_encode <- po("encode", method = "one-hot")
learner <- lrn("regr.xgboost", nrounds = 200, eta = 0.1, max_depth = 10)
pipeline <- po_encode %>>% learner %>% as_learner()

#pipeline$train(task)

saveRDS(pipeline, file = "data/xgboost.rds")

######################
## MODEL PREDICTION ##
######################

## Loading up the trained model and making predictions
xgboost <- readRDS("data/xgboost.rds")

predictions_train <- xgboost$predict(task)
predictions <- xgboost$predict(test)

test_pred <- testing_data %>% select(NAME,STATE_NAME,GEOID,STATE_NAME,total)
test_pred$coef_pred <- predictions$response

## Decoding the predicted metric into the voting share
test_pred <- test_pred %>% mutate(pred = (1.01*exp(coef_pred) - 0.01) / (exp(coef_pred) + 1))

## Calculating the states shares, comparing predicted result and wanted result and adjust the predictions accordingly (aggregate-constrained calibration).
test_pred <- test_pred %>%
  group_by(STATE_NAME) %>%
  summarise(
    pct = sum(pred * total) / (sum(pred * total) + sum((1 - pred) * total)),
    .groups = "drop"
  ) %>%
  right_join(test_pred %>% st_drop_geometry(), by = "STATE_NAME") %>%
  left_join(states_pct_goal,by=c('STATE_NAME')) %>%
  mutate(
    factor = case_when(
      aim_pct == 0.425 & pct < aim_pct ~ 0,
      aim_pct == 0.575 & pct > aim_pct ~ 0,
      aim_pct > 0.51 & pct > 0.52 ~ 0.65,
      aim_pct < 0.49 & pct > 0.48 ~ 0.65,
      TRUE ~ 0.9),
    diff = log((aim_pct-pct+0.5)/(1-aim_pct+pct-0.5)) * factor,
    coef_pred = ifelse(is.na(diff),coef_pred,coef_pred + diff),
    pred = (1.01*exp(coef_pred) - 0.01) / (exp(coef_pred) + 1)
  )

## Joining with the geographic data
test_pred <- counties %>% mutate(NAME = toupper(NAME)) %>% inner_join(test_pred, by = c("NAME","STATE_NAME","GEOID"))

## Calculating the color to display on the map
test_pred <- test_pred %>%
  mutate(
    fill_col_pred = ifelse(pred > 0.5, shade_party_rep_dem_only('100',pred),shade_party_rep_dem_only('200',1-pred))
  )

## Crating the map
p_pred <- ggplot(test_pred %>% filter(election_type == "PRES")) +
  geom_sf(aes(fill = fill_col_pred), color = "black", size = 0.35) +
  scale_fill_identity() +
  ggtitle("Predicted") +
  theme_minimal() + 
  geom_sf(
    data = states %>% filter(NAME != ""),
    fill = NA,
    color = "black",
    size = 0.8
  )

p_pred


## Calculating the predictions and color for the states
test_pred_states <- test_pred %>%
  st_drop_geometry() %>%
  group_by(STATE_NAME) %>%
  summarise(
    dem = sum(pred * total),
    rep = sum((1 - pred) * total),
    .groups = "drop"
  ) %>%
  mutate(
    pct = dem / (dem + rep),
    fill_col_pred = ifelse(pct > 0.5,
                           shade_party_state_pred("100", pct),
                           shade_party_state_pred("200", 1 - pct))
  )

test_pred_states <- states %>% left_join(test_pred_states,by=c('NAME'='STATE_NAME'))

## Defining function to make a quadrilateral in map data
square <- function(xmin, xmax, ymin, ymax) {
  st_polygon(list(matrix(
    c(xmin,ymin, xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin),
    ncol = 2, byrow = TRUE
  )))
}

## Making the rectangles for the states as can be seen in 270toWin
squares_sf <- st_sf(
  STUSPS = c("DC","MD","DE","NJ","CT","RI","MA"),
  geometry = st_sfc(lapply(0:6, \(i) square(-71, -67.7, 31 + i*1.3, 32 + i*1.3)), crs = 4326)
)

## Uniting the rectangles with the states
test_pred_states <- bind_rows(test_pred_states,squares_sf) %>%
  group_by(STUSPS) %>%
  summarise(
    across(.cols = -geometry, .fns  = ~ first(na.omit(.x)),.names = "{.col}"),
    geometry = st_union(geometry),
    .groups = "drop"
  )


## Placement of the text on the map
map_text <- read.xlsx('map_text.xlsx')

## Creating the states map
ggplot(test_pred_states) +
  geom_sf(aes(fill = fill_col_pred), color = "black", size = 0.8) +
  scale_fill_identity() +
  theme_minimal() +
  geom_text(data = map_text %>% filter(STUSPS != 'HI'), aes(x = lon, y = lat, label = label),
            size = 3.4, color = "white", fontface = "bold",family = "sans") +
  geom_text(data = map_text %>% filter(STUSPS == 'HI'), aes(x = lon, y = lat, label = label),
            size = 3.4, color = "black", fontface = "bold",family = "sans") +
  labs(x = "",y = "",title = "Predicted Election Map",subtitle = "1972 US Election") 

## Creating the leaflet map (blank)
test_pred_states$weight <- 0.8
test_pred_states$color <- 'black'
test_pred_states$fill_color <- '#66B3FF'


leaflet(test_pred_states) %>%
  addPolygons(
    color = ~color,
    weight = ~weight,
    fillColor = ~fill_color,
    fillOpacity = 1,
    label = ~NAME,
    layerId = ~STUSPS
  ) %>%
  addLabelOnlyMarkers(
    data = map_text,
    lng = ~lon,
    lat = ~lat,
    label = ~label,
    labelOptions = labelOptions(
      noHide = TRUE,
      textOnly = TRUE,
      direction = "center",
      style = list(
        "color" = "white",
        "font-weight" = "bold",
        "font-family" = "Arial, sans-serif",  # fallback fonts
        "font-size" = "12px",
        "text-shadow" = "1px 1px 2px black"
      )
    )
  )


## Saving some of the data later used in the app code

states_prediction <- bind_rows(states,squares_sf) %>%
  group_by(STUSPS) %>%
  summarise(
    across(.cols = -geometry, .fns  = ~ first(na.omit(.x)),.names = "{.col}"),
    geometry = st_union(geometry),
    .groups = "drop"
  )

save(states_prediction,data1972,map_text,feature_cols,file = "data/prediction_data.RData")





