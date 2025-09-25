## Base data manipulation libraries
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(forcats)
library(stringr)
library(data.table)
library(matrixStats)
library(colorspace)
library(readxl)
library(openxlsx)
library(haven)

## Map libraries
library(sf)
library(tigris)
library(lwgeom)
library(sp)

## Figures and tables libraries
library(ggplot2)
library(patchwork)
library(plotly)
library(leaflet)
library(htmltools)
library(DT)

## Shiny libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)

## Machine learning libraries
library(xgboost)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)

## For Documentation
library(markdown)

## Load preprocessed data and XGBoost model
load("data/election_data.RData")
load("data/party_data.RData")
load("data/prediction_data.RData")
load("data/analysis_data.RData")
xgboost <- readRDS("data/xgboost.rds")

## Setting resolution of maps (other options 500k, 5m and 20m)
map_resolution = "5m"

## Importing US states data
states <- states(cb = TRUE, resolution = map_resolution, year = 2020) %>%
  filter(!NAME %in% c(
    "United States Virgin Islands",
    "Commonwealth of the Northern Mariana Islands",
    "Guam",
    "Puerto Rico",
    "American Samoa",
    "", NA_character_
  )) %>% st_as_sf()

states <- st_transform(states,
                       crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

## Manipulation Alaska
Alaska <- as_Spatial(states %>% filter(NAME == "Alaska"))
Alaska <- elide(Alaska, rotate=-32)
Alaska_bbox = bbox(Alaska)
Alaska <- elide(Alaska, scale=max(apply(Alaska_bbox, 1, diff))/2.3)
Alaska <- elide(Alaska, shift=c(-2100000, -2500000))
Alaska <- st_as_sf(Alaska)
st_crs(Alaska) <- st_crs(states)

## Manipulation Hawaii
Hawaii <- as_Spatial(states %>% filter(NAME == "Hawaii"))
Hawaii = elide(Hawaii, rotate=-35)
Hawaii = elide(Hawaii, shift=c(5400000, -1400000))
Hawaii <- st_as_sf(Hawaii)
st_crs(Hawaii) <- st_crs(states)

states <- rbind(states %>% filter(!NAME %in% c("Alaska","Hawaii")),Alaska,Hawaii) %>% st_transform(4326)
states_post_1863 <- states

## Merging Virginias for pre 1863 maps
va_geom <- states %>%
  filter(NAME %in% c("Virginia", "West Virginia")) %>%
  st_union()

va_attrs <- states %>%
  filter(NAME == "Virginia")

va_merged <- va_attrs %>%
  st_set_geometry(va_geom)

states_pre_1863 <- states %>%
  filter(!NAME %in% c("Virginia", "West Virginia")) %>%
  rbind(va_merged)

## Importing US counties data
counties <- counties(cb = TRUE, resolution = map_resolution, year = 2020) %>%
  filter(!STATE_NAME %in% c(
    "Commonwealth of the Northern Mariana Islands",
    "American Samoa",
    "Guam",
    "Puerto Rico",
    "United States Virgin Islands",
    "", NA_character_
  )) %>% st_as_sf()

counties <- st_transform(counties,
                         crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

## Manipulation Alaska counties
Alaska_counties <- as_Spatial(counties %>% filter(STATE_NAME == "Alaska"))
Alaska_counties <- elide(Alaska_counties, rotate=-32)
Alaska_counties <- elide(Alaska_counties, scale=max(apply(Alaska_bbox, 1, diff))/2.3)
Alaska_counties <- elide(Alaska_counties, shift=c(-2100000, -2500000))
Alaska_counties <- st_as_sf(Alaska_counties)
st_crs(Alaska_counties) <- st_crs(counties)

## Manipulation Hawaii counties
Hawaii_counties <- as_Spatial(counties %>% filter(STATE_NAME == "Hawaii"))
Hawaii_counties = elide(Hawaii_counties,rotate=-35)
Hawaii_counties = elide(Hawaii_counties, shift=c(5400000, -1400000))
Hawaii_counties <- st_as_sf(Hawaii_counties)
st_crs(Hawaii_counties) <- st_crs(counties)

counties <- rbind(counties %>% filter(!STATE_NAME %in% c("Alaska","Hawaii")),Alaska_counties,Hawaii_counties) %>% st_transform(4326)

##Merge together counties that didn't exist before 1968 (end of our dataset)
county_groups = list(
  'Fairbanks' = c('02090','02240','02068'),
  'Bristol_Bay' = c('02060','02070','02164'),
  'Aleutians' = c('02013','02016'),
  'Wrangell_Petersburg' = c('02275','02195'),
  'Juneau' = c('02110','02105','02100','02230'),
  'Yuma' = c('04027','04012'),
  'Boulder' = c('08013','08014'),
  'Maui' = c('15009','15005'),
  'Valencia' = c('35061','35006'),
  'Prince_William' = c('51153','51683','51685'),
  'York' = c('51199','51735')
)

fusing <- counties %>%
  mutate(GEOID = case_when(
    GEOID %in% county_groups$Fairbanks ~ "02090",
    GEOID %in% county_groups$Bristol_Bay ~ "02060",
    GEOID %in% county_groups$Aleutians ~ "02013",
    GEOID %in% county_groups$Wrangell_Petersburg ~ "02275",
    GEOID %in% county_groups$Juneau ~ "02110",
    GEOID %in% county_groups$Yuma ~ "04027",
    GEOID %in% county_groups$Boulder ~ "08013",
    GEOID %in% county_groups$Maui ~ "15009",
    GEOID %in% county_groups$Valencia ~ "35061",
    GEOID %in% county_groups$Prince_William ~ "51153",
    GEOID %in% county_groups$York ~ "51199",
    TRUE ~ GEOID
  ))

fusing <- fusing %>%
  group_by(GEOID) %>%
  summarise(geometry = if (n() > 1) st_union(geometry) else geometry, .groups = "drop")

counties <- fusing %>% left_join(counties %>% st_drop_geometry(),by=c('GEOID'='GEOID'))

## Defining function to change the color depending on the % of votes
shade_party <- function(party, pct) {
  if (is.na(party)) return('#808080')
  color <- party_color[[party]]
  
  if (pct < 0.5) {
    return(lighten(color, amount = 0.75))
  } else if (pct < 0.6) {
    return(lighten(color, amount = 0.5))
  } else if (pct < 0.7) {
    return(lighten(color, amount = 0.25))
  } else if (pct < 0.8) {
    return(color)
  } else if (pct < 0.9) {
    return(darken(color, amount = 0.33))
  } else {
    return(darken(color, amount = 0.66))
  }
}
shade_party <- Vectorize(shade_party)

## Defining function to change the color depending on the % of votes only for Democratic and Republican party
shade_party_rep_dem_only <- function(party, pct) {
  if (party == '100') {
    if (pct < 0.5) {
      return('#CED7FF')
    } else if (pct < 0.6) {
      return('#9AAFFF')
    } else if (pct < 0.7) {
      return("#6989EE")
    } else if (pct < 0.8) {
      return("#3D66CD")
    } else if (pct < 0.9) {
      return("#2D4486")
    } else {
      return("#00235D")
    }
  }
  else {
    if (pct < 0.5) {
      return("#FFCECE")
    } else if (pct < 0.6) {
      return('#FF9A9A')
    } else if (pct < 0.7) {
      return("#F56363")
    } else if (pct < 0.8) {
      return("#CD3D3D")
    } else if (pct < 0.9) {
      return("#862D2D")
    } else {
      return("#560707")
    }
  }
}
shade_party_rep_dem_only <- Vectorize(shade_party_rep_dem_only)

## As above but for states only (only used during prediction to better show differences in %)
shade_party_rep_dem_only_states_version <- function(party, pct) {
  if (party == '100') {
    if (pct < 0.51) {
      return('#CED7FF')
    } else if (pct < 0.525) {
      return('#9AAFFF')
    } else if (pct < 0.55) {
      return("#6989EE")
    } else if (pct < 0.65) {
      return("#3D66CD")
    } else if (pct < 0.8) {
      return("#2D4486")
    } else {
      return("#00235D")
    }
  }
  else {
    if (pct < 0.51) {
      return("#FFCECE")
    } else if (pct < 0.525) {
      return('#FF9A9A')
    } else if (pct < 0.55) {
      return("#F56363")
    } else if (pct < 0.65) {
      return("#CD3D3D")
    } else if (pct < 0.8) {
      return("#862D2D")
    } else {
      return("#560707")
    }
  }
}
shade_party_rep_dem_only_states_version <- Vectorize(shade_party_rep_dem_only_states_version)

## Function to change color based on either coloring_value or on previously used color
color_switching <- function(color,coloring_value) {
  if (coloring_value == 1) {
    return('#3D66CD')
  } else if (coloring_value == 2) {
    return('#CD3D3D')
  } else if (coloring_value == 3) {
    return('#6989EE')
  } else if (coloring_value == 4) {
    return('#F56363')
  } else if (coloring_value == 5) {
    return('#9AAFFF')
  } else if (coloring_value == 6) {
    return('#FF9A9A')
  } else if (coloring_value == 7) {
    return('#CED7FF')
  } else if (coloring_value == 8) {
    return('#FFCECE')
  }
  
  if (color == '#E6E6E6') {
    return('#FFCECE')
  } else if (color == '#FFCECE') {
    return('#FF9A9A')
  } else if (color == '#FF9A9A') {
    return('#F56363')
  } else if (color == '#F56363') {
    return('#CD3D3D')
  } else if (color == '#CD3D3D') {
    return('#3D66CD')
  } else if (color == '#3D66CD') {
    return('#6989EE')
  } else if (color == '#6989EE') {
    return('#9AAFFF')
  } else if (color == '#9AAFFF') {
    return('#CED7FF')
  } else {
    return('#E6E6E6')
  }
}
color_switching <- Vectorize(color_switching)

## Function to decode the color value to the inclination to either party
color_decoding <- function(color) {
  if (color == '#E6E6E6') {
    return(NA)
  } else if (color == '#FFCECE') {
    return(-1)
  } else if (color == '#FF9A9A') {
    return(-2)
  } else if (color == '#F56363') {
    return(-3)
  } else if (color == '#CD3D3D') {
    return(-4)
  } else if (color == '#3D66CD') {
    return(4)
  } else if (color == '#6989EE') {
    return(3)
  } else if (color == '#9AAFFF') {
    return(2)
  } else {
    return(1)
  }
}
color_decoding <- Vectorize(color_decoding)

## As above but in writing
margin_decoding <- function(color) {
  if (color == "#E6E6E6") {
    return('NONE')
  } else if (color == "#CED7FF") {
    return('TILT DEMOCRATIC')
  } else if (color == "#9AAFFF") {
    return('LEAN DEMOCRATIC')
  } else if (color == "#6989EE") {
    return("LIKELY DEMOCRATIC")
  } else if (color == "#3D66CD") {
    return("SAFE DEMOCRATIC")
  } else if (color == "#2D4486") {
    return("SAFE DEMOCRATIC")
  } else if (color == "#00235D") {
    return("SAFE DEMOCRATIC")
  } else if (color == "#FFCECE") {
    return("TILT REPUBLICAN")
  } else if (color == "#FF9A9A") {
    return("LEAN REPUBLICAN")
  } else if (color == "#F56363") {
    return("LIKELY REPUBLICAN")
  } else {
    return("SAFE REPUBLICAN")
  }
}
margin_decoding <- Vectorize(margin_decoding)

## Converter between abbreviation and the full name
election_names <- c(
  PRES = "Presidential",
  GOV  = "Gubernatorial",
  CONG = "Congressional",
  SEN  = "Senate"
)

## Colors used for different influances by either party
custom_colors <- c(
  "DEMOCRATIC" = "#3D66CD", 
  "REPUBLICAN" = "#CD3D3D",
  "SAFE DEMOCRATIC" = "#3D66CD",
  "LIKELY DEMOCRATIC" = "#6989EE",
  "LEAN DEMOCRATIC" = "#9AAFFF",
  "TILT DEMOCRATIC" = "#CED7FF",
  "SAFE REPUBLICAN" = "#CD3D3D",
  "LIKELY REPUBLICAN" = "#F56363",
  "LEAN REPUBLICAN" = "#FF9A9A",
  "TILT REPUBLICAN" = "#FFCECE",
  "NONE" = "#E6E6E6",
  "NONE_2" = "#E6E6E6"
)

## UI element which creates 2 buttons with given colors and label between those
button_row <- function(id, label, color_d, border_d, color_r, border_r, icon_value) {
  div(
    style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
    actionButton(paste0("btn_", id, "_d"), "",
                 style = sprintf("color: white; background-color: %s; border-color: %s; width: 45px; height : 35px",
                                 color_d, border_d), icon = if (icon_value == 1) icon("check") else NULL),
    span(label, style = "font-weight: bold; font-size: 16px; width: 50px; text-align: center;"),
    actionButton(paste0("btn_", id, "_r"), "",
                 style = sprintf("color: white; background-color: %s; border-color: %s; width: 45px; height : 35px",
                                 color_r, border_r), icon = if (icon_value == 2) icon("check") else NULL)
  )
}

## UI element for the entire box used in prediction tab which changes based on the selected color
prediction_coloring_box <- function(map_coloring_value) {
  box(
    width = 3,
    solidHeader = TRUE,
    title = "Options",
    status = "primary",
    div(
      style = "display: flex; flex-direction: column; align-items: center; gap: 12px;",
      
      if (map_coloring_value == 1) {
        button_row("safe", "Safe", "#3D66CD", "#2C4DA5", "#CD3D3D", "#9E2E2E", 1)
      } else if (map_coloring_value == 2) {
        button_row("safe", "Safe", "#3D66CD", "#2C4DA5", "#CD3D3D", "#9E2E2E", 2)
      } else {
        button_row("safe", "Safe", "#3D66CD", "#2C4DA5", "#CD3D3D", "#9E2E2E", 0)
      },
      
      if (map_coloring_value == 3) {
        button_row("likely", "Likely", "#6989EE", "#4D6AD1", "#F56363", "#C74A4A", 1)
      } else if (map_coloring_value == 4) {
        button_row("likely", "Likely", "#6989EE", "#4D6AD1", "#F56363", "#C74A4A", 2)
      } else {
        button_row("likely", "Likely", "#6989EE", "#4D6AD1", "#F56363", "#C74A4A", 0)
      },
      
      if (map_coloring_value == 5) {
        button_row("lean", "Lean", "#9AAFFF", "#6C81E0", "#FF9A9A", "#CC7A7A", 1)
      } else if (map_coloring_value == 6) {
        button_row("lean", "Lean", "#9AAFFF", "#6C81E0", "#FF9A9A", "#CC7A7A", 2)
      } else {
        button_row("lean", "Lean", "#9AAFFF", "#6C81E0", "#FF9A9A", "#CC7A7A", 0)
      },
      
      if (map_coloring_value == 7) {
        button_row("tilt", "Tilt", "#CED7FF", "#9BA5E0", "#FFCECE", "#D9A3A3", 1)
      } else if (map_coloring_value == 8) {
        button_row("tilt", "Tilt", "#CED7FF", "#9BA5E0", "#FFCECE", "#D9A3A3", 2)
      } else {
        button_row("tilt", "Tilt", "#CED7FF", "#9BA5E0", "#FFCECE", "#D9A3A3", 0)
      }
    ),
    
    tags$hr(),
    div(
      style = "display: flex; justify-content: space-between; margin-top: 10px; width: 100%;",
      actionButton("btn_reset", "Reset",
                   style = "color: white; background-color: #6c757d; border-color: #5a6268"),
      actionButton("btn_calculate", "Calculate",
                   style = "color: white; background-color: #28a745; border-color: #218838")
    )
  )
}

## Data used for setting up a filter
analysis_all_counties <- data_for_analysis %>% select(NAMECOUNTY,NAMESTATE,YEAR) %>% distinct
