county_election_data_unlisted <- unlist(county_election_data, recursive = FALSE)

for (election_index in seq_along(county_election_data_unlisted)) {
  
  one_election <- county_election_data_unlisted[[election_index]]
  year <- strsplit(names(unlist(county_election_data, recursive = FALSE))[[election_index]],'[.]')[[1]][[2]]
  election_type <-  strsplit(names(unlist(county_election_data, recursive = FALSE))[[election_index]],'[.]')[[1]][[1]]
  
  map_data <- counties %>% mutate(NAME = toupper(NAME)) %>%
    left_join(one_election, by = c("NAME","STATE_NAME","GEOID")) %>%
    mutate(
      STATE = coalesce(STATE,STATE_NAME),
      fill_col = mapply(shade_party,winner_code,winner_pct)
    )
  
  election_type_proper <- case_when(election_type == 'PRES' ~ 'Presidential',
                                    election_type == 'GOV' ~ 'Gubernatorial',
                                    election_type == 'CONG' ~ 'Congressional',
                                    election_type == 'SEN' ~ 'Senate')
  
  map <- ggplot(map_data) +
    geom_sf(aes(fill = fill_col), color = "black", size = 0.35) +
    scale_fill_identity() +
    ggtitle(paste0(election_type_proper,' Election in ', year)) +
    theme_minimal() + 
    geom_sf(
      data = states,
      fill = NA,
      color = "black",
      size = 0.8
    ) +
    theme(plot.title = element_text(size = 20, face = "bold"))
  
  ggsave(paste0('maps/map_',election_type,'_',year,'.png'), plot = map, width = 18, height = 16, dpi = 300)
  
  print(paste0(election_type_proper,'-',year))
}


