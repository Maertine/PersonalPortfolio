server <- function(session,input, output) {
  
  #######################
  ### Interactive map ###
  #######################
  
  ## UI for the year filter
  output$year_ui <- renderUI({
    req(input$election_type)
    
    year_choices <- elections %>%
      filter(ELECT_OFFICE == input$election_type) %>%
      select(YEAR) %>%
      distinct() %>%
      arrange(YEAR) %>%
      pull(YEAR)
    
    
    current_selection <- isolate(input$election_year)
    if (!is.null(current_selection) && current_selection %in% year_choices) {
      default_year <- current_selection
    } else {
      default_year <- tail(year_choices, 1) 
    }
    
    selectInput(
      "election_year", "Election Year",
      choices = year_choices,
      selected = default_year
    )
  })
  
  ## UI for the state filter 
  output$state_ui <- renderUI({
    req(map_data())
    
    state_choices <- map_data()$state_map_data %>% filter(!is.na(winner_code)) %>% arrange(NAME) %>% pull(NAME)
    
    selected_state <- if (!is.null(input$state) && input$state %in% state_choices) {
      input$state
    } else {
      state_choices[1]
    }
    
    selectInput("state", "Select State", choices = state_choices, selected = selected_state)
  })
  
  ## To show the state filter when 'Specific state' scope is chosen
  observeEvent(c(input$scope, input$level), {
    if (input$scope == "Specific state" && input$level == 'County-wise') {
      shinyjs::show("state_container")
    } else {
      shinyjs::hide("state_container")
    }
  })
  
  ## The scope option is not available when only the states data is chosen
  observeEvent(input$level, {
    if (input$level == 'County-wise') {
      shinyjs::show("scope_container")
    } else {
      shinyjs::hide("scope_container")
    }
  })
  
  map_data <- reactiveVal(NULL)
  
  ## Function to calculate all the needed data for the map
  compute_map_data <- function(election_type, year_of_interest) {
    # Select state geometries based on year
    states <- if (year_of_interest < 1863) states_pre_1863 else states_post_1863
    
    ## Selecting the data
    county_wide_data <- county_election_data[[election_type]][[year_of_interest]]
    state_wide_data  <- state_election_data[[election_type]][[year_of_interest]]
    party_cols       <- intersect(names(county_wide_data), names(party_lookup))
    
    #########################
    ###### COUNTY WISE ######
    #########################
    
    ## Joining with the geographic data
    county_map_data <- counties %>%
      mutate(NAME = toupper(NAME)) %>%
      left_join(county_wide_data, by = c("NAME", "STATE_NAME", "GEOID")) %>%
      mutate(STATE = coalesce(STATE, STATE_NAME))
    
    ## Taking some of the data information to be later used in the creating of the tooltip
    votes_mat <- as.matrix(county_map_data[, party_cols] %>% st_drop_geometry())
    total_votes_vec <- county_map_data$total_votes
    winner_codes <- county_map_data$winner_code
    
    ## Creating the whole tooltip
    county_map_data$tooltip <- paste0(
      "<div style='font-size:13px; line-height:1.4; padding:5px;'>",
      "<strong>", coalesce(county_map_data$NAMECOUNTY, county_map_data$NAMELSAD), ", ",
      coalesce(county_map_data$NAMESTATE, county_map_data$STATE_NAME), "</strong><br>",
      ifelse(
        !is.na(total_votes_vec),
        paste0(
          "Winner: <b style='color:", party_color[winner_codes], ";'>", county_map_data$winner, "</b><br>",
          apply(votes_mat, 1, function(votes_row) {
            nonzero <- which(votes_row > 0)
            paste0(
              "<span style='color:", party_color[party_cols[nonzero]], ";'>",
              party_lookup[party_cols[nonzero]], ":</span> ",
              "<span style='color:black;'>",
              round(100 * votes_row[nonzero] / sum(votes_row), 1), "% (",
              format(votes_row[nonzero], big.mark = ",", trim = TRUE), " votes)</span><br>",
              collapse = ""
            )
          })
        ),
        ""
      ),
      "</div>"
    )
    
    ## Calculating the appropriate shade of party color
    county_map_data$fill_col <- shade_party(county_map_data$winner_code, county_map_data$winner_pct)
    
    #########################
    ###### STATE WISE #######
    #########################
    
    ## Joining the state geographic data
    state_map_data <- states %>%
      left_join(state_wide_data, by = c("NAME" = "STATE"))
    
    ## Taking some of the data information to be later used in the creating of the tooltip
    votes_mat_state <- as.matrix(state_map_data[, party_cols] %>% st_drop_geometry())
    total_votes_state <- state_map_data$total_votes
    winner_codes_state <- state_map_data$winner_code
    
    ## Creating the whole tooltip
    state_map_data$tooltip <- paste0(
      "<div style='font-size:13px; line-height:1.4; padding:5px;'>",
      "<strong>", state_map_data$NAME, "</strong><br>",
      ifelse(
        !is.na(total_votes_state),
        paste0(
          "Winner: <b style='color:", party_color[winner_codes_state], ";'>", state_map_data$winner, "</b><br>",
          apply(votes_mat_state, 1, function(votes_row) {
            nonzero <- which(votes_row > 0)
            paste0(
              "<span style='color:", party_color[party_cols[nonzero]], ";'>",
              party_lookup[party_cols[nonzero]], ":</span> ",
              "<span style='color:black;'>",
              round(100 * votes_row[nonzero] / sum(votes_row), 1), "% (",
              format(votes_row[nonzero], big.mark = ",", trim = TRUE), " votes)</span><br>",
              collapse = ""
            )
          })
        ),
        ""
      ),
      "</div>"
    )
    
    ## Calculating the appropriate shade of party color
    state_map_data$fill_col <- shade_party(state_map_data$winner_code, state_map_data$winner_pct)
    
    ## Output of the function
    list(
      county_map_data = county_map_data,
      state_map_data  = state_map_data,
      states = states,
      election_type = election_type,
      year_of_interest = year_of_interest
    )
  }
  
  ## Option selected at the start of the app
  observe({
    default_year <- '1968'
    map_data(compute_map_data('PRES', default_year))
  })
  
  ## Updating the map and the title
  observeEvent(input$update_map, {
    req(input$election_type, input$election_year)
    
    shinyjs::disable("update_map")
    
    map_data(compute_map_data(input$election_type, input$election_year))
    
    map_title_year_type(c(input$election_type,input$election_year))
    
    shinyjs::enable("update_map")
  })
  
  ## Setting up the title of the map
  map_title_year_type <- reactiveVal(c('PRES','1968'))
  
  output$map_title <- renderText({
    req(map_data())
    
    paste0(
      election_names[[map_title_year_type()[[1]]]], 
      " Election — ", map_title_year_type()[[2]])
  })
  
  ## Making the maps
  output$map <- renderLeaflet({
    req(map_data())
    
    if (input$level == "State-wise") {
      if (input$color_mode == 'winner') {
        leaflet(map_data()$state_map_data, options = leafletOptions(attributionControl = FALSE)) %>%
          addPolygons(
            color = "black",
            weight = 0.8,
            fillColor = ~party_pal(winner_code),
            fillOpacity = 1,
            label = lapply(map_data()$state_map_data$tooltip, HTML),
          ) %>% setMaxBounds(lng1 = -140, lat1 = 55, lng2 = -50, lat2 = 15)
      } else {
        leaflet(map_data()$state_map_data, options = leafletOptions(attributionControl = FALSE)) %>%
          addPolygons(
            color = "black",
            weight = 0.8,
            fillColor = ~as.character(fill_col),
            fillOpacity = 1,
            label = lapply(map_data()$state_map_data$tooltip, HTML),
          ) %>% setMaxBounds(lng1 = -140, lat1 = 55, lng2 = -50, lat2 = 15)
      }
    }
    
    else {
      if (input$scope == 'Specific state') {
        county_map_data <- map_data()$county_map_data %>% filter(STATE==input$state)
        states <- map_data()$states %>% filter(NAME==input$state)
      }
      else {
        county_map_data <- map_data()$county_map_data
        states <- map_data()$states
      }
      
      if (input$color_mode == 'winner') {
        leaflet(county_map_data, options = leafletOptions(attributionControl = FALSE)) %>%
          addPolygons(
            color = "black",
            weight = 0.35,
            fillColor = ~party_pal(winner_code),
            fillOpacity = 1,
            label = lapply(county_map_data$tooltip, HTML) 
          ) %>%
          addPolygons(
            data        = states,
            fill        = FALSE,
            color       = "black", weight = 0.8,
            opacity     = 1
          ) %>% setMaxBounds(lng1 = -140, lat1 = 55, lng2 = -50, lat2 = 15)
      } else {
        leaflet(county_map_data, options = leafletOptions(attributionControl = FALSE)) %>%
          addPolygons(
            color = "black",
            weight = 0.35,
            fillColor = ~as.character(fill_col),
            fillOpacity = 1,
            label = lapply(county_map_data$tooltip, HTML)
          ) %>%
          addPolygons(
            data        = states,
            fill        = FALSE,
            color       = "black", weight = 0.8,
            opacity     = 1
          ) %>% setMaxBounds(lng1 = -140, lat1 = 55, lng2 = -50, lat2 = 15)
      }
    }
  })
  
  interactive_map_plot_data <- reactive({
    req(map_data(),input$level,input$scope)
    
    year_of_interest = map_data()$year_of_interest
    election_type = map_data()$election_type
    
    if (input$level == 'State-wise'){
      data_used <- state_election_data[[election_type]][[year_of_interest]]
    }
    else if (input$scope == 'Specific state') {
      data_used <- county_election_data[[election_type]][[year_of_interest]] %>% filter(STATE==input$state)
    }
    else {
      data_used <- county_election_data[[election_type]][[year_of_interest]]
    }
    
    party_columns <- intersect(names(data_used), names(party_lookup))
    
    data_pie <- data_used %>%
      summarise(across(all_of(party_columns), sum)) %>%
      pivot_longer(everything(), names_to = "party", values_to = "votes") %>% filter(votes > 0) %>%
      mutate(party=factor(party,levels=party_order),
             color = party_pal(party),
             pct = votes/sum(votes)) %>%
      arrange(party)
    
    data_bar <- data_used %>%
      dplyr::count(winner_code) %>%
      mutate(party=factor(winner_code,levels=party_order),
             color = party_pal(party),
             pct = n/sum(n)) %>% arrange(party)
    
    list(data_pie=data_pie,data_bar=data_bar)
  })
  
  output$interactive_map_pie_plot <- renderPlotly({
    req(interactive_map_plot_data())
    
    plot_ly(
      data = interactive_map_plot_data()$data_pie,
      sort = FALSE,
      rotation = -40,
      labels = ~party,
      values = ~votes,
      type = "pie",
      insidetextorientation = "radial",
      marker = list(colors = ~color,line = list(color = 'black', width = 1)),
      textinfo = "none",
      hoverinfo = "text",
      hovertext = ~paste0(
        "<b>Party:</b> ", party_lookup[party],
        "<br><b>Votes Total:</b> ", format(votes, big.mark = ","),
        "<br><b>Share:</b> ", round(pct * 100, 1), "%"
      )
    ) %>%
      layout(
        title = "Votes total",
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 0, r = 0, t = 30, b = 10),
        annotations = list()
    ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$interactive_map_bar_plot <- renderPlotly({
    req(interactive_map_plot_data(),input$level)
    
    if (input$level == 'State-wise') {
      plot_ly(
        data = interactive_map_plot_data()$data_bar,
        x = ~n,
        y = ~'',
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~color,
          line = list(color = "black", width = 1)
        ),
        hoverinfo = "text",
        hovertext = ~paste0(
          "<b>Party:</b> ", party_lookup[party],
          "<br><b>Number of Claimed States:</b> ", n,
          "<br><b>Share:</b> ", round(pct * 100, 1), "%"
        )
        
      ) %>%
        layout(
          title = "Claimed States",
          barmode = "stack",
          xaxis = list(title = "",visible = FALSE),
          yaxis = list(title = "",visible = FALSE),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 0, r = 0, t = 30, b = 0),
          annotations = list()
      ) %>%
        config(displayModeBar = FALSE)
    } else {
      plot_ly(
        data = interactive_map_plot_data()$data_bar,
        x = ~n,
        y = ~'',
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~color,
          line = list(color = "black", width = 1)
        ),
        hoverinfo = "text",
        hovertext = ~paste0(
          "<b>Party:</b> ", party_lookup[party],
          "<br><b>Number of Claimed Counties:</b> ", n,
          "<br><b>Share:</b> ", round(pct * 100, 1), "%"
        )
        
      ) %>%
        layout(
          title = "Claimed Counties",
          barmode = "stack",
          xaxis = list(title = "",visible = FALSE),
          yaxis = list(title = "",visible = FALSE),
          showlegend = FALSE,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 0, r = 0, t = 30, b = 0),
          annotations = list()
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  #######################
  ##### Predictions #####
  #######################
  
  ## Getting the geographic data
  states_prediction_map <- reactiveVal(states_prediction %>% mutate(color = '#E6E6E6'))
  
  ## Rendering the map inspired by 270toWin
  output$prediction_map <- renderLeaflet({
    leaflet(states_prediction_map(),
            options = leafletOptions(
              dragging = FALSE,
              zoomControl = FALSE,
              scrollWheelZoom = FALSE,
              doubleClickZoom = FALSE,
              boxZoom = FALSE,
              touchZoom = FALSE,
              attributionControl = FALSE)
    ) %>%
      addPolygons(
        color = 'black',
        weight = 0.8,
        fillColor = ~color,
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
  })
  
  ## Setting up value which is used when we select a given color
  map_coloring_value <- reactiveVal(0)
  
  ## When the map is clicked we accordingly paint the map
  observeEvent(input$prediction_map_shape_click, {
    clicked_state_id <- input$prediction_map_shape_click$id
    
    new_states_prediction_map <- states_prediction_map() %>%
      mutate(color = ifelse(STUSPS %in% clicked_state_id, color_switching(color,map_coloring_value()), color))
    
    states_prediction_map(new_states_prediction_map)
  })
  
  ## Rendering the buttons that change with the different coloring value
  output$coloring_box <- renderUI({
    prediction_coloring_box(map_coloring_value())
  })
  
  ## Observe events which are used to adjust the value used for coloring
  observeEvent(input$btn_safe_d, {
    if (map_coloring_value() == 1) {
      map_coloring_value(0)
    } else {
      map_coloring_value(1)
    }
  })
  
  observeEvent(input$btn_safe_r, {
    if (map_coloring_value() == 2) {
      map_coloring_value(0)
    } else {
      map_coloring_value(2)
    }
  })
  
  observeEvent(input$btn_likely_d, {
    if (map_coloring_value() == 3) {
      map_coloring_value(0)
    } else {
      map_coloring_value(3)
    }
  })
  
  observeEvent(input$btn_likely_r, {
    if (map_coloring_value() == 4) {
      map_coloring_value(0)
    } else {
      map_coloring_value(4)
    }
  })
  
  observeEvent(input$btn_lean_d, {
    if (map_coloring_value() == 5) {
      map_coloring_value(0)
    } else {
      map_coloring_value(5)
    }
  })
  
  observeEvent(input$btn_lean_r, {
    if (map_coloring_value() == 6) {
      map_coloring_value(0)
    } else {
      map_coloring_value(6)
    }
  })
  
  observeEvent(input$btn_tilt_d, {
    if (map_coloring_value() == 7) {
      map_coloring_value(0)
    } else {
      map_coloring_value(7)
    }
  })
  
  observeEvent(input$btn_tilt_r, {
    if (map_coloring_value() == 8) {
      map_coloring_value(0)
    } else {
      map_coloring_value(8)
    }
  })
  
  ## Reset button resets the whole map
  observeEvent(input$btn_reset, {
    states_prediction_map(states_prediction_map() %>% mutate(color = '#E6E6E6'))
  })
  
  ## This is chunk of data which is better explained in file 'model.R'
  compute_prediction_map_data <- function(states_prediction_data) {
    state_means <- states_prediction_data %>% st_drop_geometry() %>%
      mutate(category = color_decoding(color)) %>% select(NAME,category)
    
    data1972 <- bind_cols(data1972,pivot_wider(state_means,names_from = NAME, values_from = category)) %>%
      mutate(across(where(is.logical), as.numeric))
    
    data1972[['election_type']] <- factor(data1972[['election_type']],levels = c("CONG", "GOV", "PRES", "SEN"))
    data1972['coef'] <- 0
    
    data1972$this_state <- as.matrix(data1972[, states$NAME])[
      cbind(seq_len(nrow(data1972)), match(data1972$STATE, states$NAME))]
    
    states_pct_goal <- data1972[1,states_prediction$NAME] %>%
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
    
    testing_data <- as.data.table(data1972)
    
    target_col <- "coef"
    ignore_cols <- c('ICPSRCTY','ICPSRNAM','dem','rep','inflated_total','oth_pct','year')
    
    test_task <- TaskRegr$new(
      id = "county_election_pred_test",
      backend = testing_data,
      target = target_col
    )
    test_task$select(feature_cols)
    
    predictions <- xgboost$predict(test_task)
    
    test_pred <- testing_data %>% select(NAME,STATE_NAME,GEOID,STATE_NAME,total)
    test_pred$coef_pred <- predictions$response
    
    test_pred <- test_pred %>%
      mutate(pred = (1.01*exp(coef_pred) - 0.01) / (exp(coef_pred) + 1)) %>%
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
    
    test_pred <- counties %>% mutate(NAME = toupper(NAME)) %>% inner_join(test_pred, by = c("NAME","STATE_NAME","GEOID"))
    
    test_pred <- test_pred %>%
      mutate(fill_col_pred = ifelse(pred > 0.5, shade_party_rep_dem_only('100',pred),shade_party_rep_dem_only('200',1-pred)))
    
    counties_prediction_map <- ggplot(test_pred) +
      geom_sf(aes(fill = fill_col_pred), color = "black", size = 0.35) +
      scale_fill_identity() +
      theme_minimal() + 
      geom_sf(
        data = states,
        fill = NA,
        color = "black",
        size = 0.8
      )
    
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
                               shade_party_rep_dem_only_states_version("100", pct),
                               shade_party_rep_dem_only_states_version("200", 1 - pct))
      )
    
    test_pred_states <- states_prediction %>% left_join(test_pred_states,by=c('NAME'='STATE_NAME'))
    
    states_prediction_map <- ggplot(test_pred_states) +
      geom_sf(aes(fill = fill_col_pred), color = "black", size = 0.8) +
      scale_fill_identity() +
      theme_minimal() +
      geom_text(data = map_text %>% filter(STUSPS != 'HI'), aes(x = lon, y = lat, label = label),
                size = 3.4, color = "white", fontface = "bold",family = "sans") +
      geom_text(data = map_text %>% filter(STUSPS == 'HI'), aes(x = lon, y = lat, label = label),
                size = 3.4, color = "black", fontface = "bold",family = "sans") +
      labs(x = "",y = "")
    
    list(
      counties_prediction_map = counties_prediction_map,
      states_prediction_map = states_prediction_map,
      pie_chart_data = test_pred_states
    )
  }
  
  prediction_map_data <- reactiveVal(NULL)
  
  ## Button which initiate the prediction
  observeEvent(input$btn_calculate, {
    
    shinyjs::disable("btn_calculate")
    
    prediction_map_data(compute_prediction_map_data(states_prediction_map()))
    
    shinyjs::enable("btn_calculate")
  })
  
  ## Rendering the plots
  output$counties_prediction_map <- renderPlot({
    req(prediction_map_data())
    prediction_map_data()$counties_prediction_map
  })
  
  output$states_prediction_map <- renderPlot({
    req(prediction_map_data())
    prediction_map_data()$states_prediction_map
  })
  
  ## Creating the first pie plot
  output$ggplot_1 <- renderPlot({
    req(states_prediction_map())
    
    pie_chart_data <- states_prediction_map()
    
    pie_chart_data$margin = margin_decoding(pie_chart_data$color)
    pie_chart_data$party = ifelse(pie_chart_data$margin=='NONE','NONE_2', ifelse(grepl("DEMOCRATIC",pie_chart_data$margin),"DEMOCRATIC","REPUBLICAN"))
    
    pie_chart_data <- pie_chart_data %>% st_drop_geometry() %>% left_join(map_text %>% filter(!is.na(electors)) %>% select(STUSPS,electors),by='STUSPS')
    
    party_data <- pie_chart_data %>%
      group_by(party) %>% summarise(electors = sum(electors)) %>% mutate(level = "Party") %>% rename(NAME = party)
    
    margin_data <- pie_chart_data %>%
      group_by(margin) %>% summarise(electors = sum(electors)) %>% mutate(level = "Margin") %>% rename(NAME = margin)
    
    plot_data <- bind_rows(party_data, margin_data)
    plot_data$NAME <- factor(
      plot_data$NAME,
      levels = c("DEMOCRATIC", "NONE_2" ,"REPUBLICAN",
                 "SAFE DEMOCRATIC", "LIKELY DEMOCRATIC", "LEAN DEMOCRATIC", "TILT DEMOCRATIC", "NONE",
                 "TILT REPUBLICAN", "LEAN REPUBLICAN", "LIKELY REPUBLICAN", "SAFE REPUBLICAN"))
    plot_data$ring <- factor(plot_data$level, levels = c("Margin", "Party"))
    plot_data <- plot_data %>%
      arrange(NAME) %>%
      group_by(ring) %>%
      mutate(ymax = cumsum(electors),
             ymin = ymax - electors,
             mid = 538 - (ymax + ymin) / 2)
    
    suppressWarnings(ggplot(plot_data, aes(x = ring, y = electors, fill = NAME)) +
      geom_col(color = "white", width = 2, just = -0.7, linewidth = 1.5) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = custom_colors) +
      theme_void() +
      theme(legend.position = "none") +
      geom_text(data = plot_data %>% filter(ring=='Party'), aes(y = mid, label = electors, x = 4.9), color = "black", size = 5, fontface = "bold") +
      geom_text(data = plot_data %>% filter(ring=='Margin'), aes(y = mid, label = electors, x = 3.4), color = "black", size = 5, fontface = "bold"))
  })
  
  ## Creating the second pie plot
  output$ggplot_2 <- renderPlot({
    req(prediction_map_data())
    
    pie_chart_data <- prediction_map_data()$pie_chart_data
    
    pie_chart_data$margin = margin_decoding(pie_chart_data$fill_col_pred)
    pie_chart_data$party = ifelse(pie_chart_data$margin=='NONE','NONE_2', ifelse(grepl("DEMOCRATIC",pie_chart_data$margin),"DEMOCRATIC","REPUBLICAN"))
    
    pie_chart_data <- pie_chart_data %>% st_drop_geometry() %>% left_join(map_text %>% filter(!is.na(electors)) %>% select(STUSPS,electors),by='STUSPS')
    
    party_data <- pie_chart_data %>%
      group_by(party) %>% summarise(electors = sum(electors)) %>% mutate(level = "Party") %>% rename(NAME = party)
    
    margin_data <- pie_chart_data %>%
      group_by(margin) %>% summarise(electors = sum(electors)) %>% mutate(level = "Margin") %>% rename(NAME = margin)
    
    plot_data <- bind_rows(party_data, margin_data)
    plot_data$NAME <- factor(
      plot_data$NAME,
      levels = c("DEMOCRATIC", "NONE_2" ,"REPUBLICAN",
                 "SAFE DEMOCRATIC", "LIKELY DEMOCRATIC", "LEAN DEMOCRATIC", "TILT DEMOCRATIC", "NONE",
                 "TILT REPUBLICAN", "LEAN REPUBLICAN", "LIKELY REPUBLICAN", "SAFE REPUBLICAN"))
    plot_data$ring <- factor(plot_data$level, levels = c("Margin", "Party"))
    plot_data <- plot_data %>%
      arrange(NAME) %>%
      group_by(ring) %>%
      mutate(ymax = cumsum(electors),
             ymin = ymax - electors,
             mid = 538 - (ymax + ymin) / 2)
    
    suppressWarnings(ggplot(plot_data, aes(x = ring, y = electors, fill = NAME)) +
                       geom_col(color = "white", width = 2, just = -0.7, linewidth = 1.5) +
                       coord_polar(theta = "y") +
                       scale_fill_manual(values = custom_colors) +
                       theme_void() +
                       theme(legend.position = "none") +
                       geom_text(data = plot_data %>% filter(ring=='Party'), aes(y = mid, label = electors, x = 4.9), color = "black", size = 5, fontface = "bold") +
                       geom_text(data = plot_data %>% filter(ring=='Margin'), aes(y = mid, label = electors, x = 3.4), color = "black", size = 5, fontface = "bold"))
  })
  
  ######################
  ###### Analysis ######
  ######################
  
  ## UI for county selection filter
  output$analysis_county_ui <- renderUI({
    req(input$analysis_state)
    
    if (input$analysis_state == '-') {
      county_choices = c('-')
    } else {
      county_choices <- c('-',analysis_all_counties %>% filter(NAMESTATE==input$analysis_state) %>% arrange(NAMECOUNTY) %>% pull(NAMECOUNTY))
    }
    
    selectizeInput(
      "analysis_county", "Select County",
      choices = county_choices,
      selected = "-"
    )
  })
  
  ## Obsolete map which could be used when selecting a given county (still works but was unable to fit in the dashboard)
  output$analysis_map <- renderLeaflet({
    req(input$analysis_state,input$analysis_county)
    
    if (input$analysis_state != '-') {
      analysis_map_data <- counties %>% filter(STATE_NAME == input$analysis_state)
      
      analysis_map_data <- analysis_map_data %>%
        mutate(fill_color = ifelse(NAMELSAD==input$analysis_county,'#3584C2','#66B3FF'),
               line_size = ifelse(NAMELSAD==input$analysis_county,2.5,0.8),
               line_color = ifelse(NAMELSAD==input$analysis_county,"#8C3F8C","black")
        )
      
      leaflet(analysis_map_data,options = leafletOptions(
        zoomControl = FALSE)) %>%
        addPolygons(
          color = ~line_color,
          weight = ~line_size,
          fillColor = ~fill_color,
          fillOpacity = 1,
          label = ~NAMELSAD,
          layerId = ~NAMELSAD
        )
    } else {
      leaflet(states,options = leafletOptions(
        zoomControl = FALSE)) %>%
        addPolygons(
          color = "black",
          weight = 0.8,
          fillColor = "#66B3FF",
          fillOpacity = 1,
          label = ~NAME,
          layerId = ~NAME
        )
    }
  })
  
  ## Calculating the data for the plots in this section
  plots_data <- reactive({
    req(input$analysis_state,input$analysis_county,input$year_range,input$analysis_election_type)
    
    ## Filtering if possible
    big_plot_data <- data_for_analysis %>%
      { if (input$analysis_state != "-") filter(., NAMESTATE == input$analysis_state) else . } %>%
      { if (input$analysis_county != "-") filter(., NAMECOUNTY == input$analysis_county) else . } %>%
      { if (input$analysis_election_type != "ALL") filter(., ELECT_OFFICE == input$analysis_election_type) else . } %>%
      filter(YEAR >= input$year_range[1],YEAR <= input$year_range[2])
    
    ## Getting the data for the bottom plot
    big_plot_data <- big_plot_data %>%
      mutate(PARTY_CODE = as.character(PARTY_CODE),YEAR=as.factor(YEAR)) %>%
      group_by(PARTY_CODE, YEAR) %>%
      summarise(VOTES = sum(VOTES), .groups = "drop") %>% arrange(YEAR)
    
    big_plot_data <- big_plot_data %>%
      group_by(YEAR) %>%
      mutate(
        vote_pct = VOTES / sum(VOTES),
        color = party_pal(PARTY_CODE)
      ) %>% ungroup()
    
    ## Getting the data for the first pie plot
    df_plot_pie1 <- big_plot_data %>% group_by(YEAR) %>%
      mutate(winner = (VOTES == max(VOTES))) %>% filter(winner == TRUE) %>%
      group_by(PARTY_CODE) %>%
      summarise(
        n_election_victory = sum(as.numeric(winner)),
        color = first(color)) %>%
      mutate(
        PARTY_CODE = factor(PARTY_CODE, levels = party_order),
        pct = n_election_victory/sum(n_election_victory)) %>%
      arrange(PARTY_CODE)
    
    ## Getting the data for the second pie plot
    df_plot_pie2 <- big_plot_data %>% group_by(PARTY_CODE) %>%
      summarise(total_votes = sum(VOTES),color = first(color)) %>%
      mutate(
        PARTY_CODE = factor(PARTY_CODE, levels = party_order),
        pct = total_votes/sum(total_votes)) %>%
      arrange(PARTY_CODE)
    
    ## Getting the data for the total votes plot
    df_total_votes <- big_plot_data %>% group_by(YEAR) %>%
      summarise(total_votes = sum(VOTES))
    
    list(big_plot_data = big_plot_data,
         df_plot_pie1 = df_plot_pie1,
         df_plot_pie2 = df_plot_pie2,
         df_total_votes = df_total_votes)
  })
  
  ## Another chunk of obsolete code used when selecting give county or state in the map
  observeEvent(input$analysis_map_shape_click, {
    req(input$analysis_state)
    
    if (input$analysis_state != '-') {
      clicked_county_id <- input$analysis_map_shape_click$id
      
      updateSelectizeInput(session,"analysis_county",
                           selected = clicked_county_id)
    } else {
      clicked_state_id <- input$analysis_map_shape_click$id
      
      updateSelectInput(session,"analysis_state",
                           selected = clicked_state_id)
    }
  })
  
  ## Rendering the bottom plot
  output$analysis_big_elections_plot <- renderPlotly({
    req(plots_data())
    
    plot <- plot_ly()
    
    for(party in party_order) {
      df_party <- plots_data()$big_plot_data %>% filter(PARTY_CODE == party)
      
      if(nrow(df_party) > 0){
        plot <- add_trace(
          plot,
          data = df_party,
          x = ~YEAR,
          y = ~vote_pct,
          type = 'bar',
          name = party_lookup[party],
          marker = list(
            color = unique(df_party$color),
            line = list(color = 'black', width = 1)
          ),
          text = ~ifelse(vote_pct >= 0.05, party_lookup[PARTY_CODE], ""),
          textposition = 'inside',
          textfont = list(size = 30, color = "white"),
          textangle = 90,
          hovertext = ~paste0(
            "<b>Party:</b> ", party_lookup[PARTY_CODE],
            "<br><b>Year:</b> ", YEAR,
            "<br><b>Votes:</b> ", scales::comma(VOTES),
            "<br><b>Share:</b> ", round(vote_pct*100, 1), "%"
          ),
          hoverinfo = 'text'
        )
      }
    }
    
    plot <- layout(
      plot,
      title = "",
      xaxis = list(title = "Year",fixedrange = TRUE),
      yaxis = list(
        title = "",
        range = c(0, 1),
        tick0 = 0,
        dtick = 0.1,
        tickformat = ".0%",
        fixedrange = TRUE
      ),
      barmode = 'stack',
      showlegend = FALSE,
      bargap = 0,
      shapes = list(
        list(
          type = "line",
          x0 = 0, x1 = 1,
          xref = "paper",
          y0 = 0.5, y1 = 0.5,
          line = list(color = "white", width = 1.5),
          layer = "above")
      )
    )
    
    plot
  })
  
  ##Rendering the first pie plot
  output$analysis_piechart_1 <- renderPlotly({
    req(plots_data())
    
    plot_ly(
      data = plots_data()$df_plot_pie1,
      sort = FALSE,
      labels = ~party_lookup[PARTY_CODE],
      values = ~n_election_victory,
      type = 'pie',
      insidetextorientation = 'radial',
      marker = list(colors = ~color, line = list(color = 'black', width = 1)),
      
      # Text on the pie
      text = ~paste0(party_lookup[PARTY_CODE], " - ", n_election_victory),
      textinfo = 'text',
      textfont = list(size = 8),
      
      # Hover info
      hoverinfo = 'text',
      hovertext = ~paste0(
        "<b>Party:</b> ", party_lookup[PARTY_CODE],
        "<br><b>Number of Victories:</b> ", n_election_victory,
        "<br><b>Share:</b> ", round(pct * 100, 1), "%"
      )
    ) %>%
      layout(showlegend = FALSE)
    })
  
  ##Rendering the second pie plot
  output$analysis_piechart_2 <- renderPlotly({
    req(plots_data())
    
    plot_ly(
      data = plots_data()$df_plot_pie2,
      sort = FALSE,
      labels = ~PARTY_CODE,
      values = ~total_votes,
      type = "pie",
      insidetextorientation = "radial",
      marker = list(colors = ~color,line = list(color = 'black', width = 1)),
      
      text = ~ifelse(pct>0.004,paste0(party_lookup[PARTY_CODE], " - ",round(pct * 100, 1),"%"),''),
      textinfo = "text",
      textfont = list(size = 8),
      
      hoverinfo = "text",
      hovertext = ~paste0(
        "<b>Party:</b> ", party_lookup[PARTY_CODE],
        "<br><b>Votes Total:</b> ", format(total_votes, big.mark = ","),
        "<br><b>Share:</b> ", round(pct * 100, 1), "%"
      )
    ) %>%
      layout(
        title = "",
        showlegend = FALSE
      )
  })
  
  ##Rendering the total votes plot
  output$analysis_total_votes <- renderPlotly({
    req(plots_data())
    
    plot_ly(
      data = plots_data()$df_total_votes,   # your tibble
      x = ~YEAR,
      y = ~total_votes,
      type = 'bar',
      marker = list(color = '#3C8DBC', line = list(color = 'black', width = 1)),
      hoverinfo = "text",
      hovertext = ~paste0(
        "<b>Year:</b> ", YEAR,
        "<br><b>Total Votes:</b> ", scales::comma(total_votes)
      )
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Year",fixedrange = TRUE),
        yaxis = list(title = "Total Votes",fixedrange = TRUE)
      )
  })
  
  #####################
  ####### Table #######
  #####################
  
  ## Creating the data for the table and making them fancy
  output$table <- renderDT({
    data_to_display <- data_for_analysis
    data_to_display <- data_to_display %>% mutate(PARTY_CODE = as.character(PARTY_CODE))
    data_to_display$PARTY <- party_lookup[data_to_display$PARTY_CODE]
    data_to_display <- data_to_display %>%
      mutate(
        ELECT_OFFICE = case_when(
          ELECT_OFFICE == 'PRES' ~ 'Presidential',
          ELECT_OFFICE == 'GOV' ~ 'Gubernatorial',
          ELECT_OFFICE == 'CONG' ~ 'Congressional',
          ELECT_OFFICE == 'SEN' ~ 'Senate'
        ),
        ELECT_OFFICE = factor(ELECT_OFFICE,levels = c('Presidential','Gubernatorial','Congressional','Senate'))
      )
    
    data_to_display <- data_to_display %>% rename(
      County    = NAMECOUNTY,
      State     = NAMESTATE,
      Year      = YEAR,
      Office    = ELECT_OFFICE,
      Party     = PARTY,
      PartyCode = PARTY_CODE,
      Votes     = VOTES
    ) %>% select(Year,State,County,Office,Party,Votes)
    
    data_to_display %>% arrange(-Year,Office,State,County,Party)
    
    datatable(
      data_to_display,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons'
    )
  })
  
}
