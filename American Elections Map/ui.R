
ui <- dashboardPage(
  dashboardHeader(
    title = "American Elections Map (AEM)", titleWidth = 350,
    tags$li(class = "dropdown",
            tags$a(href = "https://www.icpsr.umich.edu/web/ICPSR/studies/1",
                   "Source of data",
                   target = "_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName= "introduction", icon = icon("home")),
      menuItem("Interactive Map", tabName = "interactive_map", icon = icon("map")),
      menuItem("Predictions", tabName = "predictions", icon = icon("chart-line")),
      menuItem("Analysis", tabName= "analysis", icon = icon("chart-pie")),
      menuItem("Table", tabName= "tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                box(
                  title = "About", width = 12, solidHeader = TRUE, status = "info",
                  withMathJax(),
                  includeMarkdown("Documentation.Rmd")
                )
              )
      ),
      tabItem(
        tabName = "interactive_map",
        
        leafletOutput("map", width = "90%", height = "92vh"),
        
        absolutePanel(
          top = 60, left = "50%", width = 360, fixed = TRUE, draggable = FALSE,
          style = "transform: translateX(-50%);
                    background-color: rgba(255, 255, 255, 0.85);
                    padding: 10px 20px;
                    border-radius: 12px;
                    box-shadow: 0 4px 12px rgba(0,0,0,0.2);
                    text-align: center;
                    font-family: 'Segoe UI', sans-serif;",
          h3(style = "margin:0; color:#003366; font-weight:600;", 
             textOutput("map_title"))
        ),
        
        
        absolutePanel(
          top = 60, right = 10, width = 300, draggable = FALSE, fixed = TRUE,
          style = "background-color: rgba(173, 216, 230, 0.7); padding: 15px; border-radius: 10px; box-shadow: 2px 2px 10px grey;",
          tags$h3("Map Options"),
          selectInput(
            "election_type", "Type of Election",
            choices = c('Presidential'='PRES','Gubernatorial'='GOV','Congressional'='CONG','Senate'='SEN'),
            selected = 'Presidential'
          ),
          uiOutput("year_ui"),
          div(
            style = "text-align: center; margin-top: 15px;",
            actionButton("update_map", "Update Year/Election Type", icon = icon("refresh"))
          ),
          radioButtons("level", "Map Level", choices = c("State-wise", "County-wise"), selected = "County-wise"),
          radioButtons("color_mode", "Color Counties by", choices = c("Winner only" = "winner", "Winner % of votes" = "percent"), selected = "percent"),
          div(id = 'scope_container',
              radioButtons("scope", "Scope", choices = c("All US", "Specific state"), selected = "All US")
          ),
          div(id = "state_container", style = "display:none;", uiOutput("state_ui"))
        ),
        
        absolutePanel(
          bottom = 30, right = 10, width = 220, draggable = FALSE, fixed = TRUE,
          style = "background-color: rgba(255, 255, 255, 0.5); 
           padding: 5px; border-radius: 5px; 
           z-index: 1000;",  # ensures it's above the map
          "© 2025 Created by Martin Grätzer."
        )
      ),
      tabItem(
        tabName = "predictions",
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL,
              height = "auto",
              solidHeader = TRUE,
              title = "Paint Your Own Map of 1972 Presidential Election",
              status = "primary",
              leafletOutput("prediction_map", height = 450),
              style = "min-height: 100px;"
            )
          ),
          column(
            width = 6,
            fluidRow( uiOutput("coloring_box"),
                      box(
                        width = 9,
                        height = "auto",
                        solidHeader = TRUE,
                        title = "Electoral College Overview",
                        status = "primary",
                        fluidRow(
                          column(width = 6,
                                 tags$h2("Your Picks", style = "text-align: center; margin-bottom: 5px;")
                                 ,plotOutput("ggplot_1", height = 300)),
                          column(width = 6,
                                 tags$h2("Predicted Outcome", style = "text-align: center; margin-bottom: 5px;"),
                                 plotOutput("ggplot_2", height = 300))
                        ),
                        style = "min-height: 100px;"
                      )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              title = "States Prediction Map",
              plotOutput("states_prediction_map", height = 450)
            )
          ),
          column(
            width = 6,
            box(
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              title = "Counties Prediction Map",
              plotOutput("counties_prediction_map", height = 450)
            )
          )
        ),
        absolutePanel(
          bottom = 30, right = 10, width = 220, draggable = FALSE, fixed = TRUE,
          style = "background-color: rgba(255, 255, 255, 0.5); 
           padding: 5px; border-radius: 5px; 
           z-index: 1000;",  # ensures it's above the map
          "© 2025 Created by Martin Grätzer."
        )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                column(
                  width = 3,
                  box(
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    title = "Options",
                    selectInput(
                      "analysis_state", "Select State",
                      choices = c('-',sort(states_post_1863$NAME)),
                      selected = 'Iowa'
                    ),
                    uiOutput("analysis_county_ui"),
                    selectInput(
                      "analysis_election_type", "Select Type of Election",
                      choices = c('All'='ALL','Presidential'='PRES','Gubernatorial'='GOV','Congressional'='CONG','Senate'='SEN'),
                      selected = 'PRES'
                    ),
                    sliderTextInput(
                      inputId = "year_range",
                      label = "Select Years",
                      choices = 1823:1968,
                      selected = c(1823,1968),
                      grid = TRUE
                    ),
                    #leafletOutput("analysis_map", height = 200)
                  )
                ),
                column(
                  width = 9,
                  box(
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    title = "Election Charts",
                    fluidRow(
                      column(width = 4,
                             tags$h2("Election Victories", style = "text-align: center; margin-bottom: 5px;")
                             ,plotlyOutput("analysis_piechart_1", height = 270)),
                      column(width = 4,
                             tags$h2("Party Votes", style = "text-align: center; margin-bottom: 5px;"),
                             plotlyOutput("analysis_piechart_2", height = 270)),
                      column(width = 4,
                             tags$h2("Votes Total", style = "text-align: center; margin-bottom: 5px;"),
                             plotlyOutput("analysis_total_votes", height = 270))
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  box(
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    title = "Election Results",
                    plotlyOutput("analysis_big_elections_plot", height = "700px")
                  )
                )
              ),
              absolutePanel(
                bottom = 30, right = 10, width = 220, draggable = FALSE, fixed = TRUE,
                style = "background-color: rgba(255, 255, 255, 0.5); 
           padding: 5px; border-radius: 5px; 
           z-index: 1000;",  # ensures it's above the map
                "© 2025 Created by Martin Grätzer."
              )
              
              
      ),
      tabItem(tabName = "tables",
              fluidRow(
                box(
                  title = "Election Data", width = 12, solidHeader = TRUE, status = "primary",
                  DTOutput("table")
                )
              ),
              absolutePanel(
                bottom = 30, right = 10, width = 220, draggable = FALSE, fixed = TRUE,
                style = "background-color: rgba(255, 255, 255, 0.5); 
           padding: 5px; border-radius: 5px; 
           z-index: 1000;",  # ensures it's above the map
                "© 2025 Created by Martin Grätzer."
              )
      )
    )
  )
)
