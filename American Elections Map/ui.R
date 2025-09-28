
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
              ),
              absolutePanel(
                bottom = 10, left = 5, width = 220, draggable = FALSE, fixed = TRUE,
                style = "background-color: rgba(255, 255, 255, 0.5); 
             padding: 5px; border-radius: 5px; 
             z-index: 1000;",
                "© 2025 Created by Martin Grätzer."
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
          bottom = 10, right = 10, width = 300, fixed = TRUE, draggable = FALSE,
          style = "background-color: rgba(230, 242, 255, 0.9);
           padding: 10px 15px;
           border-radius: 8px;
           box-shadow: 0 2px 8px rgba(0,0,0,0.15);
           font-size: 13px;
           text-align: left;
           font-family: 'Segoe UI', sans-serif;",
          HTML("
            <b>Interactive Map</b>
            <ul style='margin: 5px 0 0 18px; padding: 0;'>
              <li>Hover over counties or states to see tooltips with vote breakdowns.</li>
              <li>Colors show the winning party and vote share.</li>
              <li>Use filters in the sidebar to adjust <b>Year</b>, <b>Election Type</b>, and <b>Scope</b>.</li>
            </ul>
          ")
        ),
        
        absolutePanel(
          top = 60, right = 10, width = 300, draggable = FALSE, fixed = TRUE,
          style = "background-color: rgba(230, 242, 255, 0.9);
           padding: 10px 15px;
           border-radius: 8px;
           box-shadow: 0 2px 8px rgba(0,0,0,0.15);
           font-size: 13px;
           text-align: left;
           font-family: 'Segoe UI', sans-serif;",
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
          div(id = "state_container", style = "display:none;", uiOutput("state_ui")),
          plotlyOutput("interactive_map_pie_plot", height = 150),
          plotlyOutput("interactive_map_bar_plot", height = 50)
        ),
        
        absolutePanel(
          bottom = 10, left = 5, width = 220, draggable = FALSE, fixed = TRUE,
          style = "background-color: rgba(255, 255, 255, 0.5); 
             padding: 5px; border-radius: 5px; 
             z-index: 1000;",
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
              title = "Paint the 1972 Election Map",
              status = "primary",
              leafletOutput("prediction_map", height = 450),
              style = "min-height: 100px;"
            )
          ),
          column(
            width = 6,
            wellPanel(
              style = "background-color: rgba(230, 242, 255, 0.9);
                   padding: 10px 15px;
                   border-radius: 8px;
                   box-shadow: 0 2px 8px rgba(0,0,0,0.15);
                   font-size: 13px;
                   text-align: left;
                   font-family: 'Segoe UI', sans-serif;",
              HTML("
                <b>1972 Presidential Election Prediction</b>
                <ul>
                  <li>Use the color selection box to paint the map on the left with <b>Safe/Likely/Lean/Tilt</b> options for Democrats or Republicans.</li>
                  <li>Empty states will be predicted automatically by the <b>XGBoost</b> model.</li>
                  <li>Clicking <b>'Calculate'</b> will generate predictions for the 1972 Presidential Election.</li>
                  <li>Below, view the Electoral College results along with state- and county-level maps based on your predictions.</li>
                </ul>
              ")
            ),
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
                                 ,plotOutput("ggplot_1", height = 250)),
                          column(width = 6,
                                 tags$h2("Predicted Outcome", style = "text-align: center; margin-bottom: 5px;"),
                                 plotOutput("ggplot_2", height = 250))
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
          bottom = 10, left = 5, width = 220, draggable = FALSE, fixed = TRUE,
          style = "background-color: rgba(255, 255, 255, 0.5); 
             padding: 5px; border-radius: 5px; 
             z-index: 1000;",
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
                  wellPanel(
                      style = "background-color: rgba(230, 242, 255, 0.9);
                         padding: 10px 15px;
                         border-radius: 8px;
                         box-shadow: 0 2px 8px rgba(0,0,0,0.15);
                         font-size: 13px;
                         text-align: left;
                         font-family: 'Segoe UI', sans-serif;",
                      HTML("
                      <b>Analytics Overview</b>
                      <ul>
                        <li>Explore vote distributions and trends across states, counties, and parties over time (choosing '-' includes the entire state or country).</li>
                        <li>The main chart at the bottom shows how party vote shares have shifted across elections.</li>
                        <li>Additional charts highlight victories and vote totals for parties or overall for the selected region.</li>
                      </ul>
                    ")
                  ),
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
                bottom = 10, left = 5, width = 220, draggable = FALSE, fixed = TRUE,
                style = "background-color: rgba(255, 255, 255, 0.5); 
             padding: 5px; border-radius: 5px; 
             z-index: 1000;",
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
                bottom = 10, left = 5, width = 220, draggable = FALSE, fixed = TRUE,
                style = "background-color: rgba(255, 255, 255, 0.5); 
             padding: 5px; border-radius: 5px; 
             z-index: 1000;",
                "© 2025 Created by Martin Grätzer."
              )
      )
    )
  )
)
