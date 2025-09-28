setwd("~/GitHub/PersonalPortfolio/US Elections/AEM_app")
library(rsconnect)

setAccountInfo(name='maertine',
               token='3737233CEDE7C70DB04992CD9632FC24',
               secret='Y8DihhlFWabrkH5lWG2r2Zdsq4wOktVKsJZUbnec')

deployApp(
  appFiles = c("ui.R", "server.R", "global.R", "data", "Documentation.Rmd"),
  appName = "American-Elections-Map",
  account = "maertine"
)
