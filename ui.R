#######################################################	
# Define general layout tag list	
#######################################################

headerTagList <- list(	
    tags$style(type = "text/css", ".navbar .navbar-nav {float: right; font-size: 14px} .navbar .navbar-nav li a {font-size: 14px} .nav-tabs {font-size: 12px}"),
    tags$style(type = "text/css", "div.mypptImage img {max-width: 100%; width: 100%; height: auto}"),
    tags$base(target = "_blank")	
)

#######################################################
# Define the full user-interface
#######################################################

ui <- navbarPage(
    title = strong("ProTEOS-X: Project Tracker and Experiment Organizer System for codeX"), selected = "Summary",	
    tabPanel("Summary", homepage, icon = icon("home")),	
    tabPanel("Data Entry", entrypage, icon = icon("clipboard")),	
    tabPanel("Data Export", datapage, icon = icon("info-circle")),	
    header = headerTagList,	
    collapsible = TRUE,	inverse = TRUE,
    windowTitle = "ProTEOS-X"
)

shinyUI(ui)