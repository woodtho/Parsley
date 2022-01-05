library(tidyverse)
library(shiny)
library(janitor)
library(readxl)
library(scales)




# Define UI
ui <- fluidPage(# Application title
  titlePanel(tagList(
    span("Parsely Score", 
         span(
           downloadButton('save_inputs', '< Save inputs >', icon = NULL),
           fileInputNoExtra('load_inputs', NULL, buttonLabel = '< Load inputs >', accept = ".parsely"),
           style = "position:absolute;right:2em;")
    )
  ),
  windowTitle = "Parsely Score"
  ),
  
  # These the the different css and js dependencies for the DOS theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap-grid.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap-reboot.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$script(type = "text/javascript", href = "js/bootstrap.bundle.min.js"),
    tags$script(type = "text/javascript", href = "js/bootstrap.min.js"),
    tags$script(type = "text/javascript", href = "js/alert.js"),
    tags$script(type = "text/javascript", href = "js/button.js"),
    tags$script(type = "text/javascript", href = "js/carousel.js"),
    tags$script(type = "text/javascript", href = "js/collapse.js"),
    tags$script(type = "text/javascript", href = "js/dropdown.js"),
    tags$script(type = "text/javascript", href = "js/index.js"),
    tags$script(type = "text/javascript", href = "js/modal.js"),
    tags$script(type = "text/javascript", href = "js/popover.js"),
    tags$script(type = "text/javascript", href = "js/scrollspy.js"),
    tags$script(type = "text/javascript", href = "js/tab.js"),
    tags$script(type = "text/javascript", href = "js/toast.js"),
    tags$script(type = "text/javascript", href = "js/tooltip.js"),
    tags$script("
        Shiny.addCustomMessageHandler('load_inputs', function(value) {
        Shiny.setInputValue('load_inputs', value);
        });
  ")
  ), 
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      width = width_sidebar, 
      # Wrap the content in the sidebar in a div, so that we can scroll the
      # content and set a max height.
      div(
        style = "max-height: 720px; position:relative; overflow-y: scroll; padding-right: 10px;",
        h4("Select game", style = "color:black;"),
        # select the game
        selectInput(
          "game",
          NULL,
          choices = unique(Scores$game),
          multiple = FALSE
        ),
        htmlOutput("save_slot_counter"),
        br(),
        # Generated UI for the inventory
        h4("Inventory", style = "color:black;"),
        uiOutput("inventory"),
        br(),
        # Generated score summary UI
        h4("Score Summary", style = "color:black; font"),
        # This padding is needed so the tables don't touch the scroll bar
        div(style = "padding-right: 10px;",
            uiOutput('show_inputs')
        )
      )),
    # the extra div is for scroll control.
    mainPanel(div(style = "max-height: 780px; position:relative; overflow-y: scroll; padding-right: 10px;",
                  uiOutput("score")))
  ))