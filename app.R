library(tidyverse)
library(shiny)
library(janitor)

Scores <- read_excel("Data/Scores.xlsx", sheet = "Score") %>% 
    mutate(label = if_else(level == 1, name, type) %>% 
               str_to_title())

Inventory <- read_excel("Data/Scores.xlsx", sheet = "Inventory") 

width_sidebar <- 3

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Parsley Score"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = width_sidebar,
            selectInput(
                "game",
                "Select game:",
                choices = unique(Scores$game),
                multiple = FALSE),
            h5(strong("Inventory")),
            uiOutput("inventory"),
            h5(strong("Score Summary")),
            tableOutput('show_inputs')),
        # Show a plot of the generated distribution
        mainPanel(uiOutput("score"))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    score_items <- reactive({
        req(input$game)
        
        Scores %>%
            filter(game == input$game) %>%
            group_by(type) %>%
            mutate(n = nrow(.)) %>%
            ungroup() %>%
            arrange(n)
    })

    output$score <- renderUI({
        map(unique(score_items()$type), function(.x) {
            ids <- filter(score_items(), type == .x) %>%
                pull(id) %>%
                list(id = ., type = rep(.x, times = length(.)))
            
            tagList(column(
                width = ceiling(length(unique(
                    Scores$type
                )) / (12 - width_sidebar) * 10),
                h2(str_to_title(.x)),
                map(
                    ids$id,
                    .f = ~ checkboxInput(
                        paste0("score_",str_replace_all(input$game, " ", "_"),"_", .x),
                        label = pull(score_items()[score_items()$id == .x, "name"]),
                        value = FALSE
                    )
                )
            ))
        })
        
        
    })
    
    
    AllInputs <- reactive({
        x <- reactiveValuesToList(input)
        data.frame(names = names(x),
                   values = unlist(x, use.names = FALSE) %>% parse_logical()) %>% 
            filter(str_detect(names, paste0("^score_", str_replace_all(input$game, " ", "_"), "_[0-9]"))) %>% 
            mutate(id = parse_number(names)) %>%
            left_join(Scores) %>%
            group_by(label) %>%
            summarize(Score = sum(values * value, na.rm = TRUE)) %>%
            adorn_totals()
    })
    
    output$show_inputs <- renderTable({
        AllInputs()
    })
    
    output$inventory <- renderUI({
        
        game_inventory <- Inventory %>% 
            filter(game == input$game)
        
        inventory_inputs <-  map(unique(game_inventory$id), 
            ~checkboxInput(paste0("inventory_", .x),
                           label = pull(game_inventory[game_inventory$id == .x, "name"]), 
                           value = pull(game_inventory[game_inventory$id == .x, "start_with"]))
            )
        
        fluidRow(column(6, inventory_inputs[c(TRUE, FALSE)]),
                 column(6, inventory_inputs[c(FALSE, TRUE)]))
        
    
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
