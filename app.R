library(tidyverse)
library(shiny)
library(janitor)
library(readxl)
library(scales)

Scores <- read_excel("Data/Scores.xlsx", sheet = "Score") %>%
    mutate(
        label = if_else(level == 1, name, type) %>%
            str_to_title() %>%
            fct_inorder(),
        value = if_else(value == "Inf", Inf, parse_number(value))
    ) %>%
    rownames_to_column() %>%
    group_by(game, type) %>%
    mutate(n = n(),
           game = fct_inorder(game)) %>%
    arrange(game,-n)

Inventory <- read_excel("Data/Scores.xlsx", sheet = "Inventory")

width_sidebar <- 4

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Parsley Score"),
    
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
        tags$script(type = "text/javascript", href = "js/tooltip.js")
    ), 
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = width_sidebar, 
            div(
            style = "max-height: 720px; position:relative; overflow-y: scroll; padding-right: 10px;",
            h4("Select game", style = "color:black;"),
            selectInput(
                "game",
                NULL,
                choices = unique(Scores$game),
                multiple = FALSE
            ),
            h4("Inventory", style = "color:black;"),
            uiOutput("inventory"),
            br(),
            h4("Score Summary", style = "color:black; font"),
            div(style = "padding-right: 10px;",
            uiOutput('show_inputs')
            
            
            )
        )),
        mainPanel(div(style = "max-height: 780px; position:relative; overflow-y: scroll; padding-right: 10px;",
                      uiOutput("score")))
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
        
        map(unique(score_items()$score_set), function(.x) {
            
            .data <- filter(score_items(), score_set == .x)
            .score_set <- .x
            
            tagList(fluidRow(column(12, h1(
                str_to_title(.score_set)
            ))),
            fluidRow(map(unique(.data$type), function(.x, ...) {
                ids <- filter(.data, type == .x) %>%
                    pull(id) %>%
                    list(id = ., type = rep(.x, times = length(.)))
                tagList(column(
                    width = 5,
                    h3(str_to_title(.x)),
                    map(
                        ids$id,
                        .f = ~ checkboxInput(
                            paste0(
                                str_replace_all(.score_set, " |-", "_"),
                                "_score_",
                                str_replace_all(input$game, " |-", "_"),
                                "_",
                                .x
                            ),
                            label = pull(score_items()[score_items()$id == .x, "name"]),
                            value = FALSE
                        )
                    )
                ))
            }, .data, .score_set)))
            
        })
    })
    
    
    AllInputs <- reactive({
        x <- reactiveValuesToList(input)
        data.frame(names = names(x),
                   values = unlist(x, use.names = FALSE) %>% parse_logical()) %>%
            filter(str_detect(names, paste0(
                "score_", str_replace_all(input$game, " |-", "_"), "_[0-9]"
            ))) %>%
            mutate(
                id = parse_number(names),
                score_set = str_extract(names, ".+?(?=_score)") %>% str_replace_all("_", " ")
            ) %>%
            left_join(Scores) %>%
            group_by(score_set, label) %>%
            summarize(Score = sum(values * value, na.rm = TRUE),
                      rowname = max(rowname))
    })

    
    output$show_inputs <- renderUI({
        tagList(map(unique(AllInputs()$score_set), ~
                        renderTable(width = "100%", {
                            AllInputs() %>%
                                filter(score_set == .x) %>%
                                ungroup() %>%
                                select(-score_set) %>%
                                {if(all(is.finite(.$Score))) {
                                    adorn_totals(dat = .) %>% 
                                        mutate(Score = comma(Score, accuracy = 1, suffix = " points"), 
                                               rowname = parse_number(rowname))
                                    } else {
                                        a <- . 
                                        
                                        b <- a %>% 
                                            filter(is.finite(Score)) %>% 
                                            adorn_totals() %>% 
                                            mutate(Score = comma(Score, accuracy = 1, suffix = " points"),
                                                   rowname = parse_number(rowname)) %>% 
                                            mutate(Score_update = if_else(label == "Total", paste0("Infinity + ", Score), Score)) %>% 
                                            select(-Score)
                                           
                                        a %>% 
                                            mutate(Score = if_else(is.finite(Score), comma(Score, accuracy = 1, suffix = " points"), "Priceless"), 
                                                   rowname = parse_number(rowname)) %>%
                                            full_join(b) %>% 
                                            mutate(Score = if_else(is.na(Score_update), Score, Score_update)) %>% 
                                            select(-Score_update)
                                        
                                    }} %>%
                                arrange(rowname) %>%
                                rename_with(recode, "label" = .x, .cols = label) %>%
                                select(-rowname)
                        })),
                
                if (length(unique(AllInputs()$score_set)) > 1) {
                    renderTable(width = "100%", {
                        AllInputs() %>%
                            ungroup() %>%
                            select(-score_set) %>%
                            adorn_totals() %>%
                            mutate(
                                Score = comma(Score, accuracy = 1, suffix = " points"),
                                rowname = parse_number(rowname)
                            ) %>%
                            arrange(rowname) %>%
                            rename_with(recode, "label" =  "Total", .cols = label) %>%
                            mutate(Total = str_pad(Total, width = max(nchar(Total)), pad = " ")) %>%
                            slice_tail(n = 1) %>%
                            select(-rowname)
                    })
                })
        
        
        
        
        
        
    })

    output$inventory <- renderUI({
        
        game_inventory <- Inventory %>% #filter(game == "Adventure Castle")
            filter(game == input$game)
            
            map(unique(game_inventory$rules), function(.x){
                    
                inventory_inputs <-
                    map(
                        unique(game_inventory %>% filter(rules == .x) %>% pull(id)),
                        ~ checkboxInput(
                            paste0("inventory_", .x),
                            label = pull(game_inventory[game_inventory$id == .x, "name"]),
                            value = pull(game_inventory[game_inventory$id == .x, "start_with"])
                        )
                    )
                
                tagList(
                    HTML(paste0(
                        '<input type="button" data-toggle="collapse"  onclick="return change_',
                        str_replace_all(.x, " |-", "_"),
                        '(this);" data-target="#',
                        paste0("collapse_", str_replace_all(.x, " |-", "_")),
                        '" aria-expanded="true" aria-controls="',
                        paste0("collapse_", str_replace_all(.x, " |-", "_")),
                        '" value ="',
                        .x,
                        ' ▲"></input>'
                    )
                    ),  
                    
                    div(class="collapse in", id=paste0("collapse_", str_replace_all(.x, " |-", "_")),
                        fluidRow(column(6, inventory_inputs[c(TRUE, FALSE)]),
                                 column(6, inventory_inputs[c(FALSE, TRUE)]))),
                    tags$script(type = "text/javascript", 
                                href = "js/util.js"),
                    tags$script(type = "text/javascript", 
                    paste0('function change_',str_replace_all(.x, " |-", "_"),'( el ) {
                            if ( el.value === "',.x,' ▲" )
                                el.value = "',.x,' ▼";
                            else
                                el.value = "',.x,' ▲";
                            }')
                    )
                
                
            )})
        
        
        
        
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
