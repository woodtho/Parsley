library(tidyverse)
library(shiny)
library(janitor)
library(readxl)
library(scales)
library(shinyjs)

fileInputNoExtra <-
    function(inputId,
             label,
             multiple = FALSE,
             accept = NULL,
             width = NULL,
             buttonLabel = "Browse...",
             placeholder = "No file selected") {
        restoredValue <- restoreInput(id = inputId, default = NULL)
        if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
            warning("Restored value for ", inputId, " has incorrect format.")
            restoredValue <- NULL
        }
        if (!is.null(restoredValue)) {
            restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
        }
        inputTag <-
            tags$input(
                id = inputId,
                name = inputId,
                type = "file",
                style = "display: none;",
                `data-restore` = restoredValue
            )
        if (multiple)
            inputTag$attribs$multiple <- "multiple"
        if (length(accept) > 0)
            inputTag$attribs$accept <- paste(accept, collapse = ",")
        
        tags$label(
            class = "input-group-btn",
            type = "button",
            style = if (!is.null(width))
                paste0(
                    "width: ",
                    validateCssUnit(width),
                    ";",
                    "padding-right: 0px; padding-bottom: 0px;  visible: hidden;"
                ),
            span(
                class = "btn btn-default btn-file",
                type = "button",
                buttonLabel,
                inputTag,
                style = if (!is.null(width))
                    paste0(
                        "width: ",
                        validateCssUnit(width),
                        ";",
                        "border-radius: 0px; padding-bottom: 5px;"
                    )
            )
        )
    }

# Load the score data.
Scores <- read_excel("Data/Scores.xlsx", sheet = "Score") %>%
    mutate(
        # Create the labels used in the score summary
        label = if_else(level == 1, name, type) %>%
            str_to_title() %>%
            fct_inorder(),
        # Because the data is stored in excel, we need to parse the Inf as a
        # numeric value. It defaults to making the var character
        value = if_else(value == "Inf", Inf, parse_number(value))
    ) %>%
    rownames_to_column() %>%
    group_by(game, type) %>%
    # Used for sorting the score categories, so that the largest ones are drawn
    # at the top.
    mutate(n = n(),
           game = fct_inorder(game)) %>%
    arrange(game,-n)

# Import the inventory data
Inventory <- read_excel("Data/Scores.xlsx", sheet = "Inventory")

width_sidebar <- 4

# Define UI
ui <- fluidPage(# Application title
    titlePanel(tagList(
        span("Parsely Score", 
             span(downloadButton('save_inputs', '< Save inputs >', icon = NULL),
                  fileInputNoExtra('load_inputs', NULL, buttonLabel = '< Load inputs >', accept = ".csv"),
                  style = "position:absolute;right:2em;")
        )
    ),
    windowTitle = "Parsely Score"
    ),
    useShinyjs(),
    
    # titlePanel(tagList("Parsely Score") ),
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Filter the data to remove other games
    score_items <- reactive({
        req(input$game)
        
        Scores %>%
            filter(game == input$game) %>%
            group_by(type) %>%
            mutate(n = nrow(.)) %>%
            ungroup() %>%
            # sort the score categories
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
                        .f = ~checkboxInput(
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
    
    
    all_inputs <- reactive({
        x <- reactiveValuesToList(input)
        x$load_inputs <- NA
        x[["shinyjs-resettable-load_inputs"]] <- NA
        tibble(names = names(x),
                   values = unlist(x, use.names = FALSE)) %>% 
            arrange(names)
    })
    
    output$save_inputs <- downloadHandler(
        filename = function() {
            paste(input$game, ".csv", sep="")
        },
        content = function(file) {
            write.csv(all_inputs() %>% 
                          filter(!names %in% c("save_inputs", 
                                               "load_inputs")),
                      file, row.names = FALSE)
        }
    )
    
    score_inputs <- reactive({
        all_inputs() %>%
            mutate(values = parse_logical(values)) %>% 
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
        tagList(map(unique(score_inputs()$score_set), ~
                        renderTable(width = "100%", {
                            score_inputs() %>%
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
                
                if (length(unique(score_inputs()$score_set)) > 1) {
                    renderTable(width = "100%", {
                        score_inputs() %>%
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
                        ' ▲"></input>')),  
                    
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
    
    save_state <- reactive({
        req(input$load_inputs)
        req(input$load_inputs != 1)
        print(input$load_inputs)        
        ext <- tools::file_ext(input$load_inputs$name)
        switch(ext,
               csv = read_csv(input$load_inputs$datapath),
               validate("Invalid file; Please upload a .csv file")
               )
    })
    
    observe({
        
        req(save_state())
        req(input$load_inputs != 1)
        
        updateSelectInput(session = session,
                          inputId = "game",
                          NULL,
                          choices = unique(Scores$game),
                          selected = save_state() %>% filter(names == "game") %>% pull(values))
        
        map(save_state() %>% filter(names != "game") %>% pull(names), ~observe({
            req(input$game)
            
            updateCheckboxInput(inputId = .x, value = save_state() %>% 
                                    filter(names == .x) %>% 
                                    pull(values) %>% 
                                    parse_logical())
        }))
        
        session$sendCustomMessage("load_inputs", 'null')
        session$sendCustomMessage("load_inputs", '1')
        
        })}

# Run the application 
shinyApp(ui = ui, server = server)
