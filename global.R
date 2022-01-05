library(tidyverse)
library(shiny)
library(janitor)
library(readxl)
library(scales)

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

red_checkbox_checked <- "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='24' height='14'>
<filter id='red-sunset' x='-10%' y='-10%' width='120%' height='120%' filterUnits='objectBoundingBox' primitiveUnits='userSpaceOnUse' color-interpolation-filters='sRGB'>
  <feColorMatrix type='matrix' values='.33 .33 .33 0 0
                                       .33 .33 .33 0 0
                                       .33 .33 .33 0 0
                                       0 0 0 1 0' 
    in='SourceGraphic' result='colormatrix'/>
  <feComponentTransfer in='colormatrix' result='componentTransfer'>
    <feFuncR type='table' tableValues='0.86'/>
		<feFuncG type='table' tableValues='0.08'/>
		<feFuncB type='table' tableValues='0.24'/>
		<feFuncA type='table' tableValues='0 1'/>
	</feComponentTransfer>
	<feBlend mode='normal' in='componentTransfer' in2='SourceGraphic' result='blend'/>
</filter>
<path fill='#000000' d='M4 10L4 3L6 3L6 2L2 2L2 11L6 11L6 10ZM9 10L14 10L14 4L9 4ZM20 10L18 10L18 11L22 11L22 2L18 2L18 3L20 3Z' filter='url(#red-sunset)'/>
</svg>"



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