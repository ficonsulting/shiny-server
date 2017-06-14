# ui.R

# Custom Function to Add NavBar Input
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

# Custom Function to get screen resolution
jscode <-
  '$(document).on("shiny:connected", function(e) {
var jsWidth = screen.width;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'


shinyUI(
  
  fluidPage(
    tags$script(jscode),
    uiOutput('zoom_ui'),
    navbarPageWithInputs("FCS Reestimates", theme = "readable.min.css", id = 'menus',
                         
                         tabPanel("Analysis", icon = icon('file-text-o', lib = 'font-awesome'), 
                                  fluidPage(
                                    fluidRow(column(width = 10, offset = 1, 
                                                    includeHTML('FCS_Reestimates.html'))
                                    )
                                  )
                         ),
                         tabPanel("Interactive", icon = icon('bar-chart'),
                                  
                                  uiOutput('pageUI')
                                  
                         ),
                         inputs = uiOutput('changeView')
    )
  )
  
)

