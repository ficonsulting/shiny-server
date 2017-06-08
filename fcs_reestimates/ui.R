# ui.R

# Custom Function to Add NavBar Input
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

shinyUI(
  
  navbarPageWithInputs("FCS Reestimates", theme = "readable.min.css", id = 'menus',
             
             # actionButton('infoBut2', 'Information', icon = icon('info-circle', 
             #                                                     lib = 'font-awesome')),
             tabPanel("Analysis", icon = icon('file-text-o', lib = 'font-awesome'), 
                      fluidPage(
                        fluidRow(column(width = 8, offset = 2, 
                                        includeHTML('FCS_Reestimates.html'))
                                 )
                        )
                      ),
             tabPanel("Interactive", icon = icon('bar-chart'), 
                        # tags$style("
                        #            body {
                        #            -moz-transform: scale(.9, .9); /* Moz-browsers */
                        #            zoom: .9; /* Other non-webkit browsers */
                        #            zoom: 90%; /* Webkit browsers */
                        #            }
                        #            "),
                      uiOutput('pageUI')
                      
             ),
             inputs = uiOutput('changeView')
  )

)


