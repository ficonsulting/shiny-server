# ui.R

# Custom Function to Add NavBar Input
navbarPageWithInputs <- function(..., input_view) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", input_view)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

shinyUI(
  
  fluidPage(
    tags$style(type = "text/css", "#xy_plot {height: 38vh !important;}"),
    tags$style(type = "text/css", "#reestimate_plot {height: 38vh !important;}"),
    tags$style(type = "text/css", "#bar_plots {height: 75vh !important;}"),
    tags$style(type = "text/css", "#dumbbell_plots {height: 75vh !important;}"),
    navbarPageWithInputs("FCS Reestimates", theme = "flatly.min.css", id = 'menus',
                         
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
                         input_view = uiOutput('changeView')
    )
  )
  
)


