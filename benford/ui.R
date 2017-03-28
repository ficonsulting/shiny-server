
shinyUI(
  navbarPage("Benford's Law", theme = "readable.min.css",
    tabPanel("About", icon = icon('info-circle'), 
      includeHTML('benford.html')),
    tabPanel("Analysis", icon = icon('bar-chart'), 
             
  fluidPage(
  
  fluidRow(
    column(width = 4,
           selectInput("dataset",
                       "Dataset:",
                       choices = available_data,
                       selected = 'New York Air Quality Measurements'),
           uiOutput('csv_selector')),
    uiOutput('cols'),
    uiOutput('digs')),
  
  hr(), 
  
  plotOutput('benPlot'),
  
  fluidRow(
    column(width = 4, 
           h3('Suspects Table:'),
           tableOutput('benReport')), 
    
    column(width = 6, 
           h3('Summary Stats:'),
           tableOutput('info')))
  
))))
