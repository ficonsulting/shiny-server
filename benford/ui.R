
shinyUI(
  fluidPage(theme = shinytheme('journal'),
  
  fluidRow(
    column(width = 4, 
      titlePanel("Benford's Law")),
    column(width = 8, 
      div(actionButton('morebtn', 'More info', icon = icon('info')), align = 'right'))),
  
  fluidRow(
    column(width = 4,
           selectInput("dataset",
                       "Dataset:",
                       choices = available_data,
                       selected = 'New York Air Quality Measurements'),
           uiOutput('csv_selector')),
    uiOutput('cols'),
    uiOutput('digs')),
  
  bsModal('more_modal', "Benford's Theory", 'morebtn', size = 'large',
          includeHTML('benford.html')),
  
  hr(), 
  
  plotOutput('benPlot'),
  
  fluidRow(
    column(width = 4, 
           h3('Suspects Table:'),
           tableOutput('benReport')), 
    
    column(width = 6, 
           h3('Summary Stats:'),
           tableOutput('info')))
  
  
  
))
