
shinyServer(function(input, output, session) {
  
  df <- reactive({
    req(input$dataset)
    if (input$dataset == 'csv') {
      req(input$file)
      d <- read.csv(input$file$datapath) %>% select_if(is.numeric)
      
    } else {                      
      d <- get_data(input$dataset) %>% select_if(is.numeric)
    }
    
  })
  
  output$csv_selector <- renderUI({
    if (input$dataset == 'csv') {
      
      fileInput('file', 'Choose file to test',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                ))
      
    } else {NULL}
  })
  
  # # More info modal
  # observeEvent(input$morebtn, {
  #   showModal(modalDialog(
  #     includeHTML('benford.html'),
  #     title = "Benford's Theory",
  #     size = 'm', easyClose = T
  #   ))
  # })
  
  output$cols <- renderUI({
    column(width = 4, 
         selectInput('vars', 'Column Tested:',
                     choices = names(df())))
  })
  
  output$digs <- renderUI({
    column(width = 4,
         numericInput('dig', 'Number of Digits:', 1, 1, 4, 1))
  })
  
  ben <- reactive({
    benford(df()[, input$vars], input$dig, discrete = F)
  })
  
  output$info <- renderTable({
    data.frame(
      Total_Obs = ben()$info$n,
      MAD = ben()$MAD,
      Pearsons_Chi_squared = ben()$stats$chisq$p.value,
      Mantissa_Arc = ben()$stats$mantissa.arc.test$p.value)
  })
  
  output$benPlot <- renderPlot({
    plot(ben())
  })
  
  output$benReport <- renderTable({
    head(suspectsTable(ben()))
  })
  
})
