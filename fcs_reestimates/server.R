# server.R
# 
shinyServer(function(input, output, session) {
  
  # ----------------------------------- UI Elements --------------------------------------------# 
  
  # Create Input for Navigation Bar - User can view data by Agency or Program
  
  output$changeView <- renderUI({
    
    if (input$menus == 'Interactive') {
      
      selectInput('view_type', NULL, choices = c('View By Agency' = 'Agency', 
                                                 'View By Program' = 'Program'))
      
    }
    
  })
  
  # Render UI based on input view - Agency or Program
  
  output$pageUI <- renderUI({
    
    validate(need(!is.null(input$view_type), 'Choose View Type'))
    
    if (input$view_type == 'Agency') {
      
      fluidPage(
        fluidRow(
          column(width = 4,
                 fluidRow(column(width = 7, "Select Agency:",
                                 selectInput("agency", NULL, choices = agency_list, selected = 'HUD', width = '100%')),
                          column(width = 5, "How to Use App:",
                                 actionButton('infoBut', 'Information', width = '90%',
                                              icon = icon('info-circle', lib = 'font-awesome')),
                                 bsModal("infoModal", title = NULL, "infoBut", size = "large",
                                         includeMarkdown("www/info.Rmd")))),
                 column(width = 12, 'Select Budget Year:', 
                        sliderInput('fy', NULL, width = '90%',
                                    min = 1998, max = 2018, value = 2018, sep = '', animate = T),
                        h3("Agency Summary"),
                        uiOutput('panel1'),
                        uiOutput('panel2'),
                        uiOutput('panel3'),
                        uiOutput('panel4'))),
          column(width = 7,
                 tabsetPanel(
                   tabPanel('Reestimate Plots', uiOutput('reestimateUI')),
                   tabPanel('Bar Plots', uiOutput('barUI')))
          )
        )
      )
      
      
    } else {
      
      fluidPage(
        fluidRow(
          column(width = 4,
                 column(width = 7,
                        selectInput("agency", "Select Agency:", choices = agency_list,
                                    selected = 'HUD', width = '100%')),
                 column(width = 5, 'How To Use App:',
                        actionButton('infoBut', 'Information', width = '90%', 
                                     icon = icon('info-circle', lib = 'font-awesome')),
                        bsModal("infoModal", title = NULL, "infoBut", size = "large",
                                includeMarkdown("www/info.Rmd"))
                 ),
                 column(width = 12, 'Select Budget Year:', 
                        sliderInput('fy', NULL, width = '90%',
                                    min = 1998, max = 2018, value = 2018, sep = '', animate = T),
                        bsButton("ActOne", label = "Clear Program Selection"),
                        dataTableOutput('program_dt')
                 )
          ),
          column(width = 7,
                 tabsetPanel(
                   tabPanel('Reestimate Plots', uiOutput('reestimateUI')),
                   tabPanel('Bar Plots', uiOutput('barUI')))
          )
        )
      )
      
    }
    
  })
  
  # Render Scatter and Bubble Plots
  
  output$xyUI <- renderUI({
    
    if (input$scatter_radio == 'Scatter' | input$view_type == 'Program') {
      
      plotlyOutput('xy_plot')
      
    } else {
      #   
      plotlyOutput('xy_bubble')
      
    }
    
  })
  
  # Render center column of app - scatter plot and historic reestimate plot
  
  output$reestimateUI <- renderUI({
    
    if (input$view_type == 'Agency') {
      
      column(width = 12,
             column(width = 10,
                    radioButtons('scatter_radio', NULL, 
                                 choices = c('Scatter', 'Bubble'), inline = T)),
             column(width = 12,
                    uiOutput('xyUI'),
                    hr(),
                    plotlyOutput('reestimate_plot'))
      )
      
    } else if (!is.null(program_df())) {
      
      column(width = 12,
             br(),
             uiOutput('xyUI'),
             hr(),
             plotlyOutput('reestimate_plot'))
      
    } else {
      
      column(width = 12,
             htmlOutput('prog_warning'))
    }
    
  })
  
  output$prog_warning <- renderUI({
    tags$html(
      tags$body(
        br(),
        br(),
        br(),
        h2('Select a Program'),
        br(),
        p(h4('Choose an avaliable program from the table to the left along with a budget year 
             to view interactive visualizations.')), 
        br(),
        p(h4('Click the Clear Program Selection button to reset the view or click another value in the table 
             to choose a different program.'))
        )
        )
    
  })
  
  output$prog_warn <- renderUI({
    tags$html(
      tags$body(
        br(),
        br(),
        br(),
        h2('Select a Program'),
        br(),
        p(h4('Choose an avaliable program from the table to the left along with a budget year 
             to view interactive visualizations.')), 
        br(),
        p(h4('Click the Clear Program Selection button to reset the view or click another value in the table 
             to choose a different program.'))
        )
        )
    
  })
  
  
  # Render Bar/Dumbbell Plots
  
  output$barUI <- renderUI({
    
    if (input$view_type == 'Agency') {
      
      column(width = 12,
             uiOutput('barplot_select'),
             uiOutput('barplot_radio'),
             hr(),
             column(width = 12,
                    uiOutput('barplotUI')
                    #uiOutput('typeUI')
             )
      )
      
    } else if (!is.null(program_df())) {
      
      column(width = 12,
             uiOutput('barplot_select'),
             uiOutput('barplot_radio'),
             column(width = 12,
                    uiOutput('barplotUI'))
      )
      
    } else {
      
      column(width = 12,
             htmlOutput('prog_warn'))
      
    }
    
  })
  
  output$barplotUI <- renderUI({
    
    validate(need(!is.null(input$bar_metric), 'Calculating...'))
    
    if (!input$bar_metric %in% c('disb', 'cur_re', 'life_re')) {
      
      plotlyOutput('dumbbell_plots')
      
    } else {
      
      plotlyOutput('bar_plots')
      
    }
    
    
  })
  
  output$barplot_radio <- renderUI({
    
    if (input$view_type == 'Agency') {
      
      column(width = 6,
             selectInput('grp_by', 'Group By:', choices = c('Cohort Year' = 'co_yr', 'Program Name' = 'prog'))
      )
      
    } else {
      
      column(width = 6,
             selectInput('grp_by', 'Group By:', choices = c('Cohort Year' = 'co_yr'))
      )
      
    }
    
  })
  
  output$barplot_select <- renderUI({
    
    if (input$view_type == 'Agency') {
      column(width = 6,
             selectInput('bar_metric', 'Select Metric:', choices = c('Lifetime Reestimates' = 'life_re',
                                                                     'Current Reestimates' = 'cur_re',
                                                                     'Lifetime Disbursements' = 'disb'
             ))
      )
      
    } else {
      
      column(width = 6,
             selectInput('bar_metric', 'Select Metric:', choices = c('Lifetime Reestimates' = 'life_re',
                                                                     'Current Reestimates' = 'cur_re',
                                                                     'Lifetime Disbursements' = 'disb',
                                                                     'Total Subsidy Change' = 'rate_chg',
                                                                     'Subsidy Change - Technical' = 'perc_chg_tech',
                                                                     'Subsidy Change - Interest' = 'perc_chg_int'))
      )
      
    }
    
  })
  
  # Update Agency Selection based on buget year - Don't want to show agencies that aren't in FCS as of FY
  
  observe({
    
    current_selection <- input$agency
    
    x <- unique(re[re$fy == input$fy,][['h1']]) %>% sort()
    
    if (is.null(current_selection)) {
      
      current_selection <- 'HUD'
      
    } else if (!current_selection %in% x) {
      
      current_selection <- 'HUD'
      
    }
    
    updateSelectInput(session, "agency",
                      "Select Agency:",
                      choices = x,
                      selected = current_selection)
    
  })
  
  # Also for Program selection
  
  dd = reactiveValues(select=NULL, name = NULL, new_select = NULL)
  
  observeEvent(input$program_dt_rows_selected, {
    
    x <- unique(re[re$fy == input$fy & re$h1 == input$agency,][['prog']]) %>% sort()
    
    # Original Selection
    
    dd$select <- dd$new_select <- input$program_dt_rows_selected
    
    # Original Selection Name
    
    dd$name <- x[dd$select]
    
  })
  
  observeEvent(input$fy, {
    
    x <- unique(re[re$fy == input$fy & re$h1 == input$agency,][['prog']]) %>% sort()
    
    # Updated Selection
    
    dd$new_select <- which(x == dd$name)
    
  })
  
  observeEvent(input$ActOne, ({
    
    dd$select <- dd$new_select <- dd$name <- NULL
    
  }))
  
  # ------------------------- Create Agency and Program dfs ------------------------ #
  
  # Filter Reestimate Rates on Agency Selection
  
  agency_df <- reactive({
    
    re %>% filter(h1 == input$agency)
    
  })
  
  # Create Data.Frame To Display Programs Available Within Agency Selection
  
  program_list <- reactive({
    
    agency_df()[agency_df()$fy == input$fy,] %>% 
      group_by(prog, type) %>% summarise(cnt = n_distinct(co_yr)) %>%
      ungroup() %>% select(Program = prog, Type = type, Cohorts = cnt)
    
  })
  
  # Get Program Selection
  
  program_selection <- reactive({
    
    #s = input$program_dt_rows_selected
    
    df <- if (is.null(dd$select)) { 
      
      NULL
      
    } else if (length(dd$new_select) < 1) {
      
      NULL
      
    } else { 
      
      data.frame(prog = program_list()[dd$new_select,][['Program']],
                 type = program_list()[dd$new_select,][['Type']])
    }
    
    return(df)
    
  })
  
  # Filter Reestimate Rates on Program Selection - Click From DT values
  
  program_df <- reactive({
    
    if (is.null(program_selection())) NULL else
      
      agency_df() %>% filter(prog == program_selection()[['prog']][1] & 
                               type == program_selection()[['type']][1])
    
  })
  
  
  # ------------------------------  Panels for Agency UI ----------------------- #
  
  # Number of Programs
  
  output$program_count <- renderUI({
    
    prog_cnt <- agency_df()[agency_df()$fy == input$fy,][['prog']] %>% n_distinct()
    
    HTML(paste0('<h4>', prog_cnt, ' Programs', '</h4>', 'As of budget year ', input$fy))
    
  })
  
  output$panel1 <- renderUI({
    
    insert_panel('Agency Program Count', 'program_count')
    
  })
  
  # Top Program by Disbursements
  
  output$top_prog <- renderUI({
    
    top <- agency_df() %>% filter(fy == input$fy) %>% group_by(prog) %>%
      summarise(total_disb = sum(disb, na.rm = T)) %>% filter(total_disb == max(total_disb)) %>%
      mutate(total_disb_format = sapply(total_disb, get_millions))
    
    if (sum(top[["total_disb"]]) == 0) {
      
      paste('Disbursement Data Unavailable As of Budget Year Selection')
      
    } else {
      
      HTML(paste0('<h4>', top[['prog']], '</h4>', 'Lifetime Disbursements: ', top[['total_disb_format']]))
      
    }
    
  })
  
  output$panel2 <- renderUI({
    
    insert_panel('Top Agency Program', 'top_prog')
    
  })
  
  # Total Lifetime Reestimates
  
  output$life_reestimates <- renderUI({
    
    reestimates <- agency_df() %>% filter(fy == input$fy) %>% 
      summarise(total_re = sum(life_re, na.rm = T)) %>%
      mutate(total_re_format = sapply(total_re, get_millions))
    
    HTML(paste0('<h4>', reestimates[['total_re_format']], '</h4>', 'As of Budget Year ', input$fy))
    
  })
  
  output$panel3 <- renderUI({
    
    insert_panel('Agency Lifetime Reestimates', 'life_reestimates')
    
  })
  
  # Percent of cohorts with increase in subsidy rate
  
  output$pct_positive <- renderUI({
    
    distribution <- agency_df() %>% filter(fy == input$fy) %>%
      summarise(ratio = round(sum(rate_chg > 0, na.rm = T)/sum(!is.na(rate_chg)) * 100, 1))
    
    HTML(paste0('<h4>', distribution[['ratio']], '% of Cohorts</h4>',
                'Experienced an Increase in Subsidy Rate'))
    
  })
  
  output$panel4 <- renderUI({
    
    insert_panel('Agency Subsidy Increases', 'pct_positive')
    
  })
  
  
  
  # -------------------- Create DT of Available Programs ---------------------------- #
  
  output$program_dt <- renderDataTable({
    
    datatable(program_list(), rownames = FALSE, selection = list(mode = 'single', selected = dd$new_select),
              options = list(searching = FALSE, pageLength = 8, bLengthChange = FALSE,
                             ordering = FALSE, autoWidth = FALSE, bInfo = FALSE))
    
  })
  
  # ------------------- Plot Original vs. Current Subsidy Rate ---------------------- # 
  
  output$xy_bubble <- renderPlotly({
    
    bubble_plot(agency_df(), input$fy)
    
  })
  
  output$xy_plot <- renderPlotly({
    
    # Get Underlying Data Depending on User Selections
    
    plot_data <- get_plot_df(input$view_type, agency_df(), program_df())
    
    # Display temporary error message when toggling between user inputs
    
    validate(need(length(nrow(plot_data)) > 0, 'Calculating...'))
    
    plot_data <- plot_data %>% filter(fy == input$fy)
    
    cohort_xy(plot_data, input$view_type)
    
  })
  
  # ------------------- Plot Current and Lifetime Reestimate Trend ---------------------- # 
  
  output$reestimate_plot <- renderPlotly({
    
    # Get Underlying Data Depending on User Selections
    
    plot_data <- get_plot_df(input$view_type, agency_df(), program_df())
    
    # Display temporary error message when toggling between user inputs
    
    validate(need(length(nrow(plot_data)) > 0, 'Calculating...'))
    
    plot_data <- plot_data %>% filter(fy <= input$fy)
    
    caption <- get_caption(plot_data, input$view_type)
    
    # Get Lifetime Reestimate Amounts by Budget Year
    
    df_summary <- plot_data %>% group_by(fy) %>%
      summarise(re_amt = sum(life_re, na.rm = T), curr_amt = sum(cur_re, na.rm = T))
    
    # Create Plot
    
    plot_ly(df_summary, x = ~fy, y = ~curr_amt, type = 'bar', name = 'Current </br>Reestimate') %>%
      add_trace(y = ~re_amt, type = 'scatter', mode = 'markers+lines', name = 'Lifetime </br>Reestimate') %>%
      layout(title = paste('Current and Lifetime Reestimates \u2013', caption),
             xaxis = list(title = 'Budget Year'),
             yaxis = list(title = ''),
             barmode = 'relative',
             legend = list(orientation = 'h'))
    
  })
  
  # ------------------------ Bar/Dumbbell Plots --------------------------------- # 
  
  output$bar_plots <- renderPlotly({
    
    # Get Underlying Data Depending on User Selections
    
    plot_data <- get_plot_df(input$view_type, agency_df(), program_df())
    
    # Display temporary error message when toggling between user inputs
    
    validate(need(length(nrow(plot_data)) > 0, 'Calculating...'))
    
    plot_data <- plot_data %>% filter(fy == input$fy)
    
    df <- prepare_bar_data(plot_data, input$grp_by, input$bar_metric)
    
    caption <- get_caption(plot_data, input$view_type)
    
    plt_title <- paste(caption, '-', 
                       names(cohort_metric_choices[cohort_metric_choices == input$bar_metric]))
    
    compare_bars(df, input$grp_by, plt_title, 
                 names(cohort_metric_choices[cohort_metric_choices == input$bar_metric]))
    
  })
  
  output$dumbbell_plots <- renderPlotly({
    
    validate(need(!is.null(program_df()), 'Calculating...'))
    
    plot_data <- if (is.null(program_df())) {
      
      NULL
      
    } else {
      
      program_df() %>% filter(fy == input$fy)
      
    }
    
    caption <- get_caption(plot_data, input$view_type)
    
    plt_title <- paste(caption, '-', 
                       names(cohort_metric_choices[cohort_metric_choices == input$bar_metric]))
    
    dumbbell_plt(plot_data, input$bar_metric, plt_title)
    
  })
  
  
  
})
