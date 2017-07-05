# global.R

# Load Package Dependencies

library(shiny)
require(shinyBS)
require(shinythemes)
require(plotly)
require(dplyr)
require(DT)
require(lazyeval)

# Load Data

load("reestimate_rates.RData")

# ---------------------  Clean any data anomalies for app -------------------------------- #

# Remove Duplicate Education Programs and multiply reestimate/disbursement columns by 1000

edu_progs <- c("SUBSIDIZED STAFFORD", "UNSUBSIDIZED STAFFORD", "PLUS", "CONSOLIDATED")

re <- reestimate_rates[!reestimate_rates$prog %in% edu_progs,] %>%
  mutate(life_re = life_re * 1000,
         cur_re = cur_re * 1000,
         life_re_no_int = life_re_no_int * 1000,
         disb = disb * 1000,
         rate_chg = cur_sr - orig_sr)

# -------------------  Other useful groupings ------------------------------------------- #

# Get list of all agencies
agency_list <- unique(reestimate_rates$h1) %>% sort()

program_metric_choices = c('Lifetime Reestimates' = 'life_re',
                           'Current Reestimates' = 'cur_re',
                           'Lifetime Disbursements' = 'disb')

cohort_metric_choices = c('Lifetime Reestimates' = 'life_re',
                          'Current Reestimates' = 'cur_re',
                          'Lifetime Disbursements' = 'disb',
                          'Total Subsidy Change' = 'rate_chg',
                          'Subsidy Change - Technical' = 'perc_chg_tech',
                          'Subsidy Change - Interest' = 'perc_chg_int')

# ------------------- UI and Formatting Functions ----------------------------------------------- #

insert_panel <- function(heading, outname) {
  
  tags$div(class = "panel panel-default",
           tags$div(class = "panel-heading",
                    tags$div(h4(class = "panel-title", heading))),
           tags$div(class = "panel-body",
                    htmlOutput(outname)))
  
}

# Format large currency values

get_millions <- function(x) {
  
  if (is.na(x)) {
    
    NA
    
  } else if (abs(x) < 1e9) {
    
    paste0('$', formatC(x/1e6, format = 'f', big.mark = ',', digits = 1), ' M')
    
  } else if (abs(x) < 1e12) {
    
    paste0('$', formatC(x/1e9, format = 'f', big.mark = ',', digits = 2), ' B')
    
  } else if (abs(x) >= 1e12) {
    
    paste0('$', formatC(x/1e12, format = 'f', big.mark = ',', digits = 2), ' T')
    
  }
  
}

# ------------------- Data Prep Functions for Plots ------------------------------------------------- #

get_plot_df <- function(view_type, agency_df, program_df) {
  
  df <- if (view_type == 'Agency') {
    
    agency_df
    
  } else {
    
    program_df
    
  }
  
}

get_caption <- function(plot_data, what_input) {
  
  if (what_input == 'Agency') {
    
    caption <- plot_data[['h1']][1] %>% as.character()
    
  } else {
    
    caption <- plot_data[['prog']][1] %>% as.character()
    
  }
  
  
  if (nchar(caption) > 35) {
    
    paste(substr(caption, 1, 35), '...')
    
  } else {
    
    caption
    
  }
  
}

prepare_bar_data <- function(plot_data, lab, metric) {
  
  df <- plot_data %>% group_by_(lab) %>%
    summarise_(sum_val = interp(~sum(var, na.rm = T), var = as.name(metric))) %>%
    ungroup()
  
  df <- if (lab != 'co_yr') {
    df %>% arrange(sum_val)
  } else {
    df
  }
  
  df$wrap <- sapply(df[[lab]], FUN = function(x) 
  {paste(strwrap(x, width = 25), collapse = "<br>")})
  
  #return(df)
  
  lab_factors <- factor(df[['wrap']], levels = df[['wrap']])
  # 
  compare_lab <- df %>% mutate(label = lab_factors)
  
}

# ---------------------------------- Plotting Functions ------------------------------------ #

compare_bars <- function(df, lab, plt_title, xaxis) {
  
  margin <- ifelse(lab == 'co_yr', 50, 200)
  ftsize <- ifelse(lab == 'co_yr', 16, 10)
  
  plot_ly(df,
          y = ~label,
          x = ~sum_val,
          type = 'bar',
          marker = list(color = 'rgba(55, 128, 191, 0.7)',
                        line = list(color = 'rgba(55, 128, 191, 0.9)',
                                    width = 1.5))) %>%
    layout(title = plt_title,
           titlefont = list(size = 16),
           xaxis = list(title = xaxis),
           yaxis = list(title = "", tickfont = list(size = ftsize)),
           margin = list(l = margin))
  
}

dumbbell_plt <- function(df, metric, plt_title) {
  
  if (metric == 'rate_chg') {
    
    plot_ly(df, color = I("gray80")) %>%
      add_segments(x = ~orig_sr, xend = ~cur_sr, y = ~co_yr, yend = ~co_yr, showlegend = FALSE,
                   line = list(width = 3)) %>%
      add_markers(x = ~orig_sr, y = ~co_yr, name = "Original Rate", color = I("#41E1F0"), 
                  marker = list(size = 8, line = list(color = "#068591", width = 2))) %>%
      add_markers(x = ~cur_sr, y = ~co_yr, name = "Current Rate", color = I("#7A6CF7"), 
                  marker = list(size = 8, line = list(color = "#0A0B2E", width = 2))) %>%
      layout(
        title = plt_title,
        titlefont = list(size = 16),
        xaxis = list(title = "Subsidy Rate",
                     showticklabels = TRUE,
                     showgrid = FALSE),
        yaxis = list(showticklabels = TRUE,
                     showgrid = FALSE,
                     title = 'Cohort'),
        legend = list(orientation = 'h'))
    
  } else {
    
    plot_ly(df, color = I("gray80")) %>%
      add_segments(x = 0, xend = ~get(metric), y = ~co_yr, yend = ~co_yr, showlegend = FALSE,
                   line = list(width = 3)) %>%
      add_markers(x = ~get(metric), y = ~co_yr, name = "Current Rate", color = I("#7A6CF7"), 
                  marker = list(size = 8, line = list(color = "#0A0B2E", width = 2))) %>%
      layout(
        title = plt_title,
        titlefont = list(size = 16),
        xaxis = list(title = "Subsidy Rate",
                     showticklabels = TRUE,
                     showgrid = FALSE),
        yaxis = list(showticklabels = TRUE,
                     showgrid = FALSE,
                     title = 'Cohort'),
        legend = list(orientation = 'h'))
    
  }
  
}


wrap_col <- function(col1, col2) {
  
  if (is.na(col1)) {
    paste(strwrap(col2, width = 20), collapse = "<br>")   
  } else {
    paste(strwrap(col1, width = 20), collapse = "<br>")
  }
  
}

bubble_plot <- function(df, in_fy) {
  
  plot_data <- df %>% filter(fy == in_fy & !is.na(cur_sr)) %>%
    mutate(size = sqrt(1/min(disb, na.rm = T) * disb),
           wrapped_label = mapply(wrap_col, h2, prog)) 
  
  min_point <- min(c(plot_data$cur_sr, 
                     plot_data$orig_sr), na.rm = TRUE) - 1
  max_point <- max(c(plot_data$cur_sr, 
                     plot_data$orig_sr), na.rm = TRUE) + 1
  
  a <- list(
    text = 'Subsidy Increases',
    showarrow = FALSE,
    y = max_point,
    x = min_point,
    yanchor = 'top',
    xanchor = 'left'
  )
  
  b <- list(
    text = 'Subsidy Decreases',
    showarrow = FALSE,
    y = min_point,
    x = max_point,
    yanchor = 'bottom',
    xanchor = 'right'
  )
  
  caption <- plot_data[1,][['h1']]
  
  
  plot_ly(plot_data,  
          x = ~orig_sr, y = ~cur_sr, color = ~wrapped_label, size = ~size, colors = 'Paired', hoverinfo = 'text',
          type = 'scatter', mode = 'markers', sizes = c(min(plot_data$size, na.rm = T) + 5, 
                                                        min(mean(plot_data$size, na.rm = T), 125) + 5),
          marker = list(symbol = 'circle', sizemode = 'diameter',
                        line = list(width = 2, color = '#000000')),
          text = ~paste('Cohort:', co_yr, '</br> Program:', prog,
                        '</br> Original Rate:', round(orig_sr, 2),
                        '</br> Current Rate:', round(cur_sr, 2))) %>%
    layout(title = paste('Subsidy Distribution By Cohort \u2013', caption),
           shapes = list(
             type = 'line',
             layer = 'above',
             line = list(color = "#800505", dash = "longdash", width = 3),
             x0 = min_point,
             y0 = min_point,
             x1 = max_point,
             y1 = max_point
           ),
           annotations = list(a, b),
           xaxis = list(title = 'Original Subsidy Rate', showgrid = FALSE),
           yaxis = list(title = 'Current Subsidy Rate', showgrid = FALSE)
    )
  
  
}

cohort_xy <- function(df, view_type) {
  
  min_point <- min(c(df$cur_sr, 
                     df$orig_sr), na.rm = TRUE) - 1
  max_point <- max(c(df$cur_sr, 
                     df$orig_sr), na.rm = TRUE) + 1
  
  a <- list(
    text = 'Subsidy Increases',
    showarrow = FALSE,
    y = max_point,
    x = min_point,
    yanchor = 'top',
    xanchor = 'left'
  )
  
  b <- list(
    text = 'Subsidy Decreases',
    showarrow = FALSE,
    y = min_point,
    x = max_point,
    yanchor = 'bottom',
    xanchor = 'right'
  )
  
  caption <- get_caption(df, view_type)
  
  plot_ly(df, x = ~orig_sr, y = ~cur_sr, 
          type = 'scatter', mode = 'markers', hoverinfo = 'text',
          text = ~paste('Cohort:', co_yr, '</br> Program:', prog,
                        '</br> Original Rate:', round(orig_sr, 2),
                        '</br> Current Rate:', round(cur_sr, 2)),
          marker = list(size = 8,
                        opacity = .5,
                        color = c("#7A6CF7"),
                        line = list(color = '#0A0B2E',
                                    width = 2))) %>%
    layout(title = paste('Subsidy Distribution By Cohort \u2013', caption),
           titlefont = list(size = 16),
           xaxis = list(showgrid = FALSE, title = 'Original Subsidy Rate'),
           yaxis = list(showgrid = FALSE, title = 'Current Subsidy Rate'),
           annotations = list(a, b),
           shapes = list(
             type = 'line',
             layer = 'above',
             line = list(color = "#800505", dash = "longdash", width = 3),
             x0 = min_point,
             y0 = min_point,
             x1 = max_point,
             y1 = max_point
           )
    )
  
}

