library(shiny); require(mlbench); require(plotly)
require(AppliedPredictiveModeling); require(benford.analysis); require(dplyr)
require(shinyBS); require(shinythemes)

available_data <- c(
  'Upload csv file'                      = 'csv',
  'Weight of chicks on different diets'  = 'ChickWeight',
  'New York Air Quality Measurements'    = 'airquality',
  'The Joyner-Boore Attenuation Data'    = 'attenu',
  'Boston Housing Data'                  = 'BostonHousing',
  'Pima Indians Diabetes Database'       = 'PimaIndiansDiabetes',
  'Shuttle Data'                         = 'Shuttle',
  'HPC Job Scheduling'                   = 'schedulingData')

get_data <- function(txt) {
  data(list = txt)
  return(get(txt))
}
