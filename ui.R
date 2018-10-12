# prepare environment
#--------------------

# load libs
library(DT)
library(forecast)
library(magrittr)
library(shinydashboard)
library(sweep)
library(tidyverse)
library(tidyquant)
library(timetk)

# load all custom funs
source("script/funs.R")

# header
#--------------------

# dashboard header
header <- dashboardHeader(

  # main title
  title = "Forecasting Lab",

  titleWidth = 300

)

# sidebar
#--------------------

# data
dataMenu <- menuItem(

  text = "Data",
  tabName = "data",
  icon = icon("line-chart"),
  selected = TRUE

)

# decomposition
decompMenu <- menuItem(

  text = "Time Series Decomposition",
  tabName = "decomp",
  icon = icon("line-chart")

)

# forecast
forecastMenu <- menuItem(

  text = "Forecast",
  tabName = "forecast",
  icon = icon("line-chart")

)

# full sidebar
sidebar <- dashboardSidebar(

  sidebarMenu(

    # data
    dataMenu,

    # decomp
    decompMenu,

    # forecast
    forecastMenu

  ),

  width = 300

)

# body: data - selection
#--------------------

# data settings panel
dataSettings <- box(

  # input: file upload
  uiOutput(outputId = "dataInputUI"),

  # input: data settings
  uiOutput(outputId = "dataNameUI"),
  uiOutput(outputId = "tsVarNameUI"),
  uiOutput(outputId = "tsIndexNameUI"),

  status = "primary",
  width = NULL

)

# body: data - quick check
#--------------------

# check sample
dataCheckSample <- box(

  dataTableOutput(outputId = "checkSample"),

  title = NULL,
  width = NULL

)

# check plot
dataCheckPlot <- box(

  plotOutput(
    outputId = "checkPlot",
    height = "450px"
  ),

  title = NULL,
  width = NULL

)

# body: data - full
#--------------------

# data body
dataBody <- tabItem(

  tabName = "data",

  fixedRow(

    column(
      dataCheckPlot,
      width = 12
    )

  ),

  fluidRow(

    column(
      dataSettings,
      width = 5
    ),

    column(
      dataCheckSample,
      width = 7
    )

  )

)

# body: decomp - input
#--------------------

# method settings panel
decompMethodSetting <- tabPanel(

  title = "Decomposition Method",

  # input; set decomposition method
  uiOutput("decompMethodUI"),
  
  # input; set data transformation
  uiOutput("decompTransUI"),
  
  width = NULL

)

# data settings panel
decompSeasSetting <- tabPanel(

  title = "Seasonality",

  # input; set ts frequency type
  uiOutput("decompFreqNUI"),

  # input: set ts frequency
  uiOutput("decompFreqUI"),

    width = NULL

)

# full decomposition settings
decompSetting <- tabBox(

  # method setting
  decompMethodSetting,

  # data setting
  decompSeasSetting,

  width = NULL

)

# body: decomp - full
#--------------------

# full decomp body
decompBody <- tabItem(

  tabName = "decomp",

  fixedRow(

    column(
      uiOutput("decompPlotUI"),
      width = 12
    )

  ),

  fixedRow(
    
    column(
      
      decompSetting,
      
      # input: decomposition action button
      actionButton(inputId = "decompAB", label = "Apply"),
      
      width = 5
      
    ),

    column(
      
      uiOutput("decompTableUI"),
      
      conditionalPanel(
        
        condition = "output.decompTableUI != null",
        
        downloadButton("decompDownload", "Download Result")
        
      ),
      
      width = 7
    )

  )
)

# body: forecast - input
#--------------------

# method settings panel
forecastMethodSetting <- tabPanel(

  title = "Forecast Method",

  # input; set forecast method
  uiOutput("forecastMethodUI"),
  uiOutput("forecastHorizonUI"),
  uiOutput("forecastTransUI"),

  width = NULL

)

# data settings panel
forecastSeasSetting <- tabPanel(

  title = "Seasonality",

  # input; set ts frequency type
  uiOutput("forecastFreqNUI"),

  # input: set ts frequency
  uiOutput("forecastFreqUI"),

  width = NULL

)

# full forecast settings
forecastSetting <- tabBox(

  # method setting
  forecastMethodSetting,

  # data setting
  forecastSeasSetting,

  width = NULL

)

# body: forecast - full
#--------------------

# full forecast body
forecastBody <- tabItem(

  tabName = "forecast",

  fixedRow(

    column(
      uiOutput("forecastPlotUI"),
      width = 12
    )

  ),

  fixedRow(

    column(
      
      forecastSetting,
      
      # input: forecast action button
      actionButton(inputId = "forecastAB", label = "Apply"),
      
      width = 5
      
    ),

    column(
      
      uiOutput("forecastTableUI"),
      
      conditionalPanel(
        
        condition = "output.forecastTableUI != null",
        
        downloadButton("forecastDownload", "Download Result")
        
      ),
      
      width = 7
      
    )

  )
)

# body: full
#--------------------

# full body
body <- dashboardBody(

  tags$style(
    HTML(
      '
          input[type=number] {
                -moz-appearance:textfield;
          }
          input[type=number]::{
                -moz-appearance:textfield;
          }
          input[type=number]::-webkit-outer-spin-button,
          input[type=number]::-webkit-inner-spin-button {
                -webkit-appearance: none;
                margin: 0;
          }
          .main-header .logo {
            text-align: left;
          }
      '
    )
  ),

  # list of all tabs
  tabItems(

    # data
    dataBody,

    # decomposition
    decompBody,

    # forecast
    forecastBody

  )

)

# full UI
#--------------------

ui <- dashboardPage(

  # header
  header = header,

  # side bar
  sidebar = sidebar,

  # body
  body = body

)
