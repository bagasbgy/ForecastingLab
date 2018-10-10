# prepare environment
#--------------------

# load libs
pacman::p_load(
  DT, forecast, magrittr, shinydashboard,
  sweep, tidyverse, tidyquant, timetk
)

# load all custom funs
source("script/funs.R")

# start server
#--------------------

# initiate server
server <- shinyServer(

  function(input, output, session) {

    # data
    #--------------------

    # example data
    fnb <- read_csv("data/fnb.csv")

    # initiate reactive values
    reactives <- reactiveValues(

        initialData = fnb,
        dataName = "fnb.csv",

        tsVar = fnb %>% pull(names(fnb)[2]),
        tsIndex = fnb %>% pull(names(fnb)[1])

    )

    # render file upload UI
    output$dataInputUI <- renderUI({

      # input: file upload
      fileInput(
        label = h5("Upload .csv File:"),
        inputId = "dataInput",
        accept = ".csv"
      )

    })

    # render available datasets UI
    output$dataNameUI <- renderUI({

      selectInput(
        label = h5("Select Dataset:"),
        inputId = "dataName",
        choices = list.files("data"),
        selected = reactives$dataName
      )

    })

    # observe upload status
    observeEvent(input$dataInput, {

      # check upload status
      status <- input$dataInput

      # if not null -> copy,
      if (!is.null(status)) {

        file.copy(
          status$datapath,
          file.path("data", status$name)
        )

      }

    })

    observe({

      if (!is.null(input$dataName)) {

        reactives$dataName <- input$dataName

        reactives$initialData <-
          paste0("data/", reactives$dataName) %>%
          read_csv()

      }

      reactives$initialNames <- names(reactives$initialData)
      reactives$tsVarName <- reactives$initialNames[2]
      reactives$tsIndexName <- reactives$initialNames[1]

    })

    # render ts variable input UI
    output$tsVarNameUI <- renderUI({

      selectInput(
        label = h5("Select Time Series Variable:"),
        inputId = "tsVarName",
        choices = reactives$initialNames,
        selected = reactives$tsVarName
      )

    })

    # render ts index input UI
    output$tsIndexNameUI <- renderUI({

      selectInput(
        label = h5("Select Time Series Index:"),
        inputId = "tsIndexName",
        choices = reactives$initialNames,
        selected = reactives$tsIndexName
      )

    })

    # observe initial inputs
    observe({

      # update ts variable
      if (!is.null(input$tsVarName)) {
        reactives$tsVarName <- input$tsVarName
      }

      reactives$tsVar <-
        reactives$initialData %>%
          pull(reactives$tsVarName)

      # check if ts variable contain zero
      reactives$containZero <- any(reactives$tsVar == 0)

      # store ts index
      if (!is.null(input$tsIndexName)) {
        reactives$tsIndexName <- input$tsIndexName
      }

      reactives$tsIndex <-
        reactives$initialData %>%
          pull(reactives$tsIndexName)

    })

    # check sample
    output$checkSample <- renderDataTable(

      # sample data
      reactives$initialData %>%
        mutate_if(
          is.numeric,
          funs(format(round(., 2), nsmall = 2))
        ),

      # data table options
      options = list(
        pageLength = 7,
        lengthMenu = 7
      )

    )

    # check plot
    output$checkPlot <- renderPlot({

      reactives$initialData %>%
        rename(
          index = reactives$tsIndexName,
          value = reactives$tsVarName
        ) %>%
        ggplot(aes(x = index, y = value)) +
        geom_line() +
        scale_x_datetime(date_labels = "%Y-%m-%d") +
        labs(title = "", y = "", x = "") +
        theme_bw()

    })

    # decomposition
    #--------------------

    # initial states
    reactives$decompFreqN <- 1
    reactives$decompFreq1 <- 24
    reactives$decompFreq2 <- 7
    reactives$decompFreq3 <- 4
    reactives$decompFreq <- 24

    reactives$decompMethod <- "classic"
    reactives$decompTrans <- "none"

    # number of frequency option
    output$decompFreqNUI <- renderUI({

      numericInput(
        label = h5("Set Number of Seasonality:"),
        inputId = "decompFreqN",
        value = reactives$decompFreqN,
        min = 1,
        max = 3
      )

    })

    # update number of frequency
    observeEvent(input$decompFreqN, {

      if (!is.null(input$decompFreqN)) {
        reactives$decompFreqN <- input$decompFreqN
      }

    })

    # decomposition frequency option
    output$decompFreqUI <- renderUI({

      lapply(1:reactives$decompFreqN, function(N) {

        # titles
        freqOrder  <-

          if (N == 1) "First"
          else if (N == 2) "Second"
          else if (N == 3) "Third"

        freqLabel <-

          if (reactives$decompFreqN == 1)
            paste("Set Frequency for Seasonality")
          else paste("Set Frequency for", freqOrder, "Seasonality")

        freqIds <- paste0("decompFreq", N)

        freqInits <-

          if (N == 1) 24
          else if (N == 2) 7
          else if (N == 3) 4

        numericInput(
          label = freqLabel,
          inputId = freqIds,
          value = freqInits,
          min = 2
        )

      })

    })

    # decomposition method option
    output$decompMethodUI <- renderUI({

      selectInput(
        label = h5("Set Decomposition Method:"),
        inputId = "decompMethod",
        choices = list(
          "Classical Decomposition" = "classic",
          "STL" = "stl"
        ),
        selected = reactives$decompMethod
      )

    })

    # data transformation option
    output$decompTransUI <- renderUI({

      selectInput(
        label = h5("Set Data Transformation Method:"),
        inputId = "decompTrans",
        choices = list(
          "None" = "none",
          "Natural Log" = "ln"
        ),
        selected = reactives$decompTrans
      )

    })

    # decomposition
    observe({

      # update frequency input
      if (!is.null(input$decompFreq1)) {
        reactives$decompFreq1 <- input$decompFreq1
      }

      if (!is.null(input$decompFreq2)) {
        reactives$decompFreq2 <- input$decompFreq2
      }

      if (!is.null(input$decompFreq3)) {
        reactives$decompFreq3 <- input$decompFreq3
      }

      # update selected method
      if (!is.null(input$decompMethod)) {
        reactives$decompMethod <- input$decompMethod
      }

      # update selected transformation
      if (!is.null(input$decompTrans)) {
        reactives$decompTrans <- input$decompTrans
      }

      # define frequency
      reactives$decompFreq <-

        if (reactives$decompFreqN == 1) reactives$decompFreq1

        else if (reactives$decompFreqN == 2)
          c(reactives$decompFreq1,
            reactives$decompFreq1 * reactives$decompFreq2)

        else if (reactives$decompFreqN == 3)
          c(reactives$decompFreq1,
            reactives$decompFreq1 * reactives$decompFreq2,
            reactives$decompFreq1 * reactives$decompFreq2 *
            reactives$decompFreq3)

      # start decomposition #

      # create ts object
      if (reactives$decompFreqN == 1) {

        decomposed <- reactives$tsVar %>%
          ts(frequency = reactives$decompFreq)

      }

      else if (reactives$decompFreqN != 1) {

        decomposed <- reactives$tsVar %>%
          ts(frequency = reactives$decompFreq1) %>%
          msts(seasonal.periods = reactives$decompFreq)

      }

      # use constant adjustment if contain zero
      if (reactives$containZero == TRUE) {decomposed %<>% "+" (1)}

      # log transformation
      if (reactives$decompTrans == "ln") {decomposed %<>% log()}

      # decomposition: classic
      if (reactives$decompMethod == "classic") {

          decomposed %<>%
            decompose() %>%
            decompAdj(
              constant =
                if (reactives$containZero == TRUE) -1
                else 0,
              invlog =
                if (reactives$decompTrans == "ln") TRUE
                else FALSE
            ) %>%
            sw_tidy_decomp() %>%
            mutate(
              observed = reactives$tsVar,
              index = reactives$tsIndex
            ) %>%
            select(index, observed, trend, season, random) %>%
            gather(key = key, value = value, -index) %>%
            arrange(key)

      }

      # decomposition: stl
      else if (reactives$decompMethod == "stl") {

        if (reactives$decompFreqN == 1) {

          decomposed %<>%
            stlm() %>%
            decompAdj(
              constant =
                if (reactives$containZero == TRUE) -1
                else 0,
              invlog =
                if (reactives$decompTrans == "ln") TRUE
                else FALSE
            ) %>%
            sw_tidy_decomp() %>%
            mutate(
              observed = reactives$tsVar,
              index = reactives$tsIndex
            ) %>%
            rename(random = remainder) %>%
            select(index, observed, trend, season, random) %>%
            gather(key = key, value = value, -index)

        }

        else if (reactives$decompFreqN != 1) {

          decomposed %<>%
            stlm() %>%
            sw_tidy_decomp() %>%
            mutate(
              observed = reactives$tsVar,
              index = reactives$tsIndex
            ) %>%
            rename(random = remainder) %>%
            rename_at(
              vars(matches("season.")),
              funs(
                str_replace_all(., "season.S", "s") %>%
                str_replace_all("[[0-9]]+", "") %>%
                str_replace_all("al", "_")
              )
            ) %>%
            setNames(names(.) %>% make.unique()) %>%
            rename_at(
              vars(matches("season.")),
              funs(
                str_replace_all(., "_", "1") %>%
                str_replace_all("1.1", "2") %>%
                str_replace_all("1.2", "3")
              )
            ) %>%
            select(
              index, observed, trend, matches("season"),
              random, -matches("forecast::")
            ) %>%
            gather(key = key, value = value, -index)

        }

      }

      # store final decomposition result
      reactives$decomposed <- decomposed

    })

    # display plot
    output$decompPlotUI <- renderUI({

      # names of all components
      decomposed <- reactives$decomposed
      comps <- decomposed %>%
        pull(key) %>% as.factor() %>% levels()

      # full dynamic plot tabs
      decompPlotTabs <- lapply(

        # generate plot and tab panels
        comps, function(comp) {

          compTitle <- str_to_title(comp)
          plotOutputName <- paste0("decomp", compTitle, "Plot")

          # render all component
          output[[plotOutputName]] <-

            renderPlot({

              reactives$decomposed %>%
                filter(key == comp) %>%
                ggplot(aes(x = index, y = value)) +
                geom_line() +
                labs(title = "", y = "", x = "") +
                theme_bw()

            })

          # tab panel
          tabPanel(

            # display selected output
            plotOutput(
              plotOutputName,
              height = "410px"
            ),


            title =
              if (comp == "season")
                paste("Seasonal Component")
              else if (comp == "season1")
                paste("First Seasonal Component")
              else if (comp == "season2")
                paste("Second Seasonal Component")
              else if (comp == "season3")
                paste("Third Seasonal Component")
              else paste(compTitle, "Component"),

            width = NULL

          ) %>%

          return()

        }

      )

      # return dynamic tabs
      do.call(tabBox, c(decompPlotTabs, width = NA)) %>%
        return()

    })

    # display table
    output$decompTable <- renderDataTable(

      # decomposition result
      reactives$decomposed %>%
        mutate_if(
          is.numeric,
          funs(format(round(., 2), nsmall = 2))
        ) %>%
        spread(key = key, value = value),

      # data table options
      options = list(
        pageLength = 5,
        lengthMenu = 5
      )

    )

    # download handler for decomposition result
    output$decompDownload <- downloadHandler(

      filename = function() {
        paste("decompositionResult.csv", sep = "")
      },

      content = function(file) {
        reactives$decomposed %>%
          mutate_if(
            is.numeric,
            funs(format(round(., 2), nsmall = 2))
          ) %>%
          spread(key = key, value = value) %>%
          write.csv(file, row.names = FALSE)
      }

  )

  # forecast
  #--------------------

  # initial states
  reactives$forecastFreqN <- 1
  reactives$forecastFreq1 <- 24
  reactives$forecastFreq2 <- 7
  reactives$forecastFreq3 <- 4
  reactives$forecastFreq <- 24

  reactives$forecastMethod <- "ets"
  reactives$forecastHorizon <- 24
  reactives$forecastTrans <- "none"

  # number of frequency option
  output$forecastFreqNUI <- renderUI({

    numericInput(
      label = h5("Set Number of Seasonality:"),
      inputId = "forecastFreqN",
      value = reactives$forecastFreqN,
      min = 1,
      max = 3
    )

  })

  # update number of frequency
  observeEvent(input$forecastFreqN, {

    if (!is.null(input$forecastFreqN)) {
      reactives$forecastFreqN <- input$forecastFreqN
    }

  })

  # forecast frequency option
  output$forecastFreqUI <- renderUI({

    lapply(1:reactives$forecastFreqN, function(N) {

      # titles
      freqOrder  <-

        if (N == 1) "First"
        else if (N == 2) "Second"
        else if (N == 3) "Third"

      freqLabel <-

        if (reactives$forecastFreqN == 1)
          paste("Set Frequency for Seasonality")
        else paste("Set Frequency for", freqOrder, "Seasonality")

      freqIds <- paste0("forecastFreq", N)

      freqInits <-

        if (N == 1) 24
        else if (N == 2) 7
        else if (N == 3) 4

      numericInput(
        label = freqLabel,
        inputId = freqIds,
        value = freqInits,
        min = 2
      )

    })

  })

  # forecast method option
  output$forecastMethodUI <- renderUI({

    selectInput(
      label = h5("Set Forecast Method:"),
      inputId = "forecastMethod",
      choices = list(
        "ETS" = "ets",
        "Auto ARIMA" = "auto.arima",
        "STL + ETS" = "stlm"
      ),
      selected = reactives$forecastMethod
    )

  })

  # decomposition method option
  output$forecastHorizonUI <- renderUI({

    numericInput(
      label = h5("Set Forecast Horizon:"),
      inputId = "forecastHorizon",
      value = reactives$forecastHorizon,
      min = 1
    )

  })

  # data transformation option
  output$forecastTransUI <- renderUI({

    selectInput(
      label = h5("Set Data Transformation Method:"),
      inputId = "forecastTrans",
      choices = list(
        "None" = "none",
        "Natural Log" = "ln"
      ),
      selected = reactives$forecastTrans
    )

  })

  # decomposition
  observe({

    # update frequency input
    if (!is.null(input$forecastFreq1)) {
      reactives$forecastFreq1 <- input$forecastFreq1
    }

    if (!is.null(input$forecastFreq2)) {
      reactives$forecastFreq2 <- input$forecastFreq2
    }

    if (!is.null(input$forecastFreq3)) {
      reactives$forecastFreq3 <- input$forecastFreq3
    }

    # update selected method
    if (!is.null(input$forecastMethod)) {
      reactives$forecastMethod <- input$forecastMethod
    }

    # update forecast horizon
    if (!is.null(input$forecastHorizon)) {
      reactives$forecastHorizon <- input$forecastHorizon
    }

    # update selected transformation
    if (!is.null(input$forecastTrans)) {
      reactives$forecastTrans <- input$forecastTrans
    }

    # define frequency
    reactives$forecastFreq <-

      if (reactives$forecastFreqN == 1) reactives$forecastFreq1

      else if (reactives$forecastFreqN == 2)
        c(reactives$forecastFreq1,
          reactives$forecastFreq1 * reactives$forecastFreq2)

      else if (reactives$forecastFreqN == 3)
        c(reactives$forecastFreq1,
          reactives$forecastFreq1 * reactives$forecastFreq2,
          reactives$forecastFreq1 * reactives$forecastFreq2 *
          reactives$forecastFreq3)

    # start decomposition #

    # create ts object
    if (reactives$forecastFreqN == 1) {

      forecasted <- reactives$tsVar %>%
        ts(frequency = reactives$forecastFreq)

    }

    else if (reactives$forecastFreqN != 1) {

      forecasted <- reactives$tsVar %>%
        ts(frequency = reactives$forecastFreq1) %>%
        msts(seasonal.periods = reactives$forecastFreq)

    }

    # use constant adjustment if contain zero
    if (reactives$containZero == TRUE) {forecasted %<>% "+" (1)}

    # log transformation
    if (reactives$forecastTrans == "ln") {forecasted %<>% log()}

    # forecast: ets
    if (reactives$forecastMethod == "ets") {
      forecasted %<>% ets()
    }

    # forecast: auto.arima
    else if (reactives$forecastMethod == "auto.arima") {
      forecasted %<>% ets()
    }

    # forecast: stlm
    else if (reactives$forecastMethod == "stlm") {
      forecasted %<>% stlm()
    }

    # tidy the result
    forecasted %<>%
        forecast(h = reactives$forecastHorizon) %>%
        forecastAdj(
          constant =
            if (reactives$containZero == TRUE) -1
            else 0,
          invlog =
            if (reactives$forecastTrans == "ln") TRUE
            else FALSE
        ) %>%
        sw_sweep() %>%
        mutate(
          index = c(
            reactives$tsIndex,
            reactives$tsIndex %>%
              tk_make_future_timeseries(n = reactives$forecastHorizon)
          )
        ) %>%
        select(index, key, value)

    # store final forecast result
    reactives$forecasted <- forecasted

    })

    # display plot
    output$forecastPlot <- renderPlot({

      reactives$forecasted %>%
        ggplot(aes(x = index, y = value, colour = factor(key))) +
        geom_line() +
        labs(title = "", y = "", x = "", colour = "") +
        theme_bw() +
        scale_colour_manual(
          values = c("black", "blue"),
          guide = FALSE
        )

    })

    # display table
    output$forecastTable <- renderDataTable(

      # decomposition result
      reactives$forecasted %>%
        mutate_if(
          is.numeric,
          funs(format(round(., 2), nsmall = 2))
        ) %>%
        spread(key = key, value = value),

      # data table options
      options = list(
        pageLength = 5,
        lengthMenu = 5
      )

    )

    # download handler for decomposition result
    output$forecastDownload <- downloadHandler(

      filename = function() {
        paste("forecastResult.csv", sep = "")
      },

      content = function(file) {
        reactives$forecasted %>%
          mutate_if(
            is.numeric,
            funs(format(round(., 2), nsmall = 2))
          ) %>%
          spread(key = key, value = value) %>%
          write.csv(file, row.names = FALSE)
      }

    )

  # end of server
  #--------------------

  }

)
