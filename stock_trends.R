## UI and Server for Stock Trends view of historical stock data

stock_trends_ui <- function(id, symbol_map = symbol_map) { 
  ## Create a namespace function using the provided id
  ns <- NS(id)
  
  symbol_choices <- sort(unique(symbol_map$display_name))
  initial_symbols <- unique(symbol_map[Symbol == "SPY", display_name])

  fluidPage(
    title = "Stock Trends",
      column(2,
        h4("Stock Trends"),
        selectizeInput(ns("sel_symbols"),
                       "Selected Symbols",
                       choices = NULL,
                       selected = NULL,
                       options = list(maxItems = 15)
                       ),
        radioButtons(ns("sel_recall_period"),
                       "History Period",
                       choices = c("1 Week",
                                   "1 Month",
                                   "3 Months",
                                   "1 Year",
                                   "5 Years",
                                   "Max"),
                       selected = "1 Year"),
        checkboxInput(ns("normalize_prices"), "Normalize Price at Start Date", FALSE),
        checkboxInput(ns("show_moving_average"), "Show Moving Averages", FALSE),
        conditionalPanel(condition = paste0("input['", ns("show_moving_average"), "'] == true"),
                  checkboxInput(ns("exp_moving_average"), "Use Exponential Moving Average", FALSE),
                  selectizeInput(ns("sel_mav_width"),
                                 "Moving Average Widths",
                                 choices = c(10, 20, 50, 100, 200),
                                 selected = 10,
                                 options = list(maxItems = 2))
                  ),
        downloadButton(ns("download_data"), "Download Data"),
        bookmarkButton()
    ),
    column(10,
      tabsetPanel(
        tabPanel("Graph", 
                 plotlyOutput(ns("ts_graph"), height="100%"),
                 uiOutput(ns("refresh_button_ui"))
                 ),
      tabPanel("Table", DT::dataTableOutput(ns("data_table"))),
      tabPanel("Statistics", DT::dataTableOutput(ns("stats_table")))
      )
    )
  )
}


###################################################################
## Set the Server for the visualization

stock_trends_server <- function(input, output, session, 
                                symbol_map,
                                fetch_cache_symbol) {
  print("stock trends server started")
  observe({
    req(input$sel_symbols)
  })
  
  ## Set reactive values to store reactive results
  symbol_choices <- sort(unique(symbol_map$display_name))
  initial_symbols <- unique(symbol_map[Symbol == "SPY", display_name])
  
  updateSelectizeInput(session, 'sel_symbols', choices = symbol_choices, selected = initial_symbols, server = TRUE)
  
  symbols_selected <- reactiveValues(value = unique(symbol_map[Symbol == "SPY", display_name]))

  observe({
    symbols_selected$value <- unique(symbol_map[display_name %in% input$sel_symbols, Symbol])
  })
  
  sel_mav_widths <- reactiveValues(value = 10)
  exp_moving_average <- reactiveValues(value = TRUE)
  
  observe({
    print("Updating selectize")
    
    if(input$exp_moving_average == F) sel_mav_opts <- c(10, 20, 50, 100, 200)
    if(input$exp_moving_average == T) sel_mav_opts <- c(12, 26, 52)
    updateSelectizeInput(session, "sel_mav_width", choices = sel_mav_opts, selected = sel_mav_opts[1], server = TRUE)
    sel_mav_widths$value <- sel_mav_opts[1]
    exp_moving_average$value <- input$exp_moving_average
    
    print("Selectize updated")
  })
  
  observe({
    if(length(input$sel_mav_width) != 0) sel_mav_widths$value <- as.integer(input$sel_mav_width)
    if(length(input$sel_mav_width) == 0) sel_mav_widths$value <- list()
  })
  
  stock_data <- reactive({ withProgress(message = 'Pulling data for selected stocks', value = 0, {
    if(length(symbols_selected$value) > 0) {
      progress_bar_increment <- .8 / length(symbols_selected$value)
      stock_data <- rbindlist(lapply(symbols_selected$value, 
                               function(x) {
                                incProgress(progress_bar_increment, 
                                            detail = paste0("Fetching ", x))

                                fetch_cache_symbol(x)
                              }))
    } else {
      stock_data <- data.table()
    }

    return(stock_data)
  }) })

  # ## Create a reactive to display points if clicked on the plot
  # sel_datapoints <- reactive({
  #   event.data <- event_data("plotly_click")
    
  #   # If no point is selected, then see if there are any selected (drag-tool)
  #   if(is.null(event.data) == T) {
  #     event.data <- event_data("plotly_selected")
  #   } 
    
  #   if(is.null(event.data) == T) {
  #     output$outlier_return <- renderText("") # Clear the outlier-marked message anytime you select a new point
  #     return(data.table())
  #   } else if(nrow(event.data) != 0) {
  #     staged_data <- copy(toggled_data())
  #     staged_data[, year_id := signif(year_id, 8)]
  #     staged_data[, mean := signif(mean, 8)]
      
  #     sel_data <- setDT(copy(event.data))
  #     sel_data[, x := signif(x, 8)]
  #     sel_data[, y := signif(y, 8)]
  #     sel_data <- sel_data[!duplicated(sel_data, by = c("x", "y"))]
      
  #     points <- merge(staged_data, sel_data, by.x = c("year_id", "mean"), by.y = c("x", "y"))
  #     # point <- master_data()[signif(year_id,8) == signif(event.data$x,8) & signif(mean,8) == signif(event.data$y,8),]
  #     output$outlier_return <- renderText("") # Clear the outlier-marked message anytime you select a new point
  #     return(points)
  #   }
  # })

  stock_statistics <- reactive({ withProgress(message = 'Calculating statistics for stocks', value = 0, {
    selected_methods <- input$sel_stats_method
    req_data_cols <- find_ttr_cols(method = input$sel_stats_method)

    stock_statistics <- rbindlist(lapply(symbols_selected$value),
                                         apply_ttr_method(data = stock_data()[, req_data_cols], 
                                                          method = input$sel_stats_method))
    return(stock_statistics)
  }) })

  formatted_prices <- reactive({
    formatted_prices <- copy(stock_data())
    
    ## Calculate moving averages -- either simple moving average or exponential
    ## Wrapped in !is.null conditionals to avoid T/F comparison error message on switch
    print(formatted_prices)
    print(sel_mav_widths$value)
    if(length(sel_mav_widths$value > 0)) {
      if(exp_moving_average$value == T) formatted_prices[, mav1 := EMA(Adjusted, n = sel_mav_widths$value[1]), by = "Symbol"]
      if(exp_moving_average$value == F) formatted_prices[, mav1 := SMA(Adjusted, n = sel_mav_widths$value[1]), by = "Symbol"]
      
      if(length(sel_mav_widths$value) == 2) {
        if(exp_moving_average$value == T) formatted_prices[, mav2 := EMA(Adjusted, n = sel_mav_widths$value[2]), by = "Symbol"]
        if(exp_moving_average$value == F) formatted_prices[, mav2 := SMA(Adjusted, n = sel_mav_widths$value[2]), by = "Symbol"]
      }
    }
    
    ## In Progress: Calculate % Change in last 30 days
    # formatted_prices[, pct_change_30_day := 100 * ((Adjusted / Adjusted[Date == min(Date)]) - 1), by = "Symbol"]

    ## Subset data to the start of the selected recall period
    recall_period_map <- list("1 Week" = 7,
                              "1 Month" = 30.5,
                              "3 Months" = 30.5 * 3,
                              "1 Year" = 365,
                              "5 Years" = 365 * 5,
                              "Max" = as.numeric(NA))
    
    sel_recall_period <- recall_period_map[[input$sel_recall_period]]
    if(!is.na(sel_recall_period)) formatted_prices <- formatted_prices[Sys.Date() - Date <= sel_recall_period]
    
    ## Calculate % Change since start of period
    formatted_prices[, pct_change := 100 * ((Adjusted / Adjusted[Date == min(Date)]) - 1), by = "Symbol"]
    
    ## Normalize stock prices to the start of the recall period
    if(input$normalize_prices == T) {
      print("normalizing")
      formatted_prices[, Adjusted := Adjusted / Adjusted[Date == min(Date)], by = c("Symbol")]
      if("mav1" %in% colnames(formatted_prices)) formatted_prices[, mav1 := mav1 / mav1[Date == min(Date)], by = c("Symbol")]
      if("mav2" %in% colnames(formatted_prices)) formatted_prices[, mav2 := mav2 / mav2[Date == min(Date)], by = c("Symbol")]
    }
    
    return(formatted_prices)
  })
  
  ## Graph results over year, by different facetting variables
  output$ts_graph <- renderPlotly({
    print(head(formatted_prices()))
    
    if(nrow(formatted_prices()) > 0) {
      ts_graph <- ggplot() +
                  geom_line(data = formatted_prices(), aes(x = Date, y = Adjusted, color = Symbol, label1 = Volume, label2 = pct_change)) +
                  theme_minimal()
      if(input$show_moving_average == T & !is.null(formatted_prices()$mav1)) {
        ts_graph <- ts_graph + 
          geom_line(data = formatted_prices(), aes(x = Date, y = mav1, color = Symbol), linetype = "dotdash")
      }
      
      if(input$show_moving_average == T & !is.null(formatted_prices()$mav2)) {
        ts_graph <- ts_graph + 
          geom_line(data = formatted_prices(), aes(x = Date, y = mav2, color = Symbol), linetype = "dot")
      }

      ggplotly(ts_graph)
    } else {
      plotly_empty()
    }
  })
  
  ## Show data in a DT
  output$data_table <- DT::renderDataTable({
    sel_cols <- c("Symbol", "Name", "Date", "Price", "Adjusted Price")
    sort_cols <- c("Symbol", "Name", "Date")
    dt_data <- formatted_prices()[, .SD, .SDcols = sel_cols]
    setcolorder(dt_data, sort_cols)
    
    DT::datatable(dt_data, 
                  rownames=F,
                  filter=list(position="top",plain=T),
                  options=list(
                    autoWidth=T,
                    pageLength=50,
                    scrollX=TRUE
                  )
    )
  })

  ## Display selected datapoints via a data.table
  output$selected_table <- DT::renderDataTable({
    if(nrow(sel_datapoints()) > 0) {
      sel_datapoints()[, .SD, .SDcols = c("outlier", "year_id", "viz_year", "mean", paste0("upload_", input$sel_proc, "_data_id"), "source_name", "nid", "underlying_nid", "method_name")]
    } else {
      sel_datapoints()
    }
  })
  
  
  ## Create handlers for downloading data and est tables
  output$download_data <- downloadHandler(
    filename=function() {
      paste0("mort_data_",proc_selected$value,"_",loc_selected$value,"_",
             format(Sys.time(),"%Y-%m-%d_%I-%M-%p"),
             ".csv")
    },
    content=function(con) {
      write.csv(master_data(), con, row.names=F)
    }
  )
  
}
