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
                       choices = symbol_choices,
                       selected = initial_symbols,
                       server = TRUE,
                       options = list(maxOptions = 100, maxItems = 15),
                       ),
        selectInput(ns("sel_recall_period"),
                       "History Period",
                       choices = c("1 Week",
                                   "1 Month",
                                   "3 Months",
                                   "1 Year",
                                   "5 Years",
                                   "Max")),
        checkboxInput(ns("normalize_prices"), "Normalize Price at Start Date", FALSE),
        checkboxInput(ns("show_moving_average"), "Show Moving Averages", FALSE),
        conditionalPanel(condition = paste0("input['", ns("show_moving_average"), "'] == true"),
                  checkboxInput(ns("exp_moving_average"), "Use Exponential Moving Average", FALSE),
                  selectizeInput(ns("sel_mav_width"),
                                 "Moving Average Widths",
                                 choices = c(10, 20, 50, 100, 200),
                                 selected = 10,
                                 server = TRUE,
                                 options = list(maxItems = 2),
                  )),
        downloadButton(ns("download_data"), "Download Data"),
        bookmarkButton()
    ),
    column(10,
      tabsetPanel(
        tabPanel("Graph", 
                 plotlyOutput(ns("ts_graph"), height="100%"),
                 uiOutput(ns("refresh_button_ui")),

                 )
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

  req(input$sel_symbols)

  ## Set reactive values to store reactive results
  symbols_selected <- reactiveValues(value = unique(symbol_map[Symbol == "SPY", display_name])
    
  observe({
    symbols_selected$value <- unique(symbol_map[display_name %in% input$sel_symbols, Symbol])
  })

  observe({
    if(input$exp_moving_average == FALSE) sel_mav_opts <- c(10, 20, 50, 100, 200)
    if(input$exp_moving_average == TRUE) sel_mav_opts <- c(12, 26, 52)
    updateSelectizeInput(session, "sel_mav_width", choices = sel_mav_opts, selected = sel_mav_opts[1])
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
    if(input$normalize_prices == T) {

    }

    return(formatted_prices)
  })
  
  ## Graph results over year, by different facetting variables
  output$ts_graph <- renderPlotly({
    hovertext <- NULL
    if(nrow(formatted_prices()) > 0) {
      hovertext <- paste0("paste0(",
                          "'Outlier: ', outlier_viz, '<br>",
                          "Adjustment RE/FE: ', adjustment_re_fe, '<br>",
                          "Variance: ', variance, '<br>",
                          "Microdata: ', microdata, '<br>",
                          "NID: ', nid, '<br>",
                          "Title: ', nid_title, '<br>",
                          "Underlying NID: ', underlying_nid, '<br>",
                          "Underlying Title: ', underlying_title",
                          ")")

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
