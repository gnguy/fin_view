## UI and Server for view of investment forecasts and debts over time

fin_forecast_ui <- function(id) { 
  ## Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
    title = "Finance Forecasts",
    tabsetPanel(
      tabPanel("Inputs",
              column(6,
                     inputPanel(
                       textInput(ns("overall_name"), label = "Name"),
                       dateInput(ns("overall_start_date"), label = "Start of Investment/Liability"),
                       numericInput(ns("overall_start_balance"), label = "Starting Balance", value = 0),
                       numericInput(ns("overall_interest_rate"), label = "Annualized Interest Rate", value = 0),
                       fileInput(ns("input_file_overall"), "Upload Investment/Liability Data")
                     ), 
                     DT::dataTableOutput(ns("overall_table"))
              ),
              column(6,
                     inputPanel(
                       selectInput(ns("event_name"), label = "Name", choices = ""), # This will be filled in with names of overall investments/liabilities
                       dateInput(ns("event_start_date"), label = "Start Date of Event"),
                       selectInput(ns("event_recurrence"), label = "Event Recurrence", choices = c("One-Time",
                                                                                                   "Weekly",
                                                                                                   "Bi-Weekly",
                                                                                                   "Monthly",
                                                                                                   "Quarterly",
                                                                                                   "Annually")),                             
                       fileInput(ns("input_file_event"), "Upload Event Data")
                     ), 
                    DT::dataTableOutput(ns("event_table"))
              )
      ),
      tabPanel("Plots", plotlyOutput(ns("fin_plots"))),
      tabPanel("Targets", DT::dataTableOutput(ns("fin_targets")))
    )
  )
}


###################################################################
## Set the Server for the visualization

fin_forecast_server <- function(input, output, session) {
  print("fin forecast server started")

  lapply(unique(event_data$event_name), add_event_data, data = data)

  
  
  observe({
    print("Updating selectize")

    # updateSelectizeInput(session, "sel_mav_width", choices = sel_mav_opts, selected = sel_mav_opts[1], server = TRUE)
    # sel_mav_widths$value <- sel_mav_opts[1]
    # exp_moving_average$value <- input$exp_moving_average
    
    print("Selectize updated")
  })
  
  observe({
    
  })
  
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
  output$fin_plots <- renderPlotly({
    print(head(formatted_prices()))
    
    if(nrow(formatted_prices()) > 0) {
      fin_plot <- ggplot() +
        geom_line(data = formatted_prices(), aes(x = Date, y = Adjusted, color = Symbol, label1 = Volume, label2 = pct_change)) +
        theme_minimal()
      if(input$show_moving_average == T & !is.null(formatted_prices()$mav1)) {
        fin_plot <- fin_plot + 
          geom_line(data = formatted_prices(), aes(x = Date, y = mav1, color = Symbol), linetype = "dotdash")
      }
      
      if(input$show_moving_average == T & !is.null(formatted_prices()$mav2)) {
        fin_plot <- fin_plot + 
          geom_line(data = formatted_prices(), aes(x = Date, y = mav2, color = Symbol), linetype = "dot")
      }
      
      ggplotly(fin_plot)
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
