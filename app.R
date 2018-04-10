## Basic Shiny application to display population for a selected location
library(shiny)
library(data.table)
library(plotly)
library(formattable)
library(shinythemes)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(ggthemes)
library(quantmod)
library(TTR)
library(Rbitcoin)
library(memoise)

setwd("~/Documents/Git/fin_view")


## Source helper functions and Shiny tab views
source("import_data.R")
source("stock_trends.R")
source("stock_profiles.R")
source("inv_forecast.R")


## Load data
symbol_map <- setDT(TTR::stockSymbols())
mutual_fund_map <- readRDS("mutual_fund_list.RData")
etf_map <- readRDS("etf_list.Rdata")

symbol_map <- rbindlist(list(symbol_map, mutual_fund_map, etf_map), use.names = T, fill = T)
symbol_map[, display_name := paste0(Symbol, " - ", Name)]


## Create the UI
ui <- function(request) { 
  navbarPage("Finance Center",
    # shinythemes::themeSelector(),  # To play around with UI themes
    theme = shinytheme("paper"),
    tabPanel("Landing Page", includeMarkdown("landing_page.md")),
    tabPanel("Stock Trends", stock_trends_ui("stock_trends",
                             symbol_map = symbol_map)),
    tabPanel("Stock Profiles", stock_profiles_ui("stock_profiles", 
                               symbol_map = symbol_map)),
    tabPanel("Investment Forecaster", inv_forecast_ui("inv_forecast"))
  )
}


## Create the Server
server <- function(input,output,session) {
  callModule(stock_trends_server, "stock_trends",
                                 symbol_map = symbol_map,
                                 fetch_cache_symbol = fetch_cache_symbol)
  callModule(stock_profiles_server, "stock_profiles",
                                    symbol_map = symbol_map,
                                    fetch_cache_symbol = fetch_cache_symbol)
  callModule(inv_forecast_server, "inv_forecast",
                                  fetch_cache_symbol = fetch_cache_symbol)
  
}

shinyApp(ui, server, enableBookmarking = "url")
