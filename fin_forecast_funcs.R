cagr_to_daily_rate <- function(cagr) {
  daily_rate <- (1 + (cagr / 100)) ^ 1/365
  return(daily_rate)
}

# data[, total_value := principal * cumprod(daily_rate)]

add_event_data <- function(data, event_data, event, end_date) {
  event_start <- event_data[event_name == event, start_date]
  event_recurrence <- event_data[event_name == event, recurrence]
  event_date_map <- list("Weekly" = "week",
                         "Bi-Weekly" = "2 weeks",
                         "Monthly" = "month",
                         "Quarterly" = "quarter",
                         "Annually" = "year")
  if(event_recurrence == "One-Time") {
    event_dates <- event_start
  } else {
    event_dates <- seq(event_start, end_date, by = event_date_map[[event_recurrence]])
  }
  
  for(event_date in event_dates) {
    data[, event_to_add := 0]
    event_principal <- event_data[event_name == event, principal]
    data[date >= event_date, principal := principal + event_principal]
    data[date >= event_date, total_value := total_value + event_principal * cumprod(daily_rate)/daily_rate]
  }
}
