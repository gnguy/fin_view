fetch_symbol <- function(symbol) {
	data <- as.data.table(getSymbols(symbol, auto.assign = F))
	colnames(data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
	data[, Symbol := symbol]
	return(data)
}

fetch_cache_symbol <- memoise(fetch_symbol, cache = cache_filesystem("~/Desktop/memoise_cache"))

apply_ttr_method <- function(data, symbol, method) {
	data <- data[Symbol == symbol]
	date_vals <- data$Date
	ttr_results <- as.data.table(eval(parse(text = paste0("TTR::", method, "(data[, c('High', 'Low', 'Close')])"))))
	ttr_results[, Date := date_vals]
	ttr_results[, Symbol := symbol]
	return(ttr_results)
}

# master_symbols <- c("SHOP", "IBM", "AAPL", "TCEHY")

# data <- rbindlist(lapply(master_symbols, fetch_cache_symbol))
# bband_test <- rbindlist(lapply(master_symbols, apply_ttr_method, data = data, method = "BBands"))

# mf <- memoise(getSymbols, cache = cache_filesystem("~/Desktop/memoise_cache"))
# system.time(mf("TCEHY", auto.assign = F))
