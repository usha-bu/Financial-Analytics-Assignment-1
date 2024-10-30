myMeanVarPort <- function(tickers, start_date, end_date, risk_free_rate) {
  # Load required libraries
  library(quantmod)
  library(dplyr)
  
  # Data acquisition: get stock data from Yahoo
  getSymbols(tickers, src = 'yahoo', from = start_date, to = end_date, periodicity = 'monthly')
  
  # Calculate monthly returns for each ticker
  stock_returns <- lapply(tickers, function(ticker) {
    monthly_prices <- Ad(get(ticker))
    monthly_returns <- periodReturn(monthly_prices, period = 'monthly', type = 'log')
    colnames(monthly_returns) <- ticker
    return(monthly_returns)
  })
  
  # Combine monthly returns into a single data frame
  combined_returns <- do.call(merge, stock_returns) %>% na.locf() %>% na.omit()
  
  # Calculate mean returns and covariance matrix
  mean_returns <- colMeans(combined_returns, na.rm = TRUE)
  cov_matrix <- cov(combined_returns, use = "complete.obs")
  
  # Return the results as a list
  result <- list(
    mean_returns = mean_returns,
    cov_matrix = cov_matrix
  )
  
  return(result)
}

# Example usage: First, run the R script, then use the below code in the console. Remove the # before each line before run. 
# tickers <- c('AAPL', 'MSFT', 'GOOG')
# start_date <- '2020-01-01'
# end_date <- '2023-01-01'
# risk_free_rate <- 0.02
#result <- myMeanVarPort(tickers, start_date, end_date, risk_free_rate)

# Print the result
# print(result)
