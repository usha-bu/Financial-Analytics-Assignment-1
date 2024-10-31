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
  
  # Simulate portfolios
  set.seed(12)
  num_portfolios <- 100 * num_assets
  num_assets <- length(tickers)
  simulated_portfolios <- replicate(num_portfolios, {
    weights <- runif(num_assets)
    weights <- weights / sum(weights)  # Normalize weights to sum to 1
    portfolio_return <- sum(weights * mean_returns)
    portfolio_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
    sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_risk
    c(weights, portfolio_return, portfolio_risk, sharpe_ratio)
  })
  
  # Convert simulated portfolios to a data frame
  portfolio_data <- as.data.frame(t(simulated_portfolios))
  colnames(portfolio_data) <- c(paste0("Weight_", tickers), "Mean_Return", "Risk", "Sharpe_Ratio")
  
  # Return the results as a list
  result <- list(
    mean_returns = mean_returns,
    cov_matrix = cov_matrix,
    portfolio_data = portfolio_data
  )
  
  return(result)
}
