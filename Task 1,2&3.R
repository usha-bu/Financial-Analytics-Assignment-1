# Load all required libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Part 1: Portfolio Optimization Function
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
  num_assets <- length(tickers)
  num_portfolios <- 100 * num_assets
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

# Part 2: Portfolio Visualization Function
plot_efficient_frontier <- function(portfolio_data) {
  # Find the optimal portfolio (highest Sharpe ratio)
  optimal_portfolio <- portfolio_data[which.max(portfolio_data$Sharpe_Ratio), ]
  
  # Find the minimum variance portfolio
  min_var_portfolio <- portfolio_data[which.min(portfolio_data$Risk), ]
  
  # Create the plot
  p <- ggplot(portfolio_data, aes(x = Risk, y = Mean_Return)) +
    # Plot all portfolios
    geom_point(alpha = 0.5, color = "darkblue", size = 1) +
    
    # Add optimal portfolio point
    geom_point(data = data.frame(Risk = optimal_portfolio$Risk, Mean_Return = optimal_portfolio$Mean_Return), color = "red", size = 3) +
    
    # Add minimum variance portfolio point
    geom_point(data = data.frame(Risk = min_var_portfolio$Risk, Mean_Return = min_var_portfolio$Mean_Return), color = "green", size = 3) +
    
    geom_label_repel(
      data = data.frame(
        Risk = c(optimal_portfolio$Risk, min_var_portfolio$Risk),
        Mean_Return = c(optimal_portfolio$Mean_Return, min_var_portfolio$Mean_Return),
        Label = c(
          sprintf("Optimal Portfolio\nSharpe Ratio: %.4f", optimal_portfolio$Sharpe_Ratio),
          sprintf("Minimum Variance Portfolio\nRisk: %.4f", min_var_portfolio$Risk)
        )
      ),
      aes(label = Label),
      box.padding = 0.5,
      force = 3
    ) +
    theme_minimal() +
    
    labs(
      title = "Portfolio Optimization Results",
      subtitle = "Efficient Frontier with Optimal and Minimum Variance Portfolios",
      x = "Portfolio Risk (Standard Deviation)",
      y = "Expected Return",
      caption = "Note: Red point indicates optimal portfolio, green point indicates minimum variance portfolio"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(face = "bold")
    )
  
  return(p)
}

# Part 3: Portfolio Details Function
print_portfolio_details <- function(portfolio_data, tickers) {
  # Find optimal portfolio
  optimal_idx <- which.max(portfolio_data$Sharpe_Ratio)
  optimal_portfolio <- portfolio_data[optimal_idx, ]
  
  # Find minimum variance portfolio
  min_var_idx <- which.min(portfolio_data$Risk)
  min_var_portfolio <- portfolio_data[min_var_idx, ]
  
  # Print optimal portfolio details
  cat("\nOptimal Portfolio Details:\n")
  cat("------------------------\n")
  cat("Expected Return:", round(optimal_portfolio$Mean_Return * 100, 2), "%\n")
  cat("Risk:", round(optimal_portfolio$Risk * 100, 2), "%\n")
  cat("Sharpe Ratio:", round(optimal_portfolio$Sharpe_Ratio, 4), "\n")
  cat("Weights:\n")
  for(i in 1:length(tickers)) {
    cat(sprintf("%s: %.2f%%\n", tickers[i],optimal_portfolio[[paste0("Weight_", tickers[i])]] * 100))
  }
  
  # Print minimum variance portfolio details
  cat("\nMinimum Variance Portfolio Details:\n")
  cat("----------------------------------\n")
  cat("Expected Return:", round(min_var_portfolio$Mean_Return * 100, 2), "%\n")
  cat("Risk:", round(min_var_portfolio$Risk * 100, 2), "%\n")
  cat("Sharpe Ratio:", round(min_var_portfolio$Sharpe_Ratio, 4), "\n")
  cat("Weights:\n")
  for(i in 1:length(tickers)) {
    cat(sprintf("%s: %.2f%%\n", tickers[i], min_var_portfolio[[paste0("Weight_", tickers[i])]] * 100))
  }
}