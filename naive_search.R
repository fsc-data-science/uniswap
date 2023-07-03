data("ethwbtc_trade_history")
block_min = 16000000
block_max = 16010000
trades_16m10k <- ethwbtc_trade_history[ethwbtc_trade_history$block_number %in% (block_min:block_max), ]

# Given initial & final prices
# as the first and last prices in the relevant swaps
p1 = tick_to_price(tick = head(trades_16m10k$tick, n = 1), decimal_adjustment = 1e10)
sqrtpx96_1 <- price_to_sqrtpx96(p1, F, 1e10)
p2 = tick_to_price(tick = tail(trades_16m10k$tick, n = 1), decimal_adjustment = 1e10)
sqrtpx96_2 <- price_to_sqrtpx96(p2, F, 1e10)


# Budget of 100 ETH
budget = 100

# Use Naive Search to estimate initial parameters for optimization
low_price <- ((1:9)/10)*p1
amount_1 <- budget*(1:9)/10

grid <- expand.grid(x = amount_1, y = low_price)

sv = rep(0, nrow(grid))

for(i in 1:nrow(grid)){
  sv[i] <- tryCatch({
    calculate_profit(
      params = c(grid[i,1], grid[i,2]),
      budget = 100, p1 = p1, p2 = p2, trades = trades_16m10k,
      decimal_x = 1e8, decimal_y = 1e18, fee = 0.003,
      denominate = 1,
      in_optim = TRUE)
  }, error = function(e){return(0)})
}

# initialize using naive search min
init_params <- as.numeric(grid[which.min(sv), 1:2])

# Define lower and upper bounds for ETH and price_lower
# amount1 is 0.01 - 99.9
# price_lower is 1 ETH/BTC - 0.999*P1
 lower_bounds <- c(0.01, 1)
 upper_bounds <- c(99.9, 0.99*p1)

# maximize profit using L-BFGS-B and select trades
# denominate in ETH
result <- optim(init_params,
                calculate_profit,
                method = "L-BFGS-B",
                lower = lower_bounds,
                upper = upper_bounds, budget = 100, p1 = p1, p2 = p2, trades = trades_16m10k,
                decimal_x = 1e8, decimal_y = 1e18, fee = 0.003, denominate = 1, in_optim = TRUE)

if(result$convergence == 52){
  warning(result$message)
}
# results
calculate_profit(params = result$par,
                 budget = 100, p1 = p1, p2 = p2, trades = trades_16m10k,
                 decimal_x = 1e8, decimal_y = 1e18, fee = 0.003, denominate = 1, in_optim = FALSE)




