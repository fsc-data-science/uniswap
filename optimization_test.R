# Optimal Range

# LINK/MKR Optimism
library(gmp)
library(dplyr)
library(optimx)


# Liquidity 362438256514101749
# sqrtPriceX96   uint160 :  7632249339194475209177795127

L = as.bigz('362438256514101749')
p1 <- sqrtpx96_to_price('7632249339194475209177795127')


price_range <- seq(0.5, 1.5, 0.01)
price_sets <- expand.grid(price_range, price_range)
colnames(price_sets) <- c("pax", "pbx")
price_sets <- price_sets[ price_sets$pax < 1 & price_sets$pbx > 1, ]

# Base of 100 LINK tokens; p1 = MKR/LINK (Token 1 / Token 0) 0.09
acc <- as.data.frame(curve(expr = (100 - x)*p1, from = 100, to = 0, n = 1000))


# Define the objective function
quadratic_function <- function(x) {
  return((x - 3)^2 + 2)
}

# Optimize the function
initial_guess <- 0
optim_result <- optimx(par = initial_guess, fn = quadratic_function, method = "BFGS")

# Display the results
print(optim_result)

# Define the Rosenbrock function
rosenbrock_function <- function(params) {
  x <- params[1]
  y <- params[2]
  return(100 * (y - x^2)^2 + (1 - x)^2)
}

# Optimize the function with constraints
initial_guess <- c(1.5, 1)
lower_bounds <- c(1, 0)
upper_bounds <- c(2, 3)

optim_result <- optimx(
  par = initial_guess,
  fn = rosenbrock_function,
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds
)

# Display the results
print(optim_result)
