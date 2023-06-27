
#' Get Closest Tick
#'
#' Depending on the Uniswap v3 Pool fee tier, only specific ticks are allowed to be
#' used in positions. In 0.05 percent pools, the tick spacing is 10 minimum. In 0.3 percent, the minimum is 60 ticks.
#' This function identifies the closest allowed tick given tick spacing.
#'
#' @param desired_price Your desired price, note it is important to know the unit of account. 0.05 BTC/ETH is 20 ETH/BTC.
#' @param tick_spacing The pool's minimum tick spacing, default 0.3 percent pool, i.e., 60. Use `tick_spacing = 1` to inverse ?tick_to_price.
#' Pool addresses have tickSpacing in their contract on etherscan. Generally for v3: 0.01\% pools are 1. 0.05\% pools are 10. 1\% pools are 200.
#' @param decimal_adjustment The difference in the tokens decimals, e.g., 1e10 for ETH vs BTC. 1e12 for USDC vs ETH; 1 (default) for most ERC20/ETH.
#' @param yx Whether price is already in Token 1 / Token 0 format or inverted. ETH per USDC is how the ETH Mainnet 0.05\% pool works but is not friendly for human interpretation. Default is TRUE.
#'
#' @return a list of `desired_price` (the input), the closest allowable price `actual_price`, and the `tick` of that allowable price.
#' @export
#' @examples
#' # 0.05 BTC / ETH is NOT Y/X accounting for the pool, so yx = FALSE. Result is 260220.
#' get_closest_tick(0.05,
#' tick_spacing = 60, # 0.3 percent fee pool
#' decimal_adjustment = 1e10, # ETH 18 decimals vs BTC 8 decimals.
#'  yx = FALSE)
#'
#' # same as below where price is inverted. Again, closest allowable tick: 260220
#' get_closest_tick(20, tick_spacing = 60, decimal_adjustment = 1e10, yx = TRUE)
#'
#' # You can use tick_spacing = 1 to get the exact reverse of ?tick_to_price but it may not be allowed for making a new position.
#' get_closest_tick(desired_price = 0.05004423, tick_spacing = 1, decimal_adjustment = 1e10, yx = FALSE)$tick == 260220

get_closest_tick <- function(desired_price, tick_spacing = 60, decimal_adjustment = 1e0, yx = TRUE){

    r <- list(
    desired_price = desired_price,
    actual_price = NULL,
    tick = NULL
  )

  # if price is NOT Y/X formatted already,
  # invert for tick calculation, then invert prices for human readability.
  # results in same exact tick.
  if(yx == FALSE){
    r <- get_closest_tick(desired_price = desired_price^-1,
                          tick_spacing = tick_spacing,
                          decimal_adjustment = decimal_adjustment,
                          yx = TRUE)
    r$desired_price <- r$desired_price^-1
    r$actual_price <- r$actual_price^-1
    return(r)
  }

  initial_tick <- log( sqrt(desired_price * decimal_adjustment), sqrt(1.0001) )

  if(initial_tick %% tick_spacing == 0){
    r$actual_price <- desired_price # exact match
    r$tick <- initial_tick
  } else {
    final_tick <- round(initial_tick / tick_spacing)*tick_spacing
    r$tick <- final_tick
    r$actual_price <- sqrt(1.0001)^(2*final_tick) / decimal_adjustment
  }

  return(r)

}
