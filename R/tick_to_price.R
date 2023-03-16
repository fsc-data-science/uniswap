
#' Tick to Price
#'
#' Converts a Uniswap v3 tick to a human readable price accounting
#' for differences in the token decimals and whether price is desired in
#' Token 1 / Token 0 (y/x) or inverted (x/y) format.
#'
#' @param tick The numeric tick, e.g., -255410.
#' @param decimal_adjustment The difference in the tokens decimals, e.g., 1e10 for ETH vs BTC. 1e12
#' for USDC vs ETH.
#' @param yx Whether tick is already in Token 1 / Token 0 format or inverted. ETH per USDC may be
#' how the pool functions but is not friendly for human interpretation. Default is false.
#'
#' @return A numeric price in desired format.
#' @export
#' @examples
#'  ethusdc_tick = 204232 # 1,351.327 USDC per ETH; 6 decimals vs 18 decimals -> 1e12 adjustment.
#'  tick_to_price(ethusdc_tick, decimal_adjustment = 1e12, yx = TRUE)
#'  tick_to_price(-ethusdc_tick, decimal_adjustment = 1e12, yx = FALSE) # same result
#'
#'  ethwbtc_tick = -260220 # 0.05004423 WBTC per ETH; 8 decimals vs 18 decimals -> 1e10 adjustment.
#' tick_to_price(ethwbtc_tick, decimal_adjustment = 1e10, yx = FALSE)
#'
tick_to_price <- function(tick, decimal_adjustment = 1, yx = FALSE){

  p <- sqrt(1.0001)^(2*tick)
  if(yx == TRUE){
    p <- p^-1
  }

  p <- p * decimal_adjustment

  return(p)
}
