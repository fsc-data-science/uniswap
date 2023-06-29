#' Find Recalculation Price
#'
#' Helper function for identifying the next tick where liquidity must be recalculated (i.e., at least 1 position enters or exits being active).
#' Note, pass both active and inactive positions with liquidity, see ?check_positions.
#'
#' @param ptbl Liquidity Positions table of the form tick_lower, tick_upper, liquidity. See ?ethwbtc_net_liquidity for an example.
#' This included dataset is net_liquidity, so it should be filtered to the max blockheight for a given tick_lower-tick_upper.
#' @param P Specific price in human readable format, converted to tick for flagging using ?get_closest_tick with tick_spacing = 1.
#' @param price_up Is the price rising (TRUE) or falling (FALSE) given specified format for `P`. Default TRUE.
#' @param decimal_adjustment The difference in the tokens decimals, e.g., 1e10 for ETH vs BTC. 1e12 for USDC vs ETH; 1 (default) for most ERC20/ETH.
#' @param yx Whether price is already in Token 1 / Token 0 format or inverted. ETH per USDC is how the ETH Mainnet 0.05\% pool works but is not friendly for human interpretation. Default is TRUE.
#'
#' @return Returns human readable price in the format provided (Y/X or X/Y). Best to use Y/X to align to how it is used in ?swap_across_ticks
#' @export
#'
#' @examples
#' ptbl <- ethwbtc_net_liquidity
#' # filter to a small subset for easier manual review
#' ptbl <- ptbl[(ptbl$tick_lower < 100000 & ptbl$tick_lower > 0), ]
#' # append price conversions for easier manual review
#' ptbl$price_lower <- tick_to_price(ptbl$tick_lower, 1e10, TRUE)
#' ptbl$price_upper <- tick_to_price(ptbl$tick_upper, 1e10, TRUE)
#' # 39.36252 ETH/BTC current price in correct yx format
#' P = 39.36252
#' tick = 267180
#' # Check for next higher price. 1e10 decimal_adjustment.
#' # Liquidity needs to be recalculated at 40.07743 ETH / BTC (the 92100-267180 position falls out of range!)
#' rprice <- find_recalculation_price(ptbl, P, price_up = TRUE, 1e10, yx = TRUE)
#' rprice

find_recalculation_price <- function(ptbl, P, price_up = TRUE, decimal_adjustment = 1e0, yx = TRUE){

  if( !("tick_lower" %in% colnames(ptbl)) | !("tick_upper" %in% colnames(ptbl))){
    stop("Expected tick_lower and tick_upper columns")
  }

  # for making positions tick_spacing = 1 is only valid for 0.01 percent pools, but this works for inside a valid range
  tick_target <- uniswap::get_closest_tick(P, tick_spacing = 1, decimal_adjustment = decimal_adjustment, yx = yx)

  target_tick <- tick_target$tick

  relevant_ticks <- sort(unique(c(ptbl[["tick_lower"]], ptbl[["tick_upper"]])))

  # if price going up, get the closest price above current price where liquidity changes
  # if price going down, get the closest price below current price where liquidity changes
  if( price_up == TRUE & yx == TRUE ){
    closest_tick <- relevant_ticks[which(relevant_ticks > target_tick)[1]]
  } else if( price_up == FALSE & yx == TRUE ) {
    closest_tick <- relevant_ticks[max(which(relevant_ticks < target_tick))]
  }  else {
    # if yx is FALSE, invert everything, re-calculate, and invert the result.
    return(find_recalculation_price(ptbl, P^-1, !price_up, decimal_adjustment, !yx)^-1)
  }

  closest_price <- tick_to_price(closest_tick, decimal_adjustment = decimal_adjustment, yx = yx)

  return(closest_price)
}
