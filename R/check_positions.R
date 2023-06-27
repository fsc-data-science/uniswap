#' Check Positions
#'
#' Simple helper function for flagging liquidity positions as active or not active at a specific price.
#'
#' @param ptbl Liquidity Positions table of the form tick_lower, tick_upper, liquidity. See ?ethwbtc_net_liquidity for an example.
#' This included dataset is net_liquidity, so it should be filtered to the max blockheight for a given tick_lower-tick_upper.

#' @param P Specific price in human readable format, converted to tick for flagging using ?get_closest_tick with tick_spacing = 1.
#'
#' @return The liquidity positions table with a new `active` flag column for whether the liquidity is active or not at price P.
#' @export
#'
#' @examples
#' data("ethwbtc_net_liquidity")
#' ethwbtc_net_liquidity <- check_positions(ethwbtc_net_liquidity, P = 0.10, decimal_adjustment = 1e10, yx = FALSE)
check_positions <- function(ptbl, P , decimal_adjustment = 1e0, yx = TRUE){
  if( !("tick_lower" %in% colnames(ptbl)) | !("tick_upper" %in% colnames(ptbl))){
    stop("Expected tick_lower and tick_upper columns")
  }

  tick_target <- uniswap::get_closest_tick(P, tick_spacing = 1, decimal_adjustment = decimal_adjustment, yx = yx)
  target_tick <- tick_target$tick

   # TRUE/FALSE
  ptbl$active <- (ptbl$tick_lower <= target_tick & ptbl$tick_upper >= target_tick)
  return(ptbl)
}
