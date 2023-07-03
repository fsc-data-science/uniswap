#' Get Position Balance
#'
#' Given the marginal liquidity of a position (see ?ethwbtc_net_liquidity) and the current price, identify
#' the balance of assets in that position (e.g., 1.13 LINK and 0.005 MKR in a MKR/LINK Uni v3 NFT).
#'
#' @param position_L The marginal liquidity provided by a position. This is usable on `ethwbtc_net_liquidity` but it would be the balance of all positions in a range.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param tick_lower The low tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param tick_upper The upper tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#'
#' @return a list of `token0` (x) balance and `token1` (y) balance adjusted for the provided decimals for a given position at any price
#'
#' @export
#'
#' @examples
#' # This is real world MKR/LINK position on Optimism. In the range of 0.00667 - 0.02 MKR/LINK
#' # given 343255264548669212 liquidity, at the price 0.00928 MKR/LINK price, there are
#' #  1.136317 LINK and 0.005027558 MKR in the position.
#' tick_lower = -50100
#' tick_upper = -39120
#' L = gmp::as.bigz('343255264548669212')
#' sqrtpx96 = gmp::as.bigz('7632249339194475209177795127')
#'
#' get_position_balance(position_L = L, sqrtpx96 = sqrtpx96,
#' tick_lower = tick_lower, tick_upper = tick_upper,
#' decimal_x = 1e18, decimal_y = 1e18)

get_position_balance <- function(position_L, sqrtpx96, tick_lower, tick_upper, decimal_x = 1e18, decimal_y = 1e18){

  position_L = gmp::as.bigz(position_L)
  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  price_lower <- price_to_sqrtpx96(P = tick_to_price(tick_lower, decimal_adjustment = decimal_adjustment),
                                   decimal_adjustment = decimal_adjustment)
  price_upper <- price_to_sqrtpx96(P = tick_to_price(tick_upper, decimal_adjustment = decimal_adjustment),
                                   decimal_adjustment = decimal_adjustment)

  # if price is above of range you're all token 1
  if(sqrtpx96 >= price_upper){
    token0 = 0
    token1 = size_price_change_in_tick(L = position_L,
                                        sqrtpx96 = price_lower,
                                        sqrtpx96_target = price_upper,
                                        dx = FALSE,
                                        decimal_scale = decimal_y,
                                        fee = 0)

  # if price is below range you're all token 0
  } else if(sqrtpx96 <= price_lower){
    token0 = size_price_change_in_tick(L = position_L, sqrtpx96 = price_upper,
                                       sqrtpx96_target = price_lower,
                                       dx = TRUE,
                                       decimal_scale = decimal_x,
                                       fee = 0)
    token1 = 0

    # else if you're position is in range
  } else if(sqrtpx96 >= price_lower & sqrtpx96 <= price_upper ){

  token0 = size_price_change_in_tick(L = position_L, sqrtpx96 = sqrtpx96,
                                     sqrtpx96_target = price_upper,
                                     dx = TRUE,
                                     decimal_scale = decimal_x,
                                     fee = 0)

  token1 = size_price_change_in_tick(L = position_L,
                                     sqrtpx96 = sqrtpx96,
                                     sqrtpx96_target = price_lower,
                                     dx = FALSE,
                                     decimal_scale = decimal_y,
                                     fee = 0)
  } else {
    stop("Double check tick_upper > tick_lower.")
  }

  list(
    token0 = abs(as.numeric(token0)),
    token1 = abs(as.numeric(token1))
  )

}



