#' Title
#'
#' @param position_L
#' @param sqrtpx96
#' @param t_lower
#' @param t_upper
#' @param decimal_x
#' @param decimal_y
#'
#' @return
#' @export
#'
#' @examples
get_position_balance <- function(position_L, sqrtpx96, t_lower, t_upper, decimal_x = 1e18, decimal_y = 1e18){

  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  price_lower <- price_to_sqrtpx96(P = tick_to_price(tick_lower, decimal_adjustment = decimal_adjustment),
                                   decimal_adjustment = decimal_adjustment)
  price_upper <- price_to_sqrtpx96(P = tick_to_price(tick_upper, decimal_adjustment = decimal_adjustment),
                                   decimal_adjustment = decimal_adjustment)

  token0 = -1*size_price_change_in_tick(L = position_L, sqrtpx96 = sqrtpx96,
                                     sqrtpx96_target = price_upper,
                                     dx = TRUE,
                                     decimal_scale = 1e18,
                                     fee = 0)
  token1 = -1*size_price_change_in_tick(L = position_L,
                                     sqrtpx96 = sqrtpx96,
                                     sqrtpx96_target = price_lower,
                                     dx = FALSE,
                                     decimal_scale = 1e18,
                                     fee = 0)

  list(
    token0 = as.numeric(token0),
    token1 = as.numeric(token1)
  )

}

L = as.bigz('343255264548669212')
tick_lower = -50100
tick_upper = -39120
sqrtpx96 = as.bigz('7632249339194475209177795127')

get_position_balance(position_L = L, sqrtpx96 = sqrtpx96, t_lower = tick_lower, t_upper = tick_upper)

