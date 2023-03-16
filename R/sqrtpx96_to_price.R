#' Price to SQRTPX96
#' Uniswap stores prices as square roots in 64.96 (64 bits integer, 96 bit fractional)
#' assume sqrt price is a rational and use gmp big integer.
#'
#' @param sqrtpX96 The Uniswap 64.96 square root price to convert to human readable price.
#' Note: Uniswap pools always use Token 0 / Token 1 pricing. So invert result as you see fit.
#'
#' @return Human readable decimal price (Token 0 / Token 1 format). Note, small amount of
#' precision loss (<0.01%) possible due to not adjusting for Solidity implementation of
#' Chinese Remainder Theorem as solidity uses fixed point math.
#' @export
#'
#' @examples
#' # returns FALSE b/c of precision.
#' sqrtpx96_to_price('17715955711429570570949230592') == 0.05
#' # but difference is < 1e-15
#' abs(sqrtpx96_to_price('17715955711429570570949230592') - 0.05) < 1e-15
#' # see ?price_to_sqrtpx96
sqrtpx96_to_price <- function(sqrtpX96){
  p <- gmp::as.bigq(sqrtpX96)/(gmp::as.bigz(2)^96)
  as.numeric(p^2)
}
