#' Price to SQRTPX96
#'
#' Uniswap stores prices as square roots in 64.96 (64 bits integer, 96 bit fractional)
#' assume sqrt price is a rational and use gmp big integer
#'
#' @param P price in standard form (e.g., 0.05 BTC/ETH). Be aware of the pool specific
#' token 0 and token 1 as this function takes your number as is.
#'
#' @return Big Integer price in sqrtpx96 format.
#' @export
#'
#' @examples
#' price_to_sqrtpx96(0.05) == gmp::as.bigq('17715955711429570570949230592')
price_to_sqrtpx96 <- function(P){

    gmp::as.bigq(sqrt(P)) * gmp::as.bigz(2)^96

}
