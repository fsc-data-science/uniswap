#' Price to SQRTPX96
#'
#' Uniswap stores prices as square roots in 64.96 (64 bits integer, 96 bit fractional)
#' assume sqrt price is a rational and use gmp big integer.
#'
#' @param P price in human readable form (e.g., 0.05 BTC/ETH).
#' @param invert Default FALSE. Uniswap uses Token 1 / Token 0. You must know which token is which at the pool level.
#' @param decimal_adjustment 10^(decimal difference). WBTC has 8 decimals, ETH has 18. See example.
#'
#' @return Big Integer price in sqrtpx96 format. Note, small amount of
#' precision loss (<0.01%) possible due to not adjusting for Solidity implementation of
#' Chinese Remainder Theorem as solidity uses fixed point math.
#' @import gmp
#' @export
#'
#' @examples
#' # see the reverse function ?sqrtpx96_to_price
#' # For Ethereum Mainnet ETH-USDC 0.05% v3 Pool example:
#' # $1,825.732 USDC/ETH -> 1854219362252931989533640458424264 (Slot0 in Pool Contract)
#' # invert is TRUE because pool is actually ETH/USDC (Token 1 / Token 0) NOT USDC/ETH.
#' # USDC is 6 decimals while ETH is 18 decimals (18-6 = 12 decimal_adjustment)
#' price_to_sqrtpx96(1825.732, invert = TRUE, decimal_adjustment = 1e12)
#' # 99.99999% accurate. Some precision loss compared to fixed point math in solidity.
price_to_sqrtpx96 <- function(P, invert = FALSE, decimal_adjustment = 1e0){

  if(invert){
    P <- P^-1
  }

  gmp::as.bigq(sqrt(P*decimal_adjustment)) * gmp::as.bigz(2)^96

}
