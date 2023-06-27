#' Price to SQRTPX96
#' Uniswap stores prices as square roots in 64.96 (64 bits integer, 96 bit fractional)
#' assume sqrt price is a rational and use gmp big integer.
#'
#' @param sqrtpx96 The Uniswap 64.96 square root price to convert to human readable price.
#' @param invert Default FALSE. Uniswap uses Token 1 / Token 0. You must know which token is which at the pool level.
#' @param decimal_adjustment 10^(decimal difference). WBTC has 8 decimals, ETH has 18. See example.
#'
#' @return Human readable decimal price in desired format (1/0 or 0/1 if invert = TRUE). Note, small amount of
#' precision loss (<0.01%) possible due to not adjusting for Solidity implementation of
#' Chinese Remainder Theorem as solidity uses fixed point math.
#' @export
#'
#' @examples
#' # see the reverse function ?price_to_sqrtpx96
#' # For Ethereum Mainnet ETH-USDC 0.05% v3 Pool example:
#' # 1854219362252931989533640458424264 (Slot0 from Pool Contract) -> $1,825.732 USDC/ETH
#' # invert is TRUE because pool is actually ETH/USDC (Token 1 / Token 0) NOT USDC/ETH.
#' # USDC is 6 decimals while ETH is 18 decimals (18-6 = 12 decimal_adjustment)
#' sqrtpx96_to_price('1854219362252931989533640458424264', invert = TRUE, decimal_adjustment = 1e12)
#' # Returns arbitrarily precise (1825.732 + 0.0003517861 - 0.0007035722 - 0.00000000004433787)
#' # 99.99999% accurate. Some precision loss compared to fixed point math in solidity.
sqrtpx96_to_price <- function(sqrtpx96, invert = FALSE, decimal_adjustment = 1e0){
  p <- gmp::as.bigq(sqrtpx96)/(gmp::as.bigz(2)^96)

  if(invert){
  as.numeric(p^2)^(-1) * decimal_adjustment
  } else {
  as.numeric(p^2) / decimal_adjustment
  }

}
