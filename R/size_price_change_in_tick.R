
#' Size Price Change in Tick
#'
#' Given liquidity, a current price, a target price, and the fee tier of a pool
#' calculate how large a trade would be required to get to the target price. This is used to identify
#' the swap size that would trigger a liquidity recalculation (i.e., it is possible a position fell out of range).
#' Lower fee tiers have more narrow allowable ticks for positions, so they have more recalculation checks.
#'
#' @param L active raw amount of liquidity in the pool, as big integer. (Do not use adjusted_liquidity without scaling up 1e13 or answer will be off).
#' See ?get_liquidity or read a pool contract's liquidity directly on etherscan to get this value.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param sqrtpx96_target target price in uint160 format. See ?price_to_sqrtpx96.
#' @param dx TRUE or FALSE. Whether the amount needed to trade should be denominated in token 0 (TRUE) or token 1 (FALSE).
#' @param decimal_scale decimal of token 0 (if dx == TRUE) or decimal of token 1 (if dx == FALSE). NOT decimal_adjustment between the two. 1e6 for USDC. 1e8 for WBTC, 1e18 for WETH etc.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#'
#' @return Returns the human readable (i.e., decimal adjusted) amount the trader needs to trade in the desired unit (token 0 for dx = TRUE)
#' fee already included (i.e., a .997 ETH pool difference for price target in a 0.3\% pool will return `1` because 1 ETH pays 0.003 ETH in
#' fees that are separate from how the pool reacts to trade amounts).
#' Positive results are adds to the pool (trader sells); negative results are removes from pool (trader buys).
#' @export
#'
#' @examples
#' # See: https://science.flipsidecrypto.xyz/uni_v3_explained/#Swap_across_Ticks
#' # Move LINK/MKR 0.3\% Pool on Optimism from current price to new price
#' # Will match 0.00000003067549 LINK added to pool (i.e., trader sells LINK)
#' # precision tolerance within 0.0001%:
#' #  9.999999999999/10 - 1 < 0.000001
#' size_price_change_in_tick(
#' L = "343255264548669212",
#' sqrtpx96 = "7625888646051765535543132160",
#' sqrtpx96_target = '7625888580652810738255925731',
#'  # return the amount of token 0 which is LINK
#' dx = TRUE,
#' decimal_scale = 1e18, # LINK is standard ERC20
#' fee = 0.003)
#'
#' # dx = FALSE shows
#' # -0.0000000002841929 MKR removed from pool (i.e., bought by Trader).
#' size_price_change_in_tick(
#' L = "343255264548669212",
#' sqrtpx96 = "7625888646051765535543132160",
#' sqrtpx96_target = '7625888580652810738255925731',
#' # return the amount of token 0 which is LINK
#' dx = FALSE,
#' decimal_scale = 1e18, # MKR is standard ERC20
#' fee = 0.003)
#'
size_price_change_in_tick <- function(L, sqrtpx96, sqrtpx96_target, dx = TRUE,
                                      decimal_scale = 1e0, fee = 0.003){

  # price in *square root* 64.96 form
  L = gmp::as.bigz(L)
  P = gmp::as.bigz(sqrtpx96)
  P_target = gmp::as.bigz(sqrtpx96_target)
  c96 = gmp::as.bigz(2)^96
  # inverse price
  iP = P^-1
  iP_target = P_target^-1

  if(dx == TRUE){
    dxa = (iP_target - iP) * L
    dx = gmp::as.bigq(dxa) / (1 - fee) / decimal_scale * c96
    return(as.numeric(dx))
  } else {
    dya = (P_target - P)*L
    dy = dya / (1 - fee) / c96 / decimal_scale
    return(as.numeric(dy))
  }
}
