#' Swap Within a Tick
#'
#' @description This function calculates the amount of an asset coming out of a trade from a pool
#' given the active Liquidity, the current price, the amount of the other asset, and the pool fee.
#' For simplicity it supports decimal separation to retain human readable swaps.
#'
#' @param L active amount of liquidity in the pool, as big integer.
#' See ?get_liquidity or read a pool contract's liquidity directly on etherscan to get this value.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param dx the human readable amount of token 0 you are trading with the pool (i.e., adjust for decimal using decimal_x not here). NULL if you are instead trading token 1.
#' @param dy NULL if you are trading token 0. Otherwise, the human readable amount of token 1 you are trading with the pool (i.e., adjust for decimal using decimal_y not here).
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#'
#' @return Swap functions return lists:
#' \item{liquidity}{ amount of liquidity in the pool after the trade (will always match L in swap_within_tick() this function does not auto-recalculate liquidity. See ?swap_across_tick)}
#' \item{dx}{ the amount of token 0 added to pool (sold by user, fees separated) or if negative bought by user (taken from pool).}
#' \item{dy}{ the amount of token 1 bought by user (if negative) and taken from pool or if positive sold by user (added to pool, fees separated)).)}
#' \item{price1}{ the initial sqrtpx96 price of the pool before the swap}
#' \item{price2}{ the final sqrtpx96 price of the pool after the swap}
#' \item{fee}{ the fee taken by the pool, in the unit of token added to the pool (input dx = returned dx + fee)}
#'
#' @export
#' @details This function assumes the amounts traded do not cause a recalculation of active liquidity.
#' If you have access to the liquidity across positions in a pool (including those out of range that may become in-range during a swap)
#' you should instead use ?swap_across_ticks.
#' This function should be within tolerable precision loss for small swaps on deep pools if
#' a positions table is not available even if liquidity would've been technically recalculated.
#' @examples
#' data(ethwbtc_trade_history)
#' data(ethwbtc_net_liquidity)
#' blockheight <- 16115539
#' sqrtpx96 <- gmp::as.bigz("28920208462486575390334957222100992")
#' # rough estimate that is within 0.01\% is to take price from trade data tick.
#' most_recent_trade_tick <- tail(ethwbtc_trade_history[ethwbtc_trade_history$block_number < blockheight, "tick"], 1)
#' sqrtptx96_from_tick <- price_to_sqrtpx96(P = tick_to_price(most_recent_trade_tick, 1e10), invert = FALSE, 1e10)
#' abs(as.numeric(sqrtptx96_from_tick/sqrtpx96)) - 1 < 0.00001 # very close together
#'
#' # Using liquidity value from the in block 16115539: '1785868753774080000'
#' # knowing the swap is small enough to not cause a change in liquidity available (see ?swap_across_ticks)
#' # matches row 131,165 of ethwbtc_trade_history (only trade in block 16115539) sale of 0.03 ETH, removes 0.00224477 BTC from pool.
#' swap_within_tick(L = '1785868753774080000', sqrtpx96 = sqrtpx96, dx = NULL, dy = 0.03, decimal_x = 1e8, decimal_y = 1e18, fee = 0.003)

swap_within_tick <- function(L, sqrtpx96, dx = NULL, dy = NULL,
                             decimal_x = 1e18,
                             decimal_y = 1e18,
                             fee = 0.003){

  # Change in price = Delta(Y) / L
  # change in Inverse(Price) = Delta(X)/L

  # price in *square root* 64.96 form
  L = gmp::as.bigz(L)
  P = gmp::as.bigz(sqrtpx96)
  c96 = (gmp::as.bigz(2)^96)
  # inverse price
  iP = P^-1

  # adjust real dx or dy to 96 int & adjust for fees
  if(!is.null(dx)){
    dxa <- gmp::as.bigq(dx) * (1 - fee) * decimal_x / c96
  }
  if(!is.null(dy)){
    dya <- gmp::as.bigq(dy) * (1 - fee) * c96 * decimal_y
  }

  r = list(
    liquidity = L,
    dx = NULL,
    dy = NULL,
    price1 = P,
    price2 = NULL,
    fee = NULL
  )

  if(is.null(dx) & is.null(dy)){
    stop("A change in x or y is required to use liquidity")
  }

  if(!is.null(dx) & !is.null(dy)){
    stop("Only 1 swap can be noted")
  }

  if(!is.null(dx)){
    # iP_new - iP = dx/L
    # iP_new = dx/L + iP

    iP_new = dxa/L + iP
    P_new = iP_new^-1

    # dy = (P_new - P)*L
    dya = (P_new - P)*L

    # convert back to real units
    dyz = as.numeric( dya / c96  / decimal_y )

    r$dx = dx * (1 - fee)
    r$dy = dyz
    r$price2 <- gmp::as.bigz(P_new)
    r$fee = fee * dx

  } else if(!is.null(dy)){
    # dy =  (P_new - P)*L
    # P_new = dy/L + P

    P_new = dya/L + P

    iP_new = P_new^-1

    dxa = (iP_new - iP)*L

    # convert to real units
    dxz = as.numeric(dxa * c96 / decimal_x )

    r$dx <- dxz
    r$dy <- dy * (1 - fee)
    r$price2 <- gmp::as.bigz(P_new)
    r$fee <- fee * dy
  }

  return(r)

}
