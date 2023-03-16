#' Swap Within a Tick
#'
#' @description This function calculates the amount of assets coming out of a trade from a pool
#' given the active Liquidity, the current price, the amounts of each asset, and the pool fee.
#' For simplicity it supports decimal separation to retain human readable swaps.
#'
#' @param L active amount of liquidity in the pool, as big integer.
#' See get_liquidity() or read a pool contract's liquidity directly on etherscan to get this value.
#' @param sqrtpx96 current price in uint160 format.
#' See price_to_sqrtpx96() or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param dx the human readable amount of token 0 you are trading with the pool.
#' Otherwise, NULL if you are instead trading token 1.
#' @param dy NULL if you are trading token 0.
#' Otherwise, the human readable amount of token 1 you are trading with the pool.
#' @param decimal_x The decimals used in token 0, e.g., 1e18 for ETH, 1e6 for USDC.
#' @param decimal_y The decimals used in token 1, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#'
#' @return Swap functions return lists. For a token 0 -> token 1 swap a list of:
#' \item{liquidity}{amount of liquidity in the pool after the trade (will always match L in swap_within_tick())}
#' \item{dx}{ the amount of token 0 added (i.e. sold by user) to the pool (FEES separated)}
#' \item{dy}{ the amount of token 1 removed (i.e. bought by user) from the pool}
#' \item{price1}{ the initial sqrtpx96 price of the pool before the swap}
#' \item{price2}{ the final sqrtpx96 price of the pool after the swap}
#' \item{fee}{ the fee taken by the pool, in the unit of token added to the pool (input dx = returned dx + fee)}
#
#' @export
#' @details This function assumes the amounts traded do not cause a recalculation of active liquidity.
#' If you have access to all available positions in a pool (including those out of range that may become in-range during a swap)
#' you should instead use swap_across_ticks().
#' This function should be within tolerable precision loss for small swaps on deep pools if
#' the positions table is not available.
#' @examples
#' # Trading LINK for MKR on an Optimism pool.
#' # see ?get_liquidity
#' L = '343255264548669212'
#' # see ?price_to_sqrtpx96
#' P = 0.009264495
#' swap_within_tick(L = L,
#' sqrtpx96 = price_to_sqrtpx96(P),
#' dx = 0.000000030675491064, # giving tiny amount of LINK
#' dy = NULL, # how much MKR will be returned?
#' decimal_x = 1e18, # LINK has 18 decimals
#' decimal_y = 1e18, # MKR has 18 decimals
#' fee = 0.003) # 0.3% pool
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
    dyz = as.numeric( dya / c96 ) / decimal_y

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
    dxz = as.numeric(dxa * c96) / decimal_x

    r$dx <- dxz
    r$dy <- dy * (1 - fee)
    r$price2 <- gmp::as.bigz(P_new)
    r$fee <- fee * dy
  }

  return(r)

}
