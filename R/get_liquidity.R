#' Calculate Liquidity
#'
#' This function calculates the liquidity provided by a range using its amount of tokens
#' in their full decimal form (i.e., convert ETH to Wei by multiplying by 1e18 beforehand!).
#' It takes human readable prices P, pa, pb, for simplicity, so do not use price_to_sqrtpx96() beforehand.
#'
#' @param x amount of token 0 in the exact form, i.e., adjust for decimals (e.g., x1e6 for USDC). Use gmp::as.bigz() as needed.
#' @param y amount of token 1, in the exact form,  i.e., adjust for decimals (e.g., x1e18 for ETH). Use gmp::as.bigz() as needed.
#' @param P Price, ideally in token 1/token 0 format. If not, use yx = FALSE.
#' @param pa Minimum price of a range, e.g., 0.05 in a 0.05 - 0.25 BTC/ETH position range.
#' @param pb Maximum price of a range, e.g., 0.25 in a 0.05 - 0.25 BTC/ETH position range.
#' @param yx Default TRUE, assumes Price, P, is in Y/X (Token 1 / Token 0) format as defined in Uni v3 contracts.
#' Use FALSE to invert P and swap+invert pa and pb.
#'
#' @return Returns a single Big Integer value
#' @import gmp
#' @export
#'
#' @examples
#' # See: https://science.flipsidecrypto.xyz/uni_v3_explained/#Tracking_Liquidity
#'
#' x = gmp::as.bigz('1139289230675491064') # 1.13 LINK, token 0, 18 decimals
#' y = gmp::as.bigz(0.005*1e18) # 0.005 MKR, token 1, 18 decimals
#'
#' # see sqrtpx96_to_price('7625888651129286871474510862') using Slot0 at time
#' P = 0.009264495 #  This is MKR/LINK (Token 1 / Token 0, so we'll use yx = TRUE in get_liquidity)
#'
#' # It is required that min_tick < max_tick.
#' min_tick <- -50100
#' max_tick <- -39120
#'
#' # Such that, price might need to be inverted and min and max switched
#' # to enforce min_tick < max_tick when calculating liquidity.
#' # Here, because 1 MKR (Token 1) > 1 LINK (Token 0) ticks are negative and yx = FALSE is used to get pa and pb
#' # pa, min price, is the lowest amount of MKR for 1 LINK (LINK is relatively low value compared to MKR)
#' # pa, max price, is highest amount of MKR for 1 LINK (LINK is relatively high value compared MKR)
#' # Recall at & below the minimum price the position is 100% Token 0 (LINK), and
#' # at & above the maximum price the position is 100% Token 1 (MKR).
#'
#' pa = 0.006672574 # tick_to_price(-50100, 1e0, yx = FALSE)
#' pb = 0.02000437  # tick_to_price(-39120, 1e0, yx = FALSE)

#' # Uniswap value:    343255264548669212
#' # Calculated value: 343255263830421644
#' # Within 0.000001% precision loss
#' as.numeric(
#'  get_liquidity(x = x, y = y, P = P, pa = pa, pb = pb, yx = TRUE) / 343255264548669212
#'  ) - 1 < 0.0000001
get_liquidity <- function(x, y, P, pa, pb, yx = TRUE){

  # if prices are in Y/X format, invert them and act as X/Y
  if(yx == FALSE){
    return(get_liquidity(x, y, P = P^-1, pa = pb^-1, pb = pa^-1, yx = TRUE))
  }

  # include minimum of Lx and Ly as done in contract
  # formal math in Uni v3 implemented w/ sqrt px96 price formats

  getLiq_amount0 <- function(mintickx96, maxtickx96, amount0){
    if( mintickx96 >  maxtickx96){
      temp = mintickx96
      mintickx96 <- maxtickx96
      maxtickx96 <- temp
    }
    intermediate = mintickx96*maxtickx96/gmp::as.bigz(2^96)

    return(amount0*intermediate / (maxtickx96 - mintickx96))
  }

  getLiq_amount1 <- function(mintickx96, maxtickx96, amount1){
    if( mintickx96 >  maxtickx96){
      temp = mintickx96
      mintickx96 <- maxtickx96
      maxtickx96 <- temp
    }

    return(amount1*gmp::as.bigz(2^96)/(maxtickx96 - mintickx96))
  }

  getLiq <- function(current_pricex96, mintickx96, maxtickx96, amount0, amount1){
    if( mintickx96 >  maxtickx96){
      temp = mintickx96
      mintickx96 <- maxtickx96
      maxtickx96 <- temp
    }

    if(current_pricex96 <= mintickx96){
      liq = getLiq_amount0(mintickx96, maxtickx96, amount0)
    } else if (current_pricex96 < maxtickx96){
      liq0 <- getLiq_amount0(current_pricex96, maxtickx96, amount0)
      liq1 <- getLiq_amount1(mintickx96, current_pricex96, amount1)

      #ifelse() cannot handle subassignment of big rationals
      if(liq0 < liq1){
        liq <- liq0
      } else {
        liq <- liq1
      }

    } else {
      liq = getLiq_amount1(mintickx96, maxtickx96, amount1)
    }

    return(liq)

  }

  # always return integer
  Lx = gmp::as.bigz(getLiq_amount0(mintickx96 = price_to_sqrtpx96(pa),
                                   maxtickx96 = price_to_sqrtpx96(pb),
                                   amount0 = x))

  Ly = gmp::as.bigz(getLiq_amount1(mintickx96 = price_to_sqrtpx96(pa),
                                   maxtickx96 = price_to_sqrtpx96(pb),
                                   amount1 = y))

  L = gmp::as.bigz(getLiq(current_pricex96 = price_to_sqrtpx96(P),
                          mintickx96 = price_to_sqrtpx96(pa),
                          maxtickx96 = price_to_sqrtpx96(pb),
                          amount0 = x,
                          amount1 = y))

  return(L)
}

