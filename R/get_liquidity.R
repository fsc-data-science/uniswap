#' Calculate Liquidity
#'
#' This function calculates the liquidity provided by a range using its amount of tokens (adjusting for their decimals),
#' the current price in sqrtpx96 format (see ?price_to_sqrtpx96), and the range (tick_lower, tick_upper)
#'
#' @param x number of token 0, e.g., WBTC in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param y number of token 1, e.g., ETH in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param tick_lower The low tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param tick_upper The upper tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#'
#' @return Returns a single Big Integer value, the liquidity contributed by the position.
#' @import gmp
#' @export
#'
#' @examples
#' # See: https://etherscan.io/tx/0xc10cddba3df56e6fba4f9a88b132cc9d4440ff31bb0c4926dc9d9ca652faf376#eventlog
#' # In Block 12,376,757: 1 BTC and 16.117809469 ETH were added to pool with a range of 257760 to 258900
#' # the price at the time was 16.52921 ETH / BTC (sqrtpx96 = 32211102662183904786754519772954624)
#' # it resulted in NFT Position 1005 with liquidity 1429022393248418
#' # Within 0.0001% (some precision loss expected)
#' get_liquidity(x = 1, y = 16.117809469,
#' sqrtpx96 = gmp::as.bigz('32211102662183904786754519772954624'),
#' decimal_x = 1e8, decimal_y = 1e18,
#' tick_lower = 257760,
#' tick_upper = 258900) / gmp::as.bigz('1429022393248418') - 1 < 0.000001

get_liquidity <- function(x, y, sqrtpx96, decimal_x = 1e18, decimal_y = 1e18, tick_lower, tick_upper){

  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  mintickx96 <- price_to_sqrtpx96(P = tick_to_price(tick = tick_lower, decimal_adjustment),
                                  decimal_adjustment = decimal_adjustment)
  maxtickx96 <- price_to_sqrtpx96(P = tick_to_price(tick = tick_upper, decimal_adjustment),
                                  decimal_adjustment = decimal_adjustment)

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

  #
  Lx = gmp::as.bigz(getLiq_amount0(mintickx96 = mintickx96,
                                   maxtickx96 = maxtickx96,
                                   amount0 = x*decimal_x))

  Ly = gmp::as.bigz(getLiq_amount1(mintickx96 = mintickx96,
                                   maxtickx96 = maxtickx96,
                                   amount1 = y*decimal_y))

  L = gmp::as.bigz(getLiq(current_pricex96 = sqrtpx96,
                          mintickx96 = mintickx96,
                          maxtickx96 = maxtickx96,
                          amount0 = x*decimal_x,
                          amount1 = y*decimal_y))

  return(L)
}

