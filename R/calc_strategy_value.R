#' Calculate Strategy Value
#'
#' Given a set of trades for  position to accumulate fees, consolidate the position value and fees accrued
#' denominated in the asset of your choice, using ?calc_fees_from_trades and ?get_position_balance.
#'
#' @param position_L The marginal liquidity provided by a position. See ?get_liquidity.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param tick_lower The low tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param tick_upper The upper tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param trades Trades table with columns tick, amount0_adjusted, amount1_adjusted, liquidity, see ?ethwbtc_trade_history for an example.
#' Negative (tokens bought) and Positive (tokens sold by user to pool) are expected in one of amount0_adjusted or amount1_adjusted.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#' @param denominate Denominate the result of the strategy in Token 0, `0`, or Token 1, `1`. Default 1.
#' @param price2 Default NULL. Optional override to sqrtpx96 to value assets at a market rate instead of pool rate. MUST be in token 1 / token 0 format. see ?sqrtpx96_to_price
#'
#' @return
#' \item{value}{  A single number denominated in Token 0, `denominate = 0`, or Token 1, `denominate = 1` consolidating the position to its value in that denomination
#' using either the provided current price, sqrtpx96, or the override `price2`. }
#' \item{fees}{a list of amount0 and amount1 fees as returned from ?calc_fees_from_trades.}
#' \item{balances}{a list of token0 and token1 balances as returned from ?get_position_balance.}
#' \item{price}{Human readable price in token 1 / token 0 format as returned from ?sqrtpx96_to_price or equal to override `price2`.}
#'
#' @export
#'
#' @examples
#' # Calculate fees from the the 1,000 blocks of trades between 16M to 16,001,000.
#' block_min = 16000000
#' block_max = 16001000
#' trades_16m1000 <- ethwbtc_trade_history[ethwbtc_trade_history$block_number %in% (block_min:block_max), ]
#'
#' # The position value in ETH, combined with the trade fees of the position also denominated in ETH is 2.57798 ETH total
#' calc_strategy_value(position_L = '1429022391989675',
#' sqrtpx96 = '29401036298070293399922101282930688',
#' tick_lower = 256400,
#' tick_upper = 256520,
#' decimal_x = 1e8,
#' decimal_y = 1e18,
#' trades = trades_16m1000,
#' fee = 0.003,
#' denominate = 1,
#' price2 = NULL)

calc_strategy_value <- function(position_L, sqrtpx96,
                                tick_lower, tick_upper,
                                decimal_x = 1e18, decimal_y = 1e18,
                                trades, fee = 0.003, denominate = 1,
                                price2 = NULL){

  if( !(denominate %in% 0:1)){
    stop("Use 0 to denominate strategy in Token 0, or 1 to denominate in Token 1")
  }

  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  fees <- calc_fees_from_trades(position_L = position_L,
                                tick_lower = tick_lower,
                                tick_upper = tick_upper,
                                trades = trades,
                                fee = fee)

  balances <- get_position_balance(position_L = position_L,
                                   sqrtpx96 = sqrtpx96,
                                   tick_lower = tick_lower,
                                   tick_upper = tick_upper,
                                   decimal_x = decimal_x,
                                   decimal_y = decimal_y)

  # by default use provided pool price for assessing the position balance
  # but override available with price2
  # Useful to accurately withdraw from pool but value assets at a different market rate
  if(is.null(price2)){
    P = sqrtpx96_to_price(sqrtpx96, F, decimal_adjustment = decimal_adjustment)
  } else {
    P = price2
  }

  if(denominate == 0){
value <- fees$amount0_fees + (fees$amount1_fees/P) +
      balances$token0 + (balances$token1/P)
  } else {
value <- (fees$amount0_fees*P) + fees$amount1_fees +
      (balances$token0*P) + balances$token1
  }

  total = list(value = value,
               fees = fees,
               balances = balances,
               price = P)

  return(total)

}



