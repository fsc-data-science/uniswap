#' Calculate Fees from Trade
#'
#' Given a position's marginal liquidity and range (tick_lower, tick_upper) calculate the fee rewards from trades occurring in set of blocks.
#'
#' @param position_L The marginal liquidity provided by a position. This is usable on `ethwbtc_net_liquidity` but it would be the balance of all positions in a range.
#' @param tick_lower The low tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param tick_upper The upper tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param trades Trades table with columns tick, amount0_adjusted, amount1_adjusted, liquidity, see ?ethwbtc_trade_history for an example.
#' Negative (tokens bought) and Positive (tokens sold by user to pool) are expected in one of amount0_adjusted or amount1_adjusted.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#'
#' @return a list of `amount0_fees` denominated in x units (e.g., WBTC) and `amount1_fees` denominated in y units (e.g., WETH).
#' @export
#' @import gmp
#'
#' @examples
#' # Calculate fees from the the 1,000 blocks of trades between 16M to 16,001,000.
#' block_min = 16000000
#' block_max = 16001000
#' trades_16m1000 <- ethwbtc_trade_history[ethwbtc_trade_history$block_number %in% (block_min:block_max), ]
#' # see ?get_liquidity
#' # This liquidity earns 0.0002311 BTC and 0.00926 WETH
#' calc_fees_from_trades(position_L = '1429022391989675',
#' tick_lower = 256400,
#' tick_upper = 256520,
#' trades = trades_16m1000,
#' fee = 0.003)

calc_fees_from_trades <- function(position_L, tick_lower, tick_upper, trades, fee = 0.003){

  position_L = gmp::as.bigz(position_L)
  relevant_trades <- trades[trades$tick >= tick_lower & trades$tick <= tick_upper, ]
  liquidity_fraction <- position_L/(relevant_trades$liquidity + position_L)
  amount0 <- sum(relevant_trades[relevant_trades$amount0_adjusted > 0, ]$amount0_adjusted * liquidity_fraction)*fee
  amount1 <- sum(relevant_trades[relevant_trades$amount1_adjusted > 0, ]$amount1_adjusted * liquidity_fraction)*fee

  return(
    list(
      amount0_fees = as.numeric(amount0),
      amount1_fees = as.numeric(amount1)
    )
  )
}



