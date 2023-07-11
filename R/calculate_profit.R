#' Calculate Profit
#'
#' This is a wrapper function to ?calc_strategy_value that tailored for optimization.
#' It can create a hypothetical position and calculate its marginal liquidity to assess fees from trades
#' using price, amount1, and budget (in amount1) to calculate amount0 and tick_lower combined with ?price_all_tokens to
#' get the tick_upper. This enables 2-parameter optimization to get all relevant details of a position. Note,
#' the optimal results may not fit all pools and tick spacings. Use ?get_closest_tick as needed to get near-optimal allowed
#' ranges.
#'
#' @param params array of (amount of Token 1, price_lower). Using price instead of tick improves optimization.
#'  Use ?tick_to_price to convert accordingly.
#' @param budget max amount of Token 1 possible.
#' @param p1 Exchange rate for Token 1 / Token 0 for calculating amount0 and liquidity at the beginning of a position.
#' @param p2 The market price or final price for assessing the strategy value of a position after trades.
#' @param trades Trades table with columns tick, amount0_adjusted, amount1_adjusted, liquidity, see ?ethwbtc_trade_history for an example.
#' Negative (tokens bought) and Positive (tokens sold by user to pool) are expected in one of amount0_adjusted or amount1_adjusted.
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#' @param denominate Denominate the result of the strategy in Token 0, `0`, or Token 1, `1`. Default 1.
#' @param in_optim Default FALSE returns all data from calculating a position's value. TRUE returns only -1*sv$value for iterating through optim() optimization.
#'
#' @return A list of all available information about a position after select trades, or -sv$value if in_optim = TRUE.
#' @export
#'
#' @examples
#' data("ethwbtc_trade_history")
#'block_min = 16000000
#'block_max = 16010000
#'trades_16m10k <- ethwbtc_trade_history[ethwbtc_trade_history$block_number %in% (block_min:block_max), ]
#'
#'
#' # Given initial & final prices
#' # as the first and last prices in the relevant swaps
#' p1 = tick_to_price(tick = head(trades_16m10k$tick, n = 1), decimal_adjustment = 1e10)
#' p2 = tick_to_price(tick = tail(trades_16m10k$tick, n = 1), decimal_adjustment = 1e10)
#'
#' # calculate the profit of a position with 91.17891 ETH, (100 ETH budget)
#' # and a low price of 13.52 ETH/BTC
#' # Returns above budget, 101.653 ETH value
#' # but note the tick_lower and tick_upper do not align to allowable tick spacing except for 0.01\% pools
#' calculate_profit(params = c(91.17891, 13.52415),
#'                  budget = 100, p1 = p1, p2 = p2, trades = trades_16m10k,
#'                  decimal_x = 1e8, decimal_y = 1e18, fee = 0.003, denominate = 1, in_optim = FALSE)
#' # use round(initial_tick / tick_spacing)*tick_spacing to fix unallowed ticks

calculate_profit <- function(params, budget = 100, p1, p2, trades,
                             decimal_x = 1e18, decimal_y = 1e18,
                             fee = 0.003, denominate = 1, in_optim = FALSE){

  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  a1 = params[1]
  tick_lower = get_closest_tick(desired_price = params[2], 1, decimal_adjustment = decimal_adjustment)$tick

  a0 = (budget - a1)/p1

  sqrtpx96_1 <- price_to_sqrtpx96(p1, decimal_adjustment = decimal_adjustment)
  sqrtpx96_2 <- price_to_sqrtpx96(p2, decimal_adjustment = decimal_adjustment)

  tick_upper = price_all_tokens(x = a0, y = a1,
                                sqrtpx96 = sqrtpx96_1,
                                decimal_x = decimal_x,
                                decimal_y = decimal_y,
                                tick_lower = tick_lower,
                                tick_upper = NULL)$tick_upper

  if(tick_upper < tick_lower & in_optim){
    # large positive number to be ignored in optim
    return(1e6)
  } else if (tick_upper < tick_lower & !in_optim){
    warning("tick_upper < tick_lower, switching ticks.")
    temp <- tick_lower
    tick_lower <- tick_upper
    tick_upper <- temp
  }

  L = get_liquidity(x = a0, y = a1,
                    sqrtpx96 = sqrtpx96_1,
                    decimal_x = decimal_x,
                    decimal_y = decimal_y,
                    tick_lower = tick_lower,
                    tick_upper = tick_upper)

  sv <- calc_strategy_value(position_L = L,
                            sqrtpx96 = sqrtpx96_2,
                            tick_lower = tick_lower,
                            tick_upper = tick_upper,
                            decimal_x = decimal_x,
                            decimal_y = decimal_y,
                            trades = trades,
                            fee = fee,
                            denominate = denominate)

  # calc_strategy_value uses get_position_balance which comes from liquidity
  # if liquidity is from a single token (i.e., position starts out of range and never enters)
  # then a value of 0 is assigned for one of the tokens
  # because calculate_profit has both a0 and a1
  # this can be fixed by replacing the 0 with the position details

  # only do this out of optimization
  # and even then this should be double checked
  if(denominate == 1 &
     sv$balances$token0 == 0 &
     sv$value == sv$balances$token1){
    sv$balances$token0 <- a0
    sv$value <- sv$value + (a0*p2)
  } else if(denominate == 0 &
            sv$balances$token1 == 0 &
     sv$value == sv$balances$token1){
    sv$balances$token1 <- a1
    sv$value <- sv$value + (a1/p2)
  }

  if(in_optim){
    # * -1 because optim default is mininimization
    return(-sv$value)
  } else {
    # out of optimization, return position details
    return(
      list(
        position = list(x = a0, y = a1, tick_lower = tick_lower, tick_upper = tick_upper, liquidity = L),
        strategy_value = sv
      )
    )
  }
}
