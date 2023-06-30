get_position_balance <- function(L, P, ){

  liquidity_in_range = (L * (P * t_upper)) / (t_upper - t_lower)
  token0_balance = liquidity_in_range * (t_upper - P) / (t_upper - t_lower)
  token1_balance = liquidity_in_range * (P - t_lower) / (t_upper - t_lower)

  return token0_balance, token1_balance


}
