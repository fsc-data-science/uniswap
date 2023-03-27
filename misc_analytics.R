get_liquidity(
  x = 424.5*1e18,
  y = 0.07447*1e18,
  P = 1492.57,
  pa = 1330.94,
  pb = 1537.07,
  yx = FALSE
)
# $645.51 -> Liquidity Points:
197281913793055862390


# $4,065.11
get_liquidity(
  x = 3374*1e18,
  y = 0*1e18,
  P = 1492.57,
  pa = 1413.2359,
  pb = 1430.2960,
  yx = FALSE
)
14914393655935245885754


# $645.51
a_amt <- 645.51
a_liq <- as.bigz('197281913793055862390')
# $4,065.11
c_amt <- 4065.11
c_liq <- as.bigz('14914393655935245885754')


data.frame(
  amt_ratio = c_amt/a_amt,
  c_liqs_per_dollar = as.numeric(c_liq/c_amt)/1e18,
  a_liqs_per_dollar = as.numeric(a_liq/a_amt)/1e18
)
