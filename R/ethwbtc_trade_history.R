
#' ETH-WBTC 0.3\% Trade History
#'
#' Trade History for the Ethereum mainnet ETH-WBTC 0.3\% Uniswap v3 Liquidity Pool
#' from inception to block 17,000,000. See overview.Rmd vignette for exact Flipside Crypto query.
#'
#' @format A data frame with 161,984 rows and 7 variables:
#' \describe{
#'   \item{block_number}{Ethereum mainnet block number}
#'   \item{liquidity_adjusted}{The total liquidity available (scaled down by 1e13) that includes the tick of the trade. Changes from liquidity withdrawal, deposits, and when price exists a position(s) range}
#'   \item{tick}{The price range where the trade is occuring. Within a tick multiple (very close together) swap prices will occur until the price moves to another tick. Ticks are 0.01\% from each other.}
#'   \item{amount0_adjusted}{Decimal adjusted change in amount of Token 0 in a pool. Positive value means a user sells (the pool gains the amount)}
#'   \item{token0_symbol}{The symbol of Token 0, kept for easy reference, always WBTC in this data. Recall Uniswap slot0 pool prices are Token 1 / Token 0}
#'   \item{amount1_adjusted}{Decimal adjusted change in amount of Token 1 in a pool. Negative value means a user buys (the pool loses the amount)}
#'   \item{token1_symbol}{The symbol of Token 1, kept for easy reference, always WETH in this data. Recall Uniswap slot0 pool prices are Token 1 / Token 0}
#'   \item{liquidity}{The raw liquidity available (liquidity_adjusted * 1e13) required for accurate swap sizing.}
#' }
#' @source \href{https://flipsidecrypto.xyz/}{Flipside Crypto}
"ethwbtc_trade_history"
