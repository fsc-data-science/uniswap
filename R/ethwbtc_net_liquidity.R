#' ETH-WBTC 0.3\% Net Liquidity
#'
#' Net Liquidity between ticks for the Ethereum mainnet ETH-WBTC 0.3\% Uniswap v3 Liquidity Pool
#' as of block height from inception to block 17,000,000. See overview.Rmd vignette for exact Flipside Crypto query.
#'
#' @format A data frame with 37,478 rows and 4 variables:
#' \describe{
#'   \item{block_number}{Ethereum mainnet block number}
#'   \item{tick_lower}{The lower price range for a Uniswap v3 position(s)}
#'   \item{tick_upper}{The higher price range for a Uniswap v3 position(s)}
#'   \item{net_adjusted_liquidity}{The total liquidity available between the ticks as of the block_number (i.e. cumulative of deposits and withdraws across all positions with the same tick_lower-tick_upper)}
#' }
#' @source \href{https://flipsidecrypto.xyz/}{Flipside Crypto}
"ethwbtc_net_liquidity"
