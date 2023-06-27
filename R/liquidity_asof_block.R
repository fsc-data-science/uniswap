#' Liquidity as of Block
#'
#' Filters a net liquidity positions table (see ?ethwbtc_net_liquidity for an example) to liquidity across ranges as of a block
#' by partitioning tick_lower-tick_upper and selecting the maximum block <= blockheight.
#'
#' @param ptbl Liquidity Positions table of the form tick_lower, tick_upper, liquidity. See ?ethwbtc_net_liquidity for an example.
#' This included dataset is net_liquidity, this function filters the data to a blockheight for a given tick_lower-tick_upper.
#' @param blockheight The block number to partition and filter to.
#' @param clean remove erroneous liquidity (0 or occasionally negative in case of data issue with a pool).
#'
#' @return A subset of `ptbl` with 1 row per tick_lower-tick_upper.
#' @import dplyr
#' @export
#'
#' @examples
#' data(ethwbtc_net_liquidity)
#' liquidity_asof_block(ethwbtc_net_liquidity, 16000000)
#' max(table(ethwbtc_net_liquidity$tick_lower, ethwbtc_net_liquidity$tick_upper)) == 1
liquidity_asof_block <- function(ptbl, blockheight, clean = TRUE){
  ptbl <- ptbl[ptbl$block_number <= blockheight, ]
  max_block <- aggregate(block_number ~ tick_lower + tick_upper, ptbl, max)
  r <- merge(ptbl, max_block)

  if(clean){
    return(r[r$net_adjusted_liquidity > 0, ])
  } else {
    return(r)
  }
}
