#' Swap Across Ticks
#'
#' This function loops through a trade given the swap amount and available liquidity (including inactive positions) to
#' calculate the amount received, resulting fees paid to each position, and the final price including a history of trades. It recalculates price
#' as needed via ?find_recalculation_price while looping through ?swap_within_tick and ?check_positions to get the final result.
#'
#' @param ptbl Liquidity Positions table of the form tick_lower, tick_upper, liquidity. See ?ethwbtc_net_liquidity for an example.
#' This included dataset is net_liquidity, so it should be filtered to the max blockheight for a given tick_lower-tick_upper.
#' @param sqrtpx96 The Current price of the pool in Uniswap 64.96 square root price. Can be in gmp::as.bigz() or a "string". See ?price_to_sqrtpx96.
#' @param fee_tbl A table of fees accumulated in the trade so far. Default NULL will construct a fresh table as this function calls itself recursively.
#' @param trade_record A table of trade histories within the trade so far. Default NULL will construct a fresh table as this function calls itself recursively.
#' @param dx the human readable amount of token 0 you are trading with the pool (i.e., adjust for decimal using decimal_x not here). NULL if you are instead trading token 1.
#' @param dy NULL if you are trading token 0. Otherwise, the human readable amount of token 1 you are trading with the pool (i.e., adjust for decimal using decimal_y not here).
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param fee The pool fee, default 0.3\% (0.003). Generally one of: 0.0001, 0.0005, 0.003, 0.01
#' @return Swap Across returns a `trade_record` list containing:
#' \item{ptbl}{ Liquidity Positions table of the form tick_lower, tick_upper, liquidity, active (TRUE/FALSE on if position is active at new_price). }
#' \item{new_price}{The `sqrtpx96` after the trade is complete.}
#' \item{dy_in OR dx_in}{ the amount of token 1 (`dy_in`) or token 0 (`dx_in`) added to pool (i.e. sold by user), fees separated.}
#' \item{dy_fee OR dx_fee}{ the amount of token 1 (`dy_fee`) or token 0 (`dx_fee`) taken to pay LPs to pool (add to `dy_in` or `dx_in` to get total sent by user).}
#' \item{dx_out OR dy_out}{ the amount of token 0 (`dx_out`) or token 1 (`dy_out`) taken from pool (i.e. bought by user).}
#' \item{fee_tbl}{Liquidity Positions table of the form tick_lower, tick_upper, liquidity, active and `yfee` or `xfee` distributing `dy_fee` or `dx_fee` across each liquidity position. sum(`fee_tbl[yfee]`) == `dy_fee` }
#' @import gmp
#' @export
#'
#' @examples
#' data(ethwbtc_trade_history)
#' data(ethwbtc_net_liquidity)
#' # replicating a large trade that causes recalculation of liquidity
#' blockheight <- 16119393
#' # remember some of this liquidity will not be active at certain prices so sum(l9393$liquidity) is NOT useful w/o a price (see ?check_positions).
#' l9393 <- liquidity_asof_block(ethwbtc_net_liquidity, blockheight = blockheight)
#' # Taken from Quicknode as of blockheight
#' sqrtpx96 <- gmp::as.bigz("28949841196232757349076196841750528")
#' # rough estimate that is within 0.01\% is to take price from trade data tick.
#' most_recent_trade_tick <- tail(ethwbtc_trade_history[ethwbtc_trade_history$block_number < blockheight, "tick"], 1)
#' sqrtptx96_from_tick <- price_to_sqrtpx96(P = tick_to_price(most_recent_trade_tick, 1e10), invert = FALSE, 1e10)
#' abs(as.numeric(sqrtptx96_from_tick/sqrtpx96)) - 1 < 0.00001 # very close together
#'
#' # This trade at Block 16119393 causes a recalculation of liquidity.
#' #' returns within 0.01% (some error due to both precision and possibly missing data in net liquidity)
#' swp = swap_across_ticks(l9393, sqrtpx96, NULL, NULL, NULL, 1140.00000000000, 1e8, 1e18, 0.003)
#' # Should return -84.98101962 BTC removed from pool
#' swp$dx_out
#' # Another trade this time selling 14.795 BTC at block 16115408
#' blockheight <- 16115408
#' l5408 <- liquidity_asof_block(ethwbtc_net_liquidity, blockheight = blockheight)
#' # Taken from Quicknode but close to the tick after
#' sqrtpx96 = gmp::as.bigz(2.8929142808894924e+34)
#' # rough estimate that is within 0.01\% of tick from previous trade in block 16115368 (256173)
#' sqrtpx96_from_tick <- price_to_sqrtpx96(P = tick_to_price(256173, 1e10, yx = TRUE), invert = FALSE, 1e10)
#' abs(as.numeric(sqrtpx96_from_tick/sqrtpx96)) - 1 < 0.00001 # very close together
#' swp2 = swap_across_ticks(ptbl = l5408, sqrtpx96 = sqrtpx96,
#'                         fee_tbl = NULL,
#'                        trade_record =  NULL,
#'                         dx = 14.79530830,
#'                         dy = NULL, 1e8, 1e18, 0.003)
#' # should be close to real result -196.6075
#' swp2$dy_out # (ETH taken from pool by user)


swap_across_ticks <- function(ptbl, sqrtpx96,
                              fee_tbl = NULL,
                              trade_record = NULL,
                              dx = NULL,
                              dy = NULL,
                              decimal_x = 1e18,
                              decimal_y = 1e18,
                              fee = 0.003){

  # compare decimals to get adjustment
  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  if(is.null(dx) & is.null(dy)){
    stop("A change in x or y is required to use liquidity")
  }

  if(!is.null(dx) & !is.null(dy)){
    stop("Only 1 swap can be done at a time")
  }

  # if dx is null; we're selling dy, which means price is going up!
  # (P = Y/X; more Y is more P)
  if(is.null(dx)){

    amount <- dy
    price <- sqrtpx96_to_price(sqrtpx96 = sqrtpx96, invert = FALSE, decimal_adjustment = decimal_adjustment)
    update_ptbl <- check_positions(ptbl, price, decimal_adjustment = decimal_adjustment, yx = TRUE)


    # record fees separately
    # if it is blank assume this is a fresh swap and make the fee tbl
    if(is.null(fee_tbl)){
      fee_tbl <-  update_ptbl[, c("tick_lower", "tick_upper","liquidity", "active")]
      fee_tbl$yfee <- 0

      # otherwise refresh the active positions but retain any previous fee
    } else {
      yfee = fee_tbl$yfee
      fee_tbl <-  update_ptbl[,c("tick_lower", "tick_upper","liquidity", "active")]
      fee_tbl$yfee <- yfee
    }

    recalc_price <- find_recalculation_price(ptbl = update_ptbl,
                                             P = price,
                                             price_up = TRUE,
                                             decimal_adjustment = decimal_adjustment,
                                             yx = TRUE)

    # sum liquidity in active positions
    # note, liquidity from trade table may differ if there are data issues, but should be close.
    current_L <- sum(gmp::as.bigz(update_ptbl$liquidity[update_ptbl$active]))

    # maximum change without recalc
    max_y <- size_price_change_in_tick(
      L = current_L,
      sqrtpx96 = sqrtpx96,
      sqrtpx96_target = price_to_sqrtpx96(recalc_price, invert = FALSE, decimal_adjustment = decimal_adjustment),
      dx = FALSE, # return dy to sell
      decimal_scale = decimal_y, # scale is 1 token, adjustment is between 2 tokens.
      fee = fee)

    # if you can sell without recalculation; swap within tick
    if(max_y >= amount){

      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(P = price, invert = FALSE, decimal_adjustment = decimal_adjustment),
                              dy = amount,
                              decimal_x = decimal_x,
                              decimal_y = decimal_y,
                              fee = fee)

      # attribute fees to positions
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) /
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )

      # If no previous trade record is provided make a new one and return it
      # this was a swap within a tick, not across them.
      if(is.null(trade_record)){

        fee_tbl$yfee = fee_tbl$yfee + new_fees

        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = swap$dy,
          dy_fee = swap$fee,
          dx_out = swap$dx,
          fee_tbl = fee_tbl
        )
      } else {

        # get the original trade record and original fee_tbl
        tr = trade_record
        ft = trade_record$fee_tbl

        # add previous fees from record to latest fees and active positions
        fee_tbl$yfee = ft$yfee + new_fees

        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = tr$dy_in + swap$dy,
          dy_fee = tr$dy_fee + swap$fee,
          dx_out = tr$dx_out + swap$dx,
          fee_tbl = fee_tbl
        )
      }

      # you're done; return the results.
      return(trade_record)


      # else swap as much as you can and repeat process
    } else {

      leftover = amount - max_y

      # swap max y
      # track leftover
      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(P = price, invert = FALSE, decimal_adjustment = decimal_adjustment),
                              dy = max_y,
                              decimal_x = decimal_x,
                              decimal_y = decimal_y,
                              fee = fee)

      # attribute fees to position
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) /
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )


      # UPDATE past trade record if it exists
      # or make new one

      if(is.null(trade_record)){

        fee_tbl$yfee = fee_tbl$yfee + new_fees

        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = swap$dy,
          dy_fee = swap$fee,
          dx_out = swap$dx,
          fee_tbl = fee_tbl
        )
      } else {
        # get the original trade record and original fee_tbl
        tr = trade_record
        ft = trade_record$fee_tbl

        # add previous fees from record to latest fees and active positions
        fee_tbl$yfee = ft$yfee + new_fees

        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = tr$dy_in + swap$dy,
          dy_fee = tr$dy_fee + swap$fee,
          dx_out = tr$dx_out + swap$dx,
          fee_tbl = fee_tbl
        )
      }

      # call the function again with new information including *adding* trade records
      # until the final trade_record is output
      swap_across_ticks(ptbl = trade_record$ptbl,
                        sqrtpx96 = trade_record$new_price,
                        fee_tbl = trade_record$fee_tbl,
                        trade_record = trade_record,
                        dx = NULL,
                        dy = leftover,
                        decimal_x = decimal_x,
                        decimal_y = decimal_y,
                        fee = fee)


    }
  } else if(is.null(dy)){
    amount <- dx
    price <- sqrtpx96_to_price(sqrtpx96 = sqrtpx96, invert = FALSE, decimal_adjustment = decimal_adjustment)
    update_ptbl <- check_positions(ptbl, price, decimal_adjustment = decimal_adjustment, yx = TRUE)


    # record fees separately
    # if it is blank assume this is a fresh swap and make the fee tbl
    if(is.null(fee_tbl)){
      fee_tbl <-  update_ptbl[, c("tick_lower", "tick_upper","liquidity", "active")]
      fee_tbl$xfee <- 0

      # otherwise refresh the active positions but retain any previous fee
    } else {
      xfee = fee_tbl$xfee
      fee_tbl <-  update_ptbl[,c("tick_lower", "tick_upper","liquidity", "active")]
      fee_tbl$xfee <- xfee
    }

    # else we're selling dx, price is going down
    # (P = Y/X, more X is less P)
    recalc_price <- find_recalculation_price(ptbl = update_ptbl,
                                             P = price,
                                             price_up = FALSE,
                                             decimal_adjustment = decimal_adjustment,
                                             yx = TRUE)

    # sum liquidity in active positions
    current_L <- sum(as.bigz(update_ptbl$liquidity[update_ptbl$active]))

    # maximum change without recalc
    max_x <- size_price_change_in_tick(
      L = current_L,
      sqrtpx96 = sqrtpx96,
      sqrtpx96_target = price_to_sqrtpx96(recalc_price, invert = FALSE, decimal_adjustment = decimal_adjustment),
      dx = TRUE, # return dx to sell
      decimal_scale = decimal_x, # scale is 1 token, adjustment is between 2 tokens.
      fee = fee)

    # if you can sell without recalculation; swap within tick
    if(max_x >= amount){

      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(P = price, invert = FALSE, decimal_adjustment = decimal_adjustment),
                              dx = amount,
                              decimal_x = decimal_x,
                              decimal_y = decimal_y,
                              fee = fee)

      # attribute fees to positions
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) /
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )

      # If no previous trade record is provided make a new one and return it
      # this was a swap within a tick, not across them.
      if(is.null(trade_record)){

        fee_tbl$xfee = fee_tbl$xfee + new_fees

        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = swap$dx,
          dx_fee = swap$fee,
          dy_out = swap$dy,
          fee_tbl = fee_tbl
        )
      } else {

        # get the original trade record and original fee_tbl
        tr = trade_record
        ft = trade_record$fee_tbl

        # add previous fees from record to latest fees and active positions
        fee_tbl$xfee = ft$xfee + new_fees

        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = tr$dx_in + swap$dx,
          dx_fee = tr$dx_fee + swap$fee,
          dy_out = tr$dy_out + swap$dy,
          fee_tbl = fee_tbl
        )
      }

      # you're done; return the results.
      return(trade_record)


      # else swap as much as you can and repeat process
    } else {

      leftover = amount - max_x

      # swap max x
      # track leftover
      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(P = price, invert = FALSE, decimal_adjustment = decimal_adjustment),
                              dx = max_x,
                              decimal_x = decimal_x,
                              decimal_y = decimal_y,
                              fee = fee)

      # attribute fees to position
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) /
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )

      # UPDATE past trade record if it exists
      # or make new one

      if(is.null(trade_record)){

        fee_tbl$xfee = fee_tbl$xfee + new_fees

        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = swap$dx,
          dx_fee = swap$fee,
          dy_out = swap$dy,
          fee_tbl = fee_tbl
        )
      } else {
        # get the original trade record and original fee_tbl
        tr = trade_record
        ft = trade_record$fee_tbl

        # add previous fees from record to latest fees and active positions
        fee_tbl$xfee = ft$xfee + new_fees

        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = tr$dx_in + swap$dx,
          dx_fee = tr$dx_fee + swap$fee,
          dy_out = tr$dy_out + swap$dy,
          fee_tbl = fee_tbl
        )
      }

      # call the function again with new information including *adding* trade records
      # until the final trade_record is output
      swap_across_ticks(ptbl = trade_record$ptbl,
                        sqrtpx96 = trade_record$new_price,
                        fee_tbl = trade_record$fee_tbl,
                        trade_record = trade_record,
                        dx = leftover,
                        dy = NULL,
                        decimal_x = decimal_x,
                        decimal_y = decimal_y,
                        fee = fee)

    }

  }
}
