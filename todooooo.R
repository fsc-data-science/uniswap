# Data for package


find_recalculation_price <- function(positions, current_price, price_up = TRUE){

  p <- positions[, c("min_price","max_price")]

  # if price going up, get the closest available price above current price
  if(price_up == TRUE){
    closest_price <- p[p > current_price][which.min(p[p > current_price])]
  } else {
    closest_price <- p[p < current_price][which.max(p[p < current_price])]
  }

  return(closest_price)
}


check_positions <- function(ptbl, P){
  if( !("min_price" %in% colnames(ptbl)) | !("max_price" %in% colnames(ptbl))){
    stop("Cannot find min_price and max_price columns.")
  }

  ptbl <- ptbl$active = (ptbl$P > ptbl$min_price & ptbl$P < ptbl$max_price)
  return(ptbl)
}

swap_across_ticks <- function(ptbl, sqrtpx96,
                              fee_tbl = NULL,
                              trade_record = NULL,
                              dx = NULL,
                              dy = NULL,
                              decimal_x = 1e18,
                              decimal_y = 1e18,
                              fee = 0.003){

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
    price <- sqrtpx96_to_price(sqrtpX96 = sqrtpx96)
    update_ptbl <- check_positions(ptbl, price)


    # record fees separately
    # if it is blank assume this is a fresh swap and make the fee tbl
    if(is.null(fee_tbl)){
      fee_tbl <-  update_ptbl[, c("position","liquidity", "active")]
      fee_tbl$yfee <- 0

      # otherwise refresh the active positions but retain any previous fee
    } else {
      yfee = fee_tbl$yfee
      fee_tbl <-  update_ptbl[,c("position","liquidity", "active")]
      fee_tbl$yfee <- yfee
    }

    recalc_price <- find_recalculation_price(positions = update_ptbl,
                                             current_price = price,
                                             price_up = TRUE)

    # sum liquidity in active positions
    current_L <- sum(as.bigz(update_ptbl$liquidity[update_ptbl$active]))

    # maximum change without recalc
    max_y <- size_price_change_in_tick(
      L = current_L,
      sqrtpx96 = sqrtpx96,
      sqrtpx96_target = price_to_sqrtpx96(recalc_price),
      dx = FALSE, # return dy to sell
      decimal_adjustment = decimal_y,
      fee = fee)

    # if you can sell without recalculation; swap within tick
    if(max_y >= amount){

      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(price),
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
                              sqrtpx96 = price_to_sqrtpx96(price),
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
    price <- sqrtpx96_to_price(sqrtpX96 = sqrtpx96)
    update_ptbl <- check_positions(ptbl, price)


    # record fees separately
    # if it is blank assume this is a fresh swap and make the fee tbl
    if(is.null(fee_tbl)){
      fee_tbl <-  update_ptbl[, c("position","liquidity", "active")]
      fee_tbl$xfee <- 0

      # otherwise refresh the active positions but retain any previous fee
    } else {
      xfee = fee_tbl$xfee
      fee_tbl <-  update_ptbl[,c("position","liquidity", "active")]
      fee_tbl$xfee <- xfee
    }

    # else we're selling dx, price is going down
    # (P = Y/X, more X is less P)
    recalc_price <- find_recalculation_price(positions = update_ptbl,
                                             current_price = price,
                                             price_up = FALSE)

    # sum liquidity in active positions
    current_L <- sum(as.bigz(update_ptbl$liquidity[update_ptbl$active]))

    # maximum change without recalc
    max_x <- size_price_change_in_tick(
      L = current_L,
      sqrtpx96 = sqrtpx96,
      sqrtpx96_target = price_to_sqrtpx96(recalc_price),
      dx = TRUE, # return dx to sell
      decimal_adjustment = decimal_y,
      fee = fee)

    # if you can sell without recalculation; swap within tick
    if(max_x >= amount){

      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(price),
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
                              sqrtpx96 = price_to_sqrtpx96(price),
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
