#' Match Tokens to Range
#'
#' Given current price, a price range, and an amount of 1 token; identify how much of the other token is required create
#' the Uni v3 position.
#'
#' @param x number of token 0, e.g., WBTC in ETH-WBTC 0.3 percent pool on ETH Mainnet. Should be NULL if y is provided.
#' @param y NULL if x is provided. Otherwise, number of token 1, e.g., ETH in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param tick_lower The low tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#' @param tick_upper The upper tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#'
#' @return A list of
#' @export
#'
#' @examples
#' # See: https://etherscan.io/tx/0xc10cddba3df56e6fba4f9a88b132cc9d4440ff31bb0c4926dc9d9ca652faf376#eventlog
#' # In Block 12,376,757: 1 BTC and 16.117809469 ETH were added to pool with a range of 257760 to 258900
#' # the price at the time was 16.52921 ETH / BTC (sqrtpx96 = 32211102662183904786754519772954624)
#' # Let's match 1 BTC, the price, and the range to the ETH deposited (16.117809469)
#'
match_tokens_to_range <- function(x, y, sqrtpx96, decimal_x = 1e18, decimal_y = 1e18, tick_lower, tick_upper){

  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )

  if(is.null(x) & is.null(y)){
    stop("amount of token x OR amount of token y must be provided")
  }

  if(!is.null(x) & !is.null(y)){
    stop("one of amount x or amount y should be unknown, NULL")
  }

  r <- list(
    amount_x = NULL,
    amount_y = NULL,
    sqrtpx96 = sqrtpx96,
      P = sqrtpx96_to_price(sqrtpx96, decimal_adjustment = decimal_adjustment),
    tick_lower = tick_lower,
    tick_upper = tick_upper,
    price_lower = get_closest_tick(tick_lower, 1, decimal_adjustment),
    price_upper = get_closest_tick(tick_upper, 1, decimal_adjustment)
  )

  if(!is.null(y)){
    r$amount_y <- y
  } else if (!is.null(x)){
    r$amount_x <- x
  }

  # if x is provided and prices are in X/Y format
  if(!is.null(x) & yx == FALSE){

    Lx = x * sqrt(P * pb) / ( sqrt(pb) - sqrt(P) )
    y_ = Lx * ( sqrt(P) - sqrt(pa) )
    r$amount_y = y_^-1

  }

  # if y is provided and prices are in X/Y format
  if(!is.null(y) & yx == FALSE){
    Ly = y * sqrt(P * pb) / ( sqrt(pb) - sqrt(P) )
    x_ = Ly * ( sqrt(P) - sqrt(pa) )

    r$amount_x <- x_
  }


  # if x is provided and prices are in Y/X format
  # use swap, inverse, and recursion
  if(!is.null(x) & yx == TRUE){
    r$amount_y <- match_tokens_to_range(x = x,
                                        P = P^-1,
                                        pa = pb^-1,
                                        pb = pa^-1, yx = FALSE)$amount_y
  }

  # if y is provided and prices are in Y/X format
  # use swap, inverse, and recursion
  if(!is.null(y) & yx == TRUE){
    r$amount_x <- match_tokens_to_range(y = y,
                                        P = P^-1,
                                        pa = pb^-1,
                                        pb = pa^-1, yx = FALSE)$amount_x
  }

  return(r)

}
