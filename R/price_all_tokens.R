#' Price All Tokens
#'
#' Given an amount of tokens, a current price, and either the min or max price of a range; identify the corresponding
#' max or min price for the Uni v3 position to use all tokens x and y. NOTE: not all pools allow all prices, see:
#' ?get_closest_tick for more info.
#'
#' @param x number of token 0, e.g., WBTC in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param y number of token 1, e.g., ETH in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param sqrtpx96 current price in uint160 format.See ?price_to_sqrtpx96 or read a pool contract's sqrtPriceX96 within it's slot0 on etherscan to get this value.
#' @param decimal_x The decimals used in token 0, e.g., 1e6 for USDC, 1e8 for WBTC.
#' @param decimal_y The decimals used in token 1, e.g., 1e18 for WETH.
#' @param tick_lower The low tick in a liquidity position, see ?get_closest_tick to convert a price to a tick. Otherwise NULL if tick_upper is provided.
#' @param tick_upper Default NULL. Otherwise the upper tick in a liquidity position, see ?get_closest_tick to convert a price to a tick.
#'
#' @return A list of `amount_x`, `amount_y`, `sqrtpx96`, `P`, `tick_lower`, `tick_upper`, `price_lower`, `price_upper`
#' where tick_lower or tick_upper is replaced with its matched value and their corollary human readable prices are in Token 1 / Token 0 (y/x) format.
#' @export
#'
#' @examples
#' # Matches ?match_tokens_to_range example for ETH/WBTC pool.
#' price_all_tokens(x = 1,
#'                  y = 16.11781,
#'                 sqrtpx96 = '32211102662183904786754519772954624',
#'                 decimal_x = 1e8,
#'                 decimal_y = 1e18,
#'                 tick_lower = NULL,
#'                 tick_upper = 258900)$tick_lower == 257760

price_all_tokens <- function(x, y, sqrtpx96, decimal_x = 1e18, decimal_y = 1e18, tick_lower, tick_upper){

  if(is.null(x) | is.null(y)){
    stop("both of amount of token x and amount of token y must be provided")
  }

  if(!is.null(tick_lower) & !is.null(tick_upper)){
    stop("one of tick_lower or tick_upper should be unknown, NULL")
  }

  decimal_adjustment <- max( c(decimal_y/decimal_x, decimal_x/decimal_y) )
  P = sqrtpx96_to_price(sqrtpx96, decimal_adjustment = decimal_adjustment)

  r <- list(
    amount_x = x,
    amount_y = y,
    sqrtpx96 = gmp::as.bigz(sqrtpx96),
    P = P,
    tick_lower = NULL,
    tick_upper = NULL,
    price_lower = NULL,
    price_upper = NULL
  )

  if(!is.null(tick_lower)){
    r[["tick_lower"]] <- tick_lower
    price_lower <- tick_to_price(tick = tick_lower, decimal_adjustment)
    r[["price_lower"]] <- price_lower
  } else {
    r[["tick_upper"]] <- tick_upper
    price_upper <- tick_to_price(tick = tick_upper, decimal_adjustment)
    r[["price_upper"]] <- price_upper
  }

  # if tick_lower is given
  if(!is.null(tick_lower)){
    f1 <- (y^2)/(x^2)
    f2 <- sqrt(price_lower) - sqrt(P) + (y/(sqrt(P)*x))
    pb <- f1 * (f2)^-2

    r[["price_upper"]] <- pb
    r[["tick_upper"]] <- get_closest_tick(pb, 1, decimal_adjustment)$tick

  }

  # if tick_lower is NOT given
  if(is.null(tick_lower)){
    f1 <- y / (sqrt(price_upper) * x)
    f2 <- y / (sqrt(P) * x)
    pa <- (f1 + sqrt(P) - f2)^2

    r[["price_lower"]] <- pa
    r[["tick_lower"]] <- get_closest_tick(pa, 1, decimal_adjustment)$tick
  }

  return(r)
}
