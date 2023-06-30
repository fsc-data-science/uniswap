#' Price All Tokens
#'
#' Given an amount of tokens, a current price, and either the min or max price of a range; identify the corresponding
#' max or min price for the Uni v3 position to use all tokens x and y. NOTE: not all pools allow all prices, see:
#' ?get_closest_tick for more info.
#'
#' @param x number of token 0, e.g., WBTC in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param y number of token 1, e.g., ETH in ETH-WBTC 0.3 percent pool on ETH Mainnet.
#' @param P Current (human readable) price of the pool, see ?sqrtpx96_to_price if using slot0 from a pool contract.
#' @param pa The minimum price in position, Must be NULL if pb is not NULL.
#' @param pb The maximum price in position. Must be NULL if pa is not NULL.
#' @param yx Whether current price P is in Token 1 / Token 0 (y/x) format already. Default TRUE.
#'
#' @return A list of amount_x, amount_y, current_price P, min_price pa, max_price pb (i.e., filling in the NULL).
#' @export
#'
#' @examples
#' price_all_tokens(x = 100, # given 100 BTC
#' y = 1000, # and 1000 ETH
#' P = 0.1, # and a current price of 0.1 BTC/ETH
#'  pa = NULL, # what minimum BTC/ETH price
#'  pb = 0.25, # matches 0.25 BTC/ETH max price
#'  yx = FALSE) # the unit of account used: BTC/ETH is NOT the pool's Token 1 / Token 0 price.
#'  # Function will handle all inversions.
#'
#' price_all_tokens(x = 100, # given 100 BTC
#' y = 1000, # 1000 ETH
#' P = 10, # a current price of 10 ETH/BTC
#'  pa = 4, # a minimum price of 4 ETH/BTC, note: this is 0.25^-1
#'  pb = NULL, # what maximum ETH/BTC price uses all tokens.
#'  yx = TRUE) # the unit of account used: ETH/BTC IS the pool's Token 1 / Token 0 price.
price_all_tokens <- function(x, y, P, pa = NULL, pb = NULL, yx = TRUE){

  if(is.null(pa) & is.null(pb)){
    stop("min price pa OR max price pb must be provided")
  }

  if(!is.null(pa) & !is.null(pb)){
    stop("one of min price or max price should be unknown, NULL")
  }

  r <- list(
    amount_x = x,
    amount_y = y,
    current_price = P,
    min_price = NULL,
    max_price = NULL
  )

  if(!is.null(pa)){
    r$min_price <- pa
  } else {
    r$max_price <- pb
  }

  # if min_price pa is given and prices are in Y/X format
  if(!is.null(pa) & yx == TRUE){

    f1 <- (y^2)/(x^2)
    f2 <- sqrt(pa) - sqrt(P) + (y/(sqrt(P)*x))
    pb <- f1 * (f2)^-2

    r$max_price <- pb

  }

  # if min_price pa is NOT given and prices are in Y/X format
  if(is.null(pa) & yx == TRUE){
    f1 <- y / (sqrt(pb) * x)
    f2 <- y / (sqrt(P) * x)
    pa <- (f1 + sqrt(P) - f2)^2
    r$min_price <- pa
  }

  # if min_price pa is given and prices are in X/Y format
  # use inverse and recursion
  if(!is.null(pa) & yx == FALSE){
    r$max_price <- price_all_tokens(x = x, y = y,
                                    P = P^-1, pb = pa^-1, yx = TRUE)$min_price^-1
  }

  # if min_price is NOT given and prices are in X/Y format
  # use inverse and recursion
  if(is.null(pa) & yx == FALSE){
    r$min_price <- price_all_tokens(x = x, y = y,
                                    P = P^-1, pa = pb^-1, yx = TRUE)$max_price^-1
  }

  return(r)
}
