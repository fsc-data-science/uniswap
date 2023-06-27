#' Match Tokens to Range
#'
#' Given current price, a price range, and an amount of 1 token; identify how much of the other token is required create
#' the Uni v3 position.
#'
#' @param x number of token 0, e.g., WBTC in ETH-WBTC 0.3 percent pool on ETH Mainnet. Must be NULL if y is not NULL.
#' @param y number of token 1, e.g., ETH in ETH-WBTC 0.3 percent pool on ETH Mainnet. Must be NULL if x is not NULL.
#' @param P Current (human readable) price of the pool, see ?sqrtpx96_to_price if using slot0 from a pool contract.
#' @param pa The minimum price in position.
#' @param pb The maximum price in position.
#' @param yx Whether current price P is in Token 1 / Token 0 (y/x) format already. Default TRUE.
#'
#' @return A list of amount_x, amount_y (i.e., filling in the NULL), current_price P, min_price pa, max_price pb.
#' @export
#'
#' @examples
#' match_tokens_to_range(x = NULL, # how many wrapped bitcoin, WBTC
#' y = 12.549, # to match to 12.549 ETH
#' P = 0.1, # given the current price is 0.1 BTC / ETH
#' pa = 0.05, # minimum price is 0.05 BTC / ETH (ETH is weaker than current)
#' pb = 0.25, # maximum price is 0.25 BTC / ETH (ETH is stronger than current)
#' yx = FALSE) # the unit of account used: BTC/ETH is NOT the pool's Token 1 / Token 0 price.
#'Function will handle all inversions.
#'
#' match_tokens_to_range(x = NULL, # how many wrapped bitcoin WBTC
#' y = 12.549, # to match to 12.549 ETH
#' P = 10, # given the current price is 10 ETH/BTC
#' pa = 4, # minimum price is 4 ETH / BTC (BTC is weaker than current), note: this is 0.25^-1
#' pb = 20, # maximum price is 20 ETH / BTC (BTC is stronger than current), note: this is 0.05^-1
#' yx = TRUE) # ETH/BTC IS Token 1 / Token 0 (y/x) price.
match_tokens_to_range <- function(x = NULL, y = NULL, P, pa, pb, yx = TRUE){

  if(is.null(x) & is.null(y)){
    stop("amount of token x OR amount of token y must be provided")
  }

  if(!is.null(x) & !is.null(y)){
    stop("one of amount x or amount y should be unknown, NULL")
  }

  r <- list(
    amount_x = NULL,
    amount_y = NULL,
    current_price = P,
    min_price = pa,
    max_price = pb
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
