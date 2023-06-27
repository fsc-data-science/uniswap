#' Find Recalculation Price
#'
#' @param positions tbd
#' @param current_price tbd
#' @param price_up tbd
#'
#' @return tbd
#' @export
#'
#' @examples
#' "tbd"
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
