#' Check Positions
#'
#' @param ptbl tbd
#' @param P tbd
#'
#' @return tbd
#' @export
#'
#' @examples
#' "tbd"
check_positions <- function(ptbl, P){
  if( !("min_price" %in% colnames(ptbl)) | !("max_price" %in% colnames(ptbl))){
    stop("Cannot find min_price and max_price columns.")
  }

  ptbl <- ptbl$active = (ptbl$P > ptbl$min_price & ptbl$P < ptbl$max_price)
  return(ptbl)
}
