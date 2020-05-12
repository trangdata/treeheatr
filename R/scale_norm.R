#' Performs transformation on continuous variables.
#'
#' Performs transformation on continuous variables for the heatmap color scales.
#'
#' @param x Numeric vector.
#' @inheritParams heat_tree
#'
#' @return Numeric vector of the transformed `x`.
#' @export
#'
scale_norm <- function(x, trans_type = c('normalize', 'scale', 'none')){
  trans_type <- match.arg(trans_type)

  switch(
    trans_type,
    scale = scale(x),
    normalize = my_norm(x),
    none = x
  )
}

my_norm <- function(x){
  x <- x - min(x, na.rm = T)
  x <- x/max(x, na.rm = T)
  x
}
