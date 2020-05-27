#' Performs transformation on continuous variables.
#'
#' Performs transformation on continuous variables for the heatmap color scales.
#'
#' @param x Numeric vector.
#' @inheritParams draw_heat
#' @importFrom heatmaply percentize
#' @return Numeric vector of the transformed `x`.
#' @export
#' @example scale_norm(1:5)
#' scale_norm(1:5, 'percentize')
#'
scale_norm <- function(x, trans_type = c('percentize', 'normalize', 'scale', 'none')){
  trans_type <- match.arg(trans_type)

  switch(
    trans_type,
    percentize = percentize(x),
    scale = as.numeric(scale(x)),
    normalize = my_norm(x),
    none = x
  )
}

my_norm <- function(x){
  x <- x - min(x, na.rm = T)
  x <- x/max(x, na.rm = T)
  x
}
