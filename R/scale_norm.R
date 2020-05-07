#' Performs transformation of continuous variables for the heatmap color scales.
#'
#' @param x Numeric vector.
#' @param trans_type Character string of 'normalize', 'scale' or 'none'.
#' If 'scale', subtract the mean and divide by the standard deviation.
#' If 'normalize', i.e., max-min normalize, subtract the min and divide by the max.
#' If 'none', no transformation is applied.
#' More information on what transformation to choose can be acquired here:
#' https://cran.rstudio.com/web/packages/heatmaply/vignettes/heatmaply.html#scale
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
