#' Performs transformation of continuous variables for the heatmap color scales.
#'
#' @param x Numeric vector.
#' @param trans_type Character string of 'scale' or 'normalize'.
#' If 'scale', subtract the mean and divide by the standard deviation.
#' If 'normalize', i.e., max-min normalize, subtract the min and divide by the max.
#' More information on what transformation to choose can be acquired here:
#' https://cran.rstudio.com/web/packages/heatmaply/vignettes/heatmaply.html#scale
#'
#' @return Numeric vector of transformed x.
#' @export
#'
#' @examples
scale_stand <- function(x, trans_type){
  if (trans_type == 'scale') {
    scale(x)
  } else if (trans_type == 'normalize'){
    x <- x - min(x, na.rm = T)
    x <- x/max(x, na.rm = T)
    x
  } else {
    x
  }
}
