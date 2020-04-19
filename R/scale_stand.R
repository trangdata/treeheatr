scale_stand <- function(x, trans_type){
  # This function performs transformation of continuous variables.
  # 
  # either scale (i.e., subtract the mean and divide by the standard deviation) 
  # or max-min normalize (i.e., subtract the min and divide by the max)
  # only for the heatmap color scales.
  # 
  # More information on what transformation to choose can be acquired here:
  # https://cran.rstudio.com/web/packages/heatmaply/vignettes/heatmaply.html#scale
  
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
