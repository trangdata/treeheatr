# percentize() functions adapted from the heatmaply package to avoid plotly dependency.
# https://github.com/talgalili/heatmaply/blob/master/R/percentize.R
#
# kCDF_fun, ecdf
percentize_predict <- function(x, ecdf_fun = stats::ecdf, ...) {
  # http://stackoverflow.com/questions/25130531/how-to-select-only-numeric-columns-from-a-data-table
  # x must be a data.frame
  ss_numeric <- sapply(x, is.numeric)

  # ecdf_fun <- kCDF_fun # ecdf

  ecdf_list <- list()
  for (i in 1:ncol(x)) {
    ecdf_list[[i]] <- if (ss_numeric[i]) {
      ecdf_fun(stats::na.omit(x[, i]))
    } else {
      NA
    }
  }

  fun <- function(new_x) {
    if (any(colnames(new_x) != colnames(x)))
      stop('The column names (or order) of the new x are different than that of the old x. Please fix and try again.')

    for (i in 1:ncol(x)) {
      ecdf_fun <- ecdf_list[[i]]
      if (!(is.vector(ecdf_fun) && is.na(ecdf_fun))) {
        ss_no_NA <- !is.na(new_x[, i])
        new_x[ss_no_NA, i] <- ecdf_fun(new_x[ss_no_NA, i])
      }
    }

    return(new_x)
  }

  class(fun) <- c('function', 'percentize_predict')

  return(fun)
}

percentize <- function(x, ...) {
  UseMethod('percentize')
}

percentize.default <- function(x, ...) {
  ss_no_NA <- !is.na(x)
  x[ss_no_NA] <- stats::ecdf(x[ss_no_NA])(x[ss_no_NA])
  x
}

percentize.data.frame <- function(x, ...) {
  percentize_predict(x)(x)
}

percentize.matrix <- function(x, ...) {
  x <- as.data.frame(x)
  percentize(x)
}
