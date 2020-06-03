`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.simplify_pred <- function (tab, id, nam){
  # Modified from partykit:::.simplify_pred()
  # https://github.com/cran/partykit/blob/597245ef3dfc98411ce919b74c68ba565f077c47/R/party.R#L497-L520
  # to add as.numeric() after combining the list components with do.call()

  if (all(sapply(tab, length) == 1) & all(sapply(tab, is.atomic))) {
    ret <- do.call("c", tab) %>% as.numeric()
    names(ret) <- names(tab)
    ret <- if (is.factor(tab[[1]]))
      factor(ret[as.character(id)], levels = 1:length(levels(tab[[1]])),
             labels = levels(tab[[1]]), ordered = is.ordered(tab[[1]]))
    else ret[as.character(id)]
    names(ret) <- nam
  }
  else if (length(unique(sapply(tab, length))) == 1 & all(sapply(tab,
                                                                 is.numeric))) {
    ret <- matrix(unlist(tab), nrow = length(tab), byrow = TRUE)
    colnames(ret) <- names(tab[[1]])
    rownames(ret) <- names(tab)
    ret <- ret[as.character(id), , drop = FALSE]
    rownames(ret) <- nam
  }
  else {
    ret <- tab[as.character(id)]
    names(ret) <- nam
  }
  ret
}


#' Print a ggHeatTree object.
#' Adopted from
#' https://github.com/daattali/ggExtra/blob/master/R/ggMarginal.R#L207-L244.
#'
#' \code{ggHeatTree} objects are created from \code{heat_tree()}. This is the S3
#' generic print method to print the result of the scatterplot with its marginal
#' plots.
#'
#' @param x ggHeatTree (gtable grob) object.
#' @param newpage Should a new page (i.e., an empty page) be drawn before the
#' ggHeatTree is drawn?
#' @param ... ignored
#' @keywords internal
#' @export

print.ggHeatTree <- function(x, newpage = grDevices::dev.interactive(), ...) {
  if (newpage) grid::grid.newpage()
  # if (isTRUE(getOption("rstudio.notebook.executing"))) {
  #   # x <- ggplot2::ggplot() +
  #   #   ggplot2::geom_blank() +
  #   #   ggplot2::annotation_custom(x) +
  #   #   ggplot2::theme_void()
  #   plot(x)
  # } else {
  grid::grid.draw(x)
  # }
}
