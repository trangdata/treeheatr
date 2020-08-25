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


#' Get color functions from character vectors
#' @param my_cols Character vectors of different hex values
#' @inheritParams heat_tree
#'
get_cols <- function(my_cols, task, guide = FALSE) {
  vir_opts <- list(option = 'A', begin = 0.3, end = 0.9, drop = FALSE)
  my_cols <- if (!is.null(my_cols)){
    my_cols <- do.call(ggplot2::scale_fill_manual, list(values = my_cols, guide = guide, drop = FALSE))
  } else {
    switch(task,
           classification = do.call(ggplot2::scale_fill_viridis_d, vir_opts),
           regression = do.call(ggplot2::scale_fill_viridis_c, vir_opts))
  }
  my_cols
}

#' Align decision tree and heatmap:
#'
#' @param dheat ggplot2 grob object of the heatmap.
#' @param dtree ggplot2 grob object of the decision tree
#' @inheritParams heat_tree
#'
#' @return  A gtable/grob object of the decision tree (top) and heatmap (bottom).
#'
align_plots <- function(
  dheat, dtree, heat_rel_height, show = c('heat-tree', 'heat-only', 'tree-only')) {

  show <- match.arg(show)

  g_tree <- ggplot2::ggplotGrob(dtree)
  g_heat <- ggplot2::ggplotGrob(dheat)

  if (show == 'tree-only'){
    g <- g_tree
  } else if (show == 'heat-only'){
    g <- g_heat
  } else {
    panel_id <- g_heat$layout[grep('panel', g_heat$layout$name),]
    heat_height <- g_heat$heights[panel_id[1, 't']]

    g <- g_heat %>%
      gtable::gtable_add_rows(heat_height*(1/heat_rel_height - 1), 0) %>%
      gtable::gtable_add_grob(g_tree, t = 1, l = min(panel_id$l), r = max(panel_id$l))
  }

  g
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
#' @param vp viewpoint
#' @param ... ignored
#' @keywords internal
#' @import grid
#'
#' @export

print.ggHeatTree <- function (x, newpage = is.null(vp), vp = NULL, ...)
{
  ggplot2::set_last_plot(x)
  if (newpage)
    grid.newpage()
  grDevices::recordGraphics(requireNamespace("ggplot2", quietly = TRUE),
                            list(), getNamespace("ggplot2"))
  if (is.null(vp)) {
    grid.draw(x)
  }
  else {
    if (is.character(vp))
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}

