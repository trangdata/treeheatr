#' Draws and aligns decision tree and heatmap.
#'
#' heat_tree() alias.
#'
#' @rdname heat_tree
#' @examples
#' treeheatr(penguins, target_lab = 'species')
#'
#' treeheatr(
#'   data = galaxy[1:100, ],
#'   target_lab = 'target',
#'   task = 'regression',
#'   terminal_vars = NULL,
#'   tree_space_bottom = 0)
#' @export
treeheatr <- heat_tree
