#' Draws and aligns decision tree and heatmap.
#'
#' @param data Tidy dataset.
#' @param data_test Tidy test dataset. If NULL, heatmap displays (training) `data`.
#' @param target_lab Name of the column in data that contains target/label information.
#' @param task Character string indicating the type of problem,
#' either 'classification' (categorical outcome) or 'regression' (continuous outcome).
#' @param feat_types Named vector indicating the type of each features,
#' e.g., c(sex = 'factor', age = 'numeric').
#' If feature types are not supplied, infer from column type.
#' @param label_map Named vector of the meaning of the target values,
#' e.g., c(`0` = 'Edible', `1` = 'Poisonous').
#' @param target_cols Character vectors representing the hex values of different
#' level colors for targets, defaults to viridis option B.
#' @param target_lab_disp Character string for displaying the label of target label.
#' If NULL, use `target_lab`.
#' @param clust_samps Logical. If TRUE, hierarhical clustering would be performed
#' among samples within each leaf node.
#' @param clust_target Logical. If TRUE, target/label is included in hierarchical clustering
#' of samples within each leaf node and might yield a more interpretable heatmap.
#' @param custom_tree Custom tree with the partykit syntax.
#' https://cran.r-project.org/web/packages/partykit/vignettes/partykit.pdf
#' If NULL, a conditional inference tree is computed.
#' @param custom_layout Dataframe with 3 columns: id, x and y
#' for manually input custom layout.
#' @param heat_rel_height Relative height of heatmap compared to whole figure (with tree).
#' @param lev_fac Relative weight of child node positions
#' according to their levels, commonly ranges from 1 to 1.5.
#' 1 for parent node perfectly in the middle of child nodes.
#' @param panel_space Spacing between facets relative to viewport,
#' recommended to range from 0.001 to 0.01.
#' @param \dots Further arguments passed to `draw_tree()` and/or `draw_heat()`.
#'
#' @return A gtable/grob object of the decision tree (top) and heatmap (bottom).
#' @export
#'
#' @examples
#' heat_tree(penguins, target_lab = 'species')
#'
#' heat_tree(
#'   data = galaxy[1:100, ],
#'   target_lab = 'target',
#'   task = 'regression',
#'   terminal_vars = NULL,
#'   tree_space_bottom = 0)
#'
heat_tree <- function(
  data, data_test = NULL, target_lab, task = c('classification', 'regression'),
  feat_types = NULL, label_map = NULL, target_cols = NULL, target_lab_disp = target_lab,
  clust_samps = TRUE, clust_target = TRUE, custom_tree = NULL, custom_layout = NULL,
  heat_rel_height = 0.2, lev_fac = 1.3, panel_space = 0.001, ...){

  stopifnot(target_lab %in% colnames(data))
  target_cols <- get_cols(target_cols, match.arg(task))
  mf <- match.call()
  ctree_vars <-
    c('data', 'data_test', 'target_lab', 'task', 'feat_types', 'label_map', 'clust_samps',
      'clust_target', 'custom_tree', 'custom_layout', 'lev_fac', 'panel_space')
  tree_vars <-
    c('title', 'task', 'tree_space_top', 'tree_space_bottom', 'par_node_vars', 'terminal_vars',
      'edge_vars', 'edge_text_vars', 'print_eval', 'metrics', 'x_eval', 'y_eval')
  heat_vars <-
    c('feat_types', 'trans_type', 'cont_cols', 'cate_cols', 'clust_feats', 'cont_legend',
      'cate_legend', 'target_space', 'panel_space', 'target_pos', 'show_all_feats', 'p_thres',
      'custom_tree')

  m_ctree <- match(ctree_vars, names(mf), 0L)
  m_tree <- match(tree_vars, names(mf), 0L)
  m_heat <- match(heat_vars, names(mf), 0L)


  ################################################################
  ##### Compute conditional inference tree:

  ctree_result <- mf[c(1L, m_ctree)]
  ctree_result[[1L]] <- quote(compute_tree)
  ctree_result <- eval(ctree_result, parent.frame())

  ################################################################
  ##### Draw decision tree and heatmap:

  dtree <- mf[c(1L, m_tree)]
  dtree$target_cols <- target_cols
  for (argg in c('fit', 'dat', 'layout', 'term_dat')){
    dtree[[argg]] <- ctree_result[[argg]]
  }
  dtree[[1L]] <- quote(draw_tree)
  dtree <- eval(dtree, parent.frame())


  dheat <- mf[c(1L, m_heat)]
  dheat$fit <- ctree_result$fit
  dheat$dat <- ctree_result$dat
  dheat$target_lab_disp <- target_lab_disp # b/c/ default in draw_heat() is NULL
  dheat$target_cols <- target_cols
  dheat[[1L]] <- quote(draw_heat)
  dheat <- eval(dheat, parent.frame())

  ################################################################
  ##### Align decision tree and heatmap:

  g <- ggplot2::ggplotGrob(dheat)
  panel_id <- g$layout[grep('panel', g$layout$name),]
  heat_height <- g$heights[panel_id[1, 't']]

  new_g <- g %>%
    gtable::gtable_add_rows(heat_height*(1/heat_rel_height - 1), 0) %>%
    gtable::gtable_add_grob(
      ggplot2::ggplotGrob(dtree),
      t = 1, l = min(panel_id$l), r = max(panel_id$l))
  class(new_g) <- c("ggHeatTree", class(new_g))

  new_g

}

