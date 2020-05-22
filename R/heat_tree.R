#' Draws and aligns decision tree and heatmap.
#'
#' @param data Tidy dataset.
#' @param target_lab Name of the column in data that contains target/label information.
#' @param data_test Tidy test dataset. If NULL, heatmap displays (training) `data`.
#' @param custom_tree Custom tree with the partykit syntax.
#' https://cran.r-project.org/web/packages/partykit/vignettes/partykit.pdf
#' If NULL, a conditional inference tree is computed.
#' @param task Character string indicating the type of problem,
#' either 'classification' (categorical outcome) or 'regression' (continuous outcome).
#' @param target_cols Function determine color scale for target,
#' defaults to viridis option B.
#' @param label_map Named vector of the meaning of the target values,
#' e.g., c(`0` = 'Edible', `1` = 'Poisonous').
#' @param custom_layout Dataframe with 3 columns: id, x and y
#' for manually input custom layout.
#' @param clust_samps Logical. If TRUE, hierarhical clustering would be performed
#' among samples within each leaf node.
#' @param clust_target Logical. If TRUE, target/label is included in hierarchical clustering
#' of samples within each leaf node and might yield a more interpretable heatmap.
#' @param show_all_feats Logical. If TRUE, show all features regarless p_thres.
#' @param p_thres Numeric value indicating the p-value threshold of feature importance.
#' Feature with p-values computed from the decision tree below this value
#' will be displayed on the heatmap.
#' @param lev_fac Relative weight of child node positions
#' according to their levels, commonly ranges from 1 to 1.5.
#' 1 for parent node perfectly in the middle of child nodes.
#' @param heat_rel_height Relative height of heatmap compared to whole figure (with tree).
#'
#' @param tree_space_top Numeric value to pass to expand for top margin of tree.
#' @param tree_space_bottom Numeric value to pass to expand for bottom margin of tree.
#' @param par_node_vars Named list containing arguments to be passed to the
#' `geom_node_label()` call for non-terminal nodes.
#' @param terminal_vars Named list containing arguments to be passed to the
#' `geom_node_label()` call for terminal nodes.
#' @param edge_vars Named list containing arguments to be passed to the
#' `geom_edge()` call for tree edges.
#' @param edge_text_vars Named list containing arguments to be passed to the
#' `geom_edge_label()` call for tree edge annotations.
#' @param print_eval Logical. If TRUE, print evaluation of the tree performance.
#' @param x_eval Numeric value indicating x position to print performance statistics.
#' @param y_eval Numeric value indicating y position to print performance statistics.
#' @param my_metrics A set of metric functions to evaluate decision tree,
#' defaults to common metrics for classification/regression problems.
#' Can be defined with `yardstick::metric_set`.
#'
#' @param feat_types Named vector indicating the type of each features,
#' e.g., c(sex = 'factor', age = 'numeric').
#' If feature types are not supplied, infer from column type.
#' @param trans_type Character string of 'normalize', 'scale' or 'none'.
#' If 'scale', subtract the mean and divide by the standard deviation.
#' If 'normalize', i.e., max-min normalize, subtract the min and divide by the max.
#' If 'none', no transformation is applied.
#' More information on what transformation to choose can be acquired here:
#' https://cran.rstudio.com/package=heatmaply/vignettes/heatmaply.html#data-transformation-scaling-normalize-and-percentize
#' @param cont_cols Function determine color scale for continuous variable,
#' defaults to viridis option D.
#' @param cate_cols Function determine color scale for nominal categorical variable,
#' defaults to viridis option D.
#' @param clust_feats Logical. If TRUE, performs cluster on the features.
#' @param target_space Numeric value indicating spacing between
#' the target label and the rest of the features
#' @param panel_space Spacing between facets relative to viewport,
#' recommended to range from 0.001 to 0.01.
#' @param target_pos Character string specifying the position of the target label
#' on heatmap, can be 'top', 'bottom' or 'none'.
#' @param target_lab_disp Character string for displaying the label of target label.
#' If NULL, use `target_lab`.
#' @param \dots further arguments passed to `partkit::ctree()`
#'
#' @return A gtable/grob object of the decision tree (top) and heatmap (bottom).
#' @export
#'
#' @examples
#' heat_tree(iris, 'Species')
#'
#' heat_tree(
#'   data = galaxy[1:100, ],
#'   target_lab = 'target',
#'   task = 'regression',
#'   terminal_vars = NULL,
#'   tree_space_bottom = 0)
#'
heat_tree <- function(
  data, target_lab,
  data_test = NULL,
  custom_tree = NULL,
  task = c('classification', 'regression'),
  target_cols = NULL,
  label_map = NULL,
  custom_layout = NULL,
  clust_samps = TRUE,
  clust_target = TRUE,
  show_all_feats = FALSE,

  p_thres = 0.05,
  lev_fac = 1.3,
  heat_rel_height = 0.2,

  ### tree parameters:
  tree_space_top = 0.05,
  tree_space_bottom = 0.05,
  par_node_vars = list(
    label.size = 0, # no border around labels, unlike terminal nodes
    label.padding = ggplot2::unit(0.15, 'lines'),
    line_list = list(ggplot2::aes(label = splitvar)),
    line_gpar = list(list(size = 9)),
    ids = 'inner'),
  terminal_vars = list(
    label.padding = ggplot2::unit(0.25, "lines"),
    size = 3,
    col = 'white'),
  edge_vars = list(color = 'grey70', size = 0.5),
  edge_text_vars = list(
    color = 'grey30', size = 3,
    mapping = ggplot2::aes(label = paste(breaks_label, "*NA"))),
  print_eval = FALSE,
  x_eval = 0,
  y_eval = 0.9,
  my_metrics = NULL,

  ### heatmap parameters:
  feat_types = NULL,
  trans_type = c('normalize', 'scale', 'none'),
  cont_cols = ggplot2::scale_fill_viridis_c(),
  cate_cols = ggplot2::scale_fill_viridis_d(),
  clust_feats = TRUE,
  target_space = 0.05,
  panel_space = 0.001,
  target_pos = 'top',
  target_lab_disp = target_lab,
  ...
){

  stopifnot(target_lab %in% colnames(data))
  task <- match.arg(task)
  trans_type <- match.arg(trans_type)
  vir_opts <- list(option = 'B', begin = 0.3, end = 0.85)
  target_cols <- target_cols %||%
    switch(task,
           classification = do.call(ggplot2::scale_fill_viridis_d, vir_opts),
           regression = do.call(ggplot2::scale_fill_viridis_c, vir_opts))

  ################################################################
  ##### Prepare dataset:

  dat <- data %>%
    dplyr::rename('my_target' = sym(!!target_lab))

  if (task == 'classification'){
    dat <- dplyr::mutate(dat, my_target = as.factor(my_target))
    dat$my_target <- tryCatch(
      recode(dat$my_target, !!!label_map),
      error = function(e) dat$my_target)
  }

  # separate feature types:
  feat_names <- setdiff(colnames(dat), 'my_target')

  # convert character features to categorical:
  dat <- dat %>%
    dplyr::mutate_if(is.character, as.factor)
  if (any(feat_types[names(which(sapply(dat, class) == 'character'))] != 'factor')){
    warning('Character variables are considered categorical.')
  }

  if (!is.null(data_test)){
    stopifnot(target_lab %in% colnames(data_test))
    data_test <- data_test %>%
      dplyr::rename('my_target' = sym(!!target_lab))

    if (task == 'classification'){
      data_test <- dplyr::mutate(data_test, my_target = as.factor(my_target))
      data_test$my_target <- tryCatch(
        recode(data_test$my_target, !!!label_map),
        error = function(e) data_test$my_target)
    }
    data_test <- data_test %>%
      dplyr::mutate_if(is.character, as.factor)
  }

  ################################################################
  ##### Compute conditional inference tree:

  ctree_result <- compute_tree(
    dat = dat,
    data_test = data_test,
    custom_tree = custom_tree,
    task = task,
    my_metrics = my_metrics,
    clust_samps = clust_samps,
    clust_target = clust_target,
    show_all_feats = show_all_feats,
    feat_names = feat_names,
    panel_space = panel_space,
    custom_layout = custom_layout,
    lev_fac = lev_fac,
    p_thres = p_thres,
    ...)

  disp_feats <- get_disp_feats(
    fit = ctree_result$fit, feat_names, show_all_feats, custom_tree, p_thres)

  ################################################################
  ##### Draw decision tree and heatmap:

  dtree <- draw_tree(
    fit = ctree_result$fit,
    layout = ctree_result$my_layout,
    term_dat = ctree_result$term_dat,
    target_cols = target_cols,
    tree_space_top = tree_space_top,
    tree_space_bottom = tree_space_bottom,
    par_node_vars = par_node_vars,
    terminal_vars = terminal_vars,
    edge_vars = edge_vars,
    edge_text_vars = edge_text_vars,
    print_eval = print_eval,
    x_eval = x_eval,
    y_eval = y_eval,
    text_eval = ctree_result$text_eval
  )

  dheat <- draw_heat(
    dat = ctree_result$scaled_dat,
    disp_feats = disp_feats,
    feat_names = feat_names,
    target_cols = target_cols,
    feat_types = feat_types,
    trans_type = trans_type,
    cont_cols = cont_cols,
    cate_cols = cate_cols,
    clust_feats = clust_feats,
    target_space = target_space,
    panel_space = panel_space,
    target_pos = target_pos,
    target_lab_disp = target_lab_disp)

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

