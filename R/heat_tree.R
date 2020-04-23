#' Draws and aligns decision tree and heatmap.
#'
#' @param data Tidy dataset.
#' @param class_lab Name of the column in data that contains class/label information.
#' @param class_cols Vector of RGBs for the class colors,
#' defaults to a colorblind friendly palette.
#' @param label_map Named vector of the meaning of the class values,
#' e.g., c(`0` = 'Edible', `1` = 'Poisonous').
#' @param panel_space Spacing between facets relative to viewport,
#' recommended to range from 0.001 to 0.01.
#' @param lev_fac Relative weight of child node positions
#' according to their levels, commonly ranges from 1 to 1.5.
#' 1 for parent node perfectly in the middle of child nodes.
#' @param heat_rel_height Relative height of heatmap compared to whole figure (with tree).
#' @param clust_samps Logical. If TRUE, hierarhical clustering would be performed
#' among samples within each leaf node.
#' @param clust_class Logical. If TRUE, class/label would be included in hierarchical clustering
#' of samples within each leaf node and might yield a more interpretable heatmap.
#' @param custom_layout Dataframe with 3 columns: id, x and y
#' for manually input custom layout.
#' @param p_thres Numeric value indicating the p-value threshold of feature importance.
#' Feature with p-values computed from the decision tree below this value
#' will be displayed on the heatmap.
#' @param show_all_feats Logical. If TRUE, show all features from the dataset, regarless p_thres.
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
#' @param feat_types Named vector indicating the type of each features,
#' e.g., c(sex = 'factor', age = 'numeric').
#' If feature types are not supplied, infer from column type.
#' @param trans_type Character string specifying transformation type,
#' can be 'scale' or 'normalize'.
#' @param cont_cols Function determine color scale for continuous variable,
#' defaults to viridis option D.
#' @param cate_cols Function determine color scale for nominal categorical variable,
#' defaults to viridis option D.
#' @param clust_feats Logical. If TRUE, performs cluster on the features.
#' @param class_space Numeric value indicating spacing between
#' the class label and the rest of the features
#' @param class_pos Character string specifying the position of the class label
#' on heatmap, can be 'top', 'bottom' or 'none'.
#' @param class_lab_disp Character string for displaying the label of class label
#' if it differs from class_lab.
#'
#' @return A gtable/grob object of the decision tree (top) and heatmap (bottom).
#' @export
#'
#' @examples heat_tree(iris, class_lab = 'Species')
#'
heat_tree <- function(
  data, class_lab,
  label_map = NULL,
  panel_space = 0.001,
  lev_fac = 1.3,
  heat_rel_height = 0.2,
  clust_samps = TRUE,
  clust_class = TRUE,
  custom_layout = NULL,
  p_thres = 0.05,
  show_all_feats = FALSE,
  class_cols = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7'),

  ### tree parameters:
  tree_space_top = 0.05,
  tree_space_bottom = 0.035,
  par_node_vars = list(
    label.size = 0, # no border around labels, unlike terminal nodes
    label.padding = ggplot2::unit(0.15, 'lines'),
    line_list = list(ggplot2::aes(label = splitvar)),
    line_gpar = list(list(size = 9)),
    ids = 'inner'),
  terminal_vars = list(label.padding = ggplot2::unit(0.25, 'lines'), size = 3),
  edge_vars = list(color = 'grey70', size = 0.5),
  edge_text_vars = list(color = 'grey30', size = 3),

  ### heatmap parameters:
  feat_types = NULL,
  trans_type = 'normalize',
  cont_cols = ggplot2::scale_fill_viridis_c(),
  cate_cols = ggplot2::scale_fill_viridis_d(option = 'D', begin = 0.3, end = 0.9),
  clust_feats = TRUE,
  class_space = 0.03,
  class_pos = 'top',
  class_lab_disp = class_lab
){

  ################################################################
  ##### Prepare dataset:

  dat <- data %>%
    dplyr::rename('my_class' = sym(!!class_lab)) %>%
    dplyr::mutate(my_class = as.factor(my_class))

  dat$my_class <- tryCatch(
    recode(dat$my_class, !!!label_map),
    error = function(e) dat$my_class)

  # separate feature types:
  feat_names <- setdiff(colnames(dat), 'my_class')

  # if class color scales are not supplied, use viridis pallete:
  num_class <- length(unique(dat$my_class))
  if (is.null(class_cols)){
    class_cols <- scales::viridis_pal(option = 'B', begin = 0.3, end = 0.85)(num_class)
  }


  ################################################################
  ##### Compute conditional inference tree:

  if ('character' %in% sapply(dat, class)){
    dat <- dat %>%
      dplyr::mutate_if(is.character, as.factor)
    warning('Character variables are considered categorical.')
  }

  fit <- partykit::ctree(my_class ~ ., data = dat)

  scaled_dat <- dat %>%
    dplyr::select(- my_class) %>%
    dplyr::mutate(my_class = dat$my_class,
                  node_id = stats::predict(fit, type = 'node'),
                  y_hat = stats::predict(fit, type = 'response'),
                  # y_hat = ifelse(is.numeric(y_pred), y_pred > 0.5, y_pred),
                  correct = (y_hat == my_class)) %>%
    lapply(unique(.$node_id), clust, dat = .,
           clust_vec = if (clust_class) c(feat_names, 'my_class') else feat_names,
           clust_samps = clust_samps) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Sample = row_number())


  ################################################################
  ##### Prepare layout, terminal data, add node labels:

  plot_data <- ggparty:::get_plot_data(fit)
  my_layout <- position_nodes(plot_data, custom_layout, lev_fac, panel_space)

  node_labels <- scaled_dat %>%
    dplyr::distinct(Sample, .keep_all = T) %>%
    dplyr::count(node_id, y_hat) %>%
    dplyr::rename(id = node_id)

  term_dat <- plot_data %>%
    dplyr::left_join(node_labels, by = 'id') %>%
    dplyr::select(- c(x, y)) %>%
    dplyr::left_join(my_layout, by = 'id') %>%
    dplyr::filter(kids == 0)


  ################################################################
  ##### Draw decision tree and heatmap:

  if (show_all_feats){
    disp_feats <- feat_names
  } else {
    # important features to display in decision trees
    # (pass p value threshold):
    disp_feats <- partykit::nodeapply(
      fit, ids = partykit::nodeids(fit),
      FUN = function(n) {
        node_pvals <- partykit::info_node(n)$p.value
        names(node_pvals[node_pvals < p_thres])
      }) %>%
      unlist() %>%
      unique()
  }


  dheat <- draw_heat(
    dat = scaled_dat,
    feat_names = feat_names,
    disp_feats = disp_feats,
    class_cols = class_cols,
    panel_space = panel_space,
    feat_types = feat_types,
    trans_type = trans_type,
    cont_cols = cont_cols,
    cate_cols = cate_cols,
    clust_feats = clust_feats,
    class_space = class_space,
    class_pos = class_pos,
    class_lab_disp = class_lab_disp)

  dtree <- draw_tree(
    fit = fit,
    class_cols = class_cols,
    layout = my_layout,
    term_dat = term_dat,
    tree_space_top = tree_space_top,
    tree_space_bottom = tree_space_bottom,
    par_node_vars = par_node_vars,
    terminal_vars = terminal_vars,
    edge_vars = edge_vars,
    edge_text_vars = edge_text_vars
  )

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

  new_g
  # grid::grid.newpage()
  # grid::grid.draw(new_g)
}
