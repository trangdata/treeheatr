#' Draws the heatmap to be placed below the decision tree.
#'
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param feat_names Character vector specifying the feature names in dat.
#' @param disp_feats Character vector specifying features to be displayed.
#' @param class_cols Vector of RGBs for the class colors,
#' defaults to a colorblind friendly palette.
#' @param panel_space Spacing between facets relative to viewport,
#' recommended to range from 0.001 to 0.01.
#' @param feat_types Named vector indicating the type of each features,
#' e.g., c(sex = 'factor', age = 'numeric').
#' If feature types are not supplied, infer from column type.
#' @param trans_type Character string specifying transformation type,
#' can be 'scale' or 'normalize'.
#' @param cont_cols Function determine color scale for continuous variable,
#' defaults to viridis option D.
#' @param cate_cols Function determine color scale for nominal categorical variable,
#' defaults to viridis option D.
#' @param clust_feats If TRUE, performs cluster on the features.
#' @param class_space Numeric value indicating spacing between
#' the class label and the rest of the features
#' @param class_pos Character string specifying the position of the class label
#' on heatmap, can be 'top', 'bottom' or 'none'.
#'
#' @return A ggplot2 grob object of the heatmap.
#' @export
#'
#' @examples
draw_heat <- function(
  dat, feat_names, disp_feats, class_cols, panel_space,
  feat_types = NULL,
  trans_type = 'normalize',
  cont_cols = ggplot2::scale_fill_viridis_c(),
  cate_cols = ggplot2::scale_fill_viridis_d(option = 'D', begin = 0.3, end = 0.9),
  clust_feats = TRUE,
  class_space = 0.03,
  class_pos = 'top'
){

  # if feature types are not supplied, infer from column type:
  if (is.null(feat_types)){feat_types <- sapply(dat[, feat_names], class)}

  # prepare feature orders:
  feat_list <- prepare_feats(disp_feats, dat, feat_types, clust_feats, trans_type)
  tile_cont <- feat_list[['df_cont']]
  tile_cate <- feat_list[['df_cate']]

  # number of features displayed:
  n_feats <- length(disp_feats)

  dheat <- ggplot2::ggplot(dat) +
    ggplot2::facet_grid(cols = vars(node_id), scales = 'free_x', space = 'free') +
    ggplot2::geom_tile(
      aes(y = dplyr::case_when(
        class_pos == 'top' ~ (n_feats + 1 + class_space),
        class_pos == 'bottom' ~ (- class_space)), # if 'none', returns NA
          x = Sample, fill = my_class)) +
    ggplot2::scale_fill_manual(values = class_cols) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'None',
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(panel_space, 'npc'),
      plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'pt')
    )

  if (!is.null(tile_cate)){
    for (i in levels(tile_cate$cate_feat)){
      dheat <- dheat +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_tile(data = tile_cate %>% filter(cate_feat == i),
                           aes(y = cate_feat, x = Sample, fill = value)) +
        cate_cols
    }
  }

  if (!is.null(tile_cont)){
    dheat <- dheat +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_tile(data = tile_cont, aes(y = cont_feat, x = Sample, fill = value)) +
      cont_cols
  }

  return(dheat)
}
