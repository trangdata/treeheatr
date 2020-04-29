#' Draws the heatmap.
#'
#' Draws the heatmap to be placed below the decision tree.
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param feat_names Character vector specifying the feature names in dat.
#' @param disp_feats Character vector specifying features to be displayed.
#' @param target_cols Function determine color scale for target,
#' defaults to viridis option B.
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
#' @param clust_feats Logical. If TRUE, performs cluster on the features.
#' @param target_space Numeric value indicating spacing between
#' the target label and the rest of the features
#' @param target_pos Character string specifying the position of the target label
#' on heatmap, can be 'top', 'bottom' or 'none'.
#' @param target_lab_disp Character string for displaying the label of target label
#' if it differs from target_lab.
#'
#' @return A ggplot2 grob object of the heatmap.
#' @export
#'
draw_heat <- function(
  dat, feat_names, disp_feats, target_cols, panel_space,
  feat_types = NULL,
  trans_type = 'normalize',
  cont_cols = ggplot2::scale_fill_viridis_c(),
  cate_cols = ggplot2::scale_fill_viridis_d(option = 'D', begin = 0.3, end = 0.9),
  clust_feats = TRUE,
  target_space = 0.05,
  target_pos = 'top',
  target_lab_disp = NULL
){

  # if feature types are not supplied, infer from column type:
  if (is.null(feat_types)){feat_types <- sapply(dat[, feat_names], class)}

  # prepare feature orders:
  feat_list <- prepare_feats(dat, disp_feats, feat_types, clust_feats, trans_type)
  tile_cont <- feat_list[['df_cont']]
  tile_cate <- feat_list[['df_cate']]

  n_conts <- length(unique(tile_cont$cont_feat))
  n_cates <- length(unique(tile_cate$cate_feat))

  # number of features displayed:
  n_feats <- length(disp_feats)
  target_y <- dplyr::case_when(
    target_pos == 'top' ~ (n_feats + 1 + target_space),
    target_pos == 'bottom' ~ (- target_space)) # if 'none', returns NA

  dheat <- ggplot2::ggplot() +
    target_cols +
    ggplot2::facet_grid(cols = vars(node_id), scales = 'free_x', space = 'free') +
    ggplot2::geom_tile(data = dat,
      ggplot2::aes(x = Sample, y = target_y, fill = my_target)) +
    ggplot2::scale_x_continuous(expand =  c(0,0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = 'None',
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(panel_space, 'npc'),
      plot.margin = ggplot2::unit(c(0, 5.5, 5.5, 5.5), 'pt')
    )

  if (!is.null(tile_cate)){
    tile_cate <- tile_cate %>%
      dplyr::mutate(y = cate_feat %>% `levels<-`(seq.int(n_cates)) %>% as.numeric())

    for (i in levels(tile_cate$cate_feat)){
      dheat <- dheat +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_tile(data = tile_cate %>% filter(cate_feat == i),
                           ggplot2::aes(y = y, x = Sample, fill = value)) +
        cate_cols
    }
  }

  if (!is.null(tile_cont)){
    tile_cont <- tile_cont %>%
      dplyr::mutate(y = cont_feat %>%
                      `levels<-`(seq(n_cates + 1, n_conts + n_cates)) %>%
                      as.character() %>%
                      as.numeric())

    dheat <- dheat +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_tile(data = tile_cont,
                         ggplot2::aes(y = y, x = Sample, fill = value)) +
      cont_cols
  }

  dheat <- dheat +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = c(target_y, seq.int(n_feats)),
      labels = c(target_lab_disp, levels(tile_cate$cate_feat), levels(tile_cont$cont_feat)))

  return(dheat)
}
