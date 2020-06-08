#' Draws the heatmap.
#'
#' Draws the heatmap to be placed below the decision tree.
#'
#' @param fit party object, e.g., as output from partykit::ctree()
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param trans_type Character string of 'normalize', 'scale' or 'none'.
#' If 'scale', subtract the mean and divide by the standard deviation.
#' If 'normalize', i.e., max-min normalize, subtract the min and divide by the max.
#' If 'none', no transformation is applied.
#' More information on what transformation to choose can be acquired here:
#' https://cran.rstudio.com/package=heatmaply/vignettes/heatmaply.html#data-transformation-scaling-normalize-and-percentize
#' @param clust_feats Logical. If TRUE, performs cluster on the features.
#' @param show_all_feats Logical. If TRUE, show all features regarless p_thres.
#' @param p_thres Numeric value indicating the p-value threshold of feature importance.
#' Feature with p-values computed from the decision tree below this value
#' will be displayed on the heatmap.
#' @param cont_legend Function determining the options for legend of continuous variables,
#' defaults to FALSE. If TRUE, use guide_colorbar(barwidth = 10, barheight = 0.5, title = NULL).
#' Any other [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) functions
#' would also work.
#' @param cate_legend Function determining the options for legend of categorical variables,
#' defaults to FALSE. If TRUE, use guide_legend(title = NULL).
#' Any other [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) functions
#' would also work.
#' @param cont_cols Function determining color scale for continuous variable,
#' defaults to scale_fill_viridis_c(guide = cont_legend)
#' @param cate_cols Function determining color scale for nominal categorical variable,
#' defaults to scale_fill_viridis_d(begin = 0.3, end = 0.9).
#' @param target_space Numeric value indicating spacing between
#' the target label and the rest of the features
#' @param target_pos Character string specifying the position of the target label
#' on heatmap, can be 'top', 'bottom' or 'none'.
#'
#' @inheritParams heat_tree
#'
#' @return A ggplot2 grob object of the heatmap.
#' @export
#'
draw_heat <- function(fit, dat, feat_types = NULL, target_cols = NULL, target_lab_disp = NULL,
  trans_type = c('percentize', 'normalize', 'scale', 'none'), clust_feats = TRUE,
  show_all_feats = FALSE, p_thres = 0.05, custom_tree = NULL,
  cont_legend = FALSE, cate_legend = FALSE,
  cont_cols = ggplot2::scale_fill_viridis_c,
  cate_cols = ggplot2::scale_fill_viridis_d,
  panel_space = 0.001, target_space = 0.05, target_pos = 'top'){

  if (is.logical(cont_legend) && cont_legend)
    cont_legend <- ggplot2::guide_colorbar(barwidth = 10, barheight = 0.5, title = NULL)
  if (is.logical(cate_legend) && !cate_legend){
    cate_legend <- 'none'
  } else if (is.logical(cate_legend) && cate_legend){
    cate_legend <- guide_legend(title = NULL)
  }

  cont_cols <- do.call(cont_cols, list(guide = cont_legend))
  cate_cols <- do.call(cate_cols, list(begin = 0.3, end = 0.9, guide = cate_legend))

  feat_names <- setdiff(colnames(fit$data), 'my_target')
  trans_type <- match.arg(trans_type)
  # if feature types are not supplied, infer from column type:
  feat_types <- feat_types %||% sapply(dat[, feat_names], class)
  disp_feats <- get_disp_feats(
    fit, feat_names, show_all_feats, custom_tree, p_thres)

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
      dplyr::mutate(
        y = cont_feat %>%
          `levels<-`(seq(n_cates + 1, n_conts + n_cates)) %>%
          as.character() %>%
          as.numeric())

    dheat <- dheat +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_tile(data = tile_cont,
                         ggplot2::aes(y = y, x = Sample, fill = value)) +
      ggplot2::theme(legend.position = 'bottom') +
      cont_cols
  }

  dheat <- dheat +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = c(target_y, seq.int(n_feats)),
      labels = c(target_lab_disp, levels(tile_cate$cate_feat), levels(tile_cont$cont_feat)))

  return(dheat)
}


# ------------------------------------------------------------------------------------
#' Select the important features to be displayed.
#'
#' Select features with p-value (computed from decision tree) < `p_thres`
#' or all features if `show_all_feats == TRUE`.
#'
#' @param fit constparty object of the decision tree.
#' @param feat_names Character vector specifying the feature names in dat.
#' @inheritParams draw_heat
#' @return A character vector of feature names.
#' @export
#'
get_disp_feats <- function(fit, feat_names, show_all_feats, custom_tree, p_thres){
  if (show_all_feats || (!is.null(custom_tree))){
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
}

