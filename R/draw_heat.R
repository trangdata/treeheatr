#' Draws the heatmap.
#'
#' Draws the heatmap to be placed below the decision tree.
#'
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param fit party object, e.g., as output from partykit::ctree()
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
#' @inheritParams heat_tree
#'
#' @return A ggplot2 grob object of the heatmap.
#'
#' @import ggplot2
#' @export
#' @examples
#' x <- compute_tree(penguins, target_lab = 'species')
#' draw_heat(x$dat, x$fit)
#'
#'
draw_heat <- function(
  dat, fit, feat_types = NULL, target_cols = NULL, target_lab_disp = '',
  trans_type = c('percentize', 'normalize', 'scale', 'none'), clust_feats = TRUE,
  show_all_feats = FALSE, p_thres = 0.05, custom_tree = NULL, cont_legend = FALSE,
  cate_legend = FALSE, cont_cols = ggplot2::scale_fill_viridis_c,
  cate_cols = ggplot2::scale_fill_viridis_d, panel_space = 0.001, target_space = 0.05,
  target_pos = 'top'){

  trans_type <- match.arg(trans_type)

  if (is.logical(cont_legend) && cont_legend)
    cont_legend <- guide_colorbar(barwidth = 10, barheight = 0.5, title = NULL)
  if (is.logical(cate_legend) && !cate_legend){
    cate_legend <- 'none'
  } else if (is.logical(cate_legend) && cate_legend){
    cate_legend <- guide_legend(title = NULL)
  }

  cont_cols <- do.call(cont_cols, list(begin = 0.1, guide = cont_legend))
  cate_cols <- do.call(cate_cols, list(begin = 0.3, end = 0.9, guide = cate_legend))

  feat_names <- setdiff(colnames(fit$data), 'my_target')

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

  dheat <- ggplot() +
    target_cols +
    facet_grid(cols = vars(node_id), scales = 'free_x', space = 'free') +
    geom_tile(data = dat,
      aes(x = Sample, y = target_y, fill = my_target)) +
    scale_x_continuous(expand =  c(0,0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = ggplot2::unit(panel_space, 'npc'),
      plot.margin = ggplot2::unit(c(0, 5.5, 5.5, 5.5), 'pt')
    )

  if (!is.null(tile_cate)){
    tile_cate <- tile_cate %>%
      mutate(y = cate_feat %>% `levels<-`(seq.int(n_cates)) %>% as.numeric())

    for (i in levels(tile_cate$cate_feat)){
      dheat <- dheat +
        ggnewscale::new_scale_fill() +
        geom_tile(data = tile_cate %>% filter(cate_feat == i),
                           aes(y = y, x = Sample, fill = value)) +
        cate_cols
    }
  }

  if (!is.null(tile_cont)){
    tile_cont <- tile_cont %>%
      mutate(
        y = cont_feat %>%
          `levels<-`(seq(n_cates + 1, n_conts + n_cates)) %>%
          as.character() %>%
          as.numeric())

    dheat <- dheat +
      ggnewscale::new_scale_fill() +
      geom_tile(data = tile_cont, aes(y = y, x = Sample, fill = value)) +
      theme(legend.position = 'bottom') +
      cont_cols
  }

  dheat <- dheat +
    scale_y_continuous(
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


# ------------------------------------------------------------------------------------
#' Prepares the feature dataframes for tiles.
#'
#' If R does not recognize a categorical feature (input from user) as factor,
#' converts to factor.
#'
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param disp_feats Character vector specifying features to be displayed.
#' @inheritParams draw_heat
#'
#' @return A list of two dataframes (continuous and categorical)
#' from the original dataset.
#'
prepare_feats <- function(dat, disp_feats, feat_types, clust_feats, trans_type){
  cont_feats <- names(feat_types[feat_types == 'numeric'| feat_types == 'integer'])
  cate_feats <- names(feat_types[feat_types == 'factor'])

  n_conts <- sum(cont_feats %in% disp_feats)
  n_cates <- sum(cate_feats %in% disp_feats)

  if (n_conts > 0){
    df_cont <- dat %>%
      select(- cate_feats) %>%
      mutate_at(cont_feats, ~ scale_norm(., trans_type = trans_type)) %>%
      tidyr::pivot_longer(cont_feats, names_to = 'cont_feat') %>%
      filter(cont_feat %in% disp_feats) %>%
      mutate(cont_feat = as.factor(cont_feat))
  } else {
    df_cont <- NULL
  }

  if (n_cates > 0){
    df_cate <- dat %>%
      select(- cont_feats) %>%
      mutate_at(cate_feats, as.factor) %>%
      tidyr::pivot_longer(cate_feats, names_to = 'cate_feat') %>%
      filter(cate_feat %in% disp_feats) %>%
      mutate(cate_feat = as.factor(cate_feat))
  } else {
    df_cate <- NULL
  }

  if (n_conts > 1){
    clustered_conts <-
      clust_feat_func(
        dat = dat,
        clust_vec = cont_feats[cont_feats %in% disp_feats],
        clust_feats = clust_feats
      )
    df_cont$cont_feat <- factor(df_cont$cont_feat, levels = clustered_conts)
  }

  if (n_cates > 1){
    clustered_cates <-
      clust_feat_func(
        dat = dat %>% mutate_if(is.factor, as.numeric),
        clust_vec = cate_feats[cate_feats %in% disp_feats],
        clust_feats = clust_feats
      )
    df_cate$cate_feat <- factor(df_cate$cate_feat, levels = clustered_cates)
  }

  return(list(df_cont = df_cont, df_cate = df_cate))
}
