#' Prepares the feature dataframes for tiles.
#' If R does not recognize a categorical feature (input from user) as factor,
#' converts to factor.
#'
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param disp_feats Character vector specifying features to be displayed.
#' @param feat_types Named vector indicating the type of each features,
#' e.g., c(sex = 'factor', age = 'numeric').
#' @param clust_feats Logical. If TRUE, performs cluster on the features across all samples.
#' @param trans_type Character string specifying transformation type,
#' can be 'scale' or 'normalize'.
#'
#' @return a list of two dataframes (continuous and categorical)
#' from the original dataset.
#' @export
#'
prepare_feats <- function(dat, disp_feats, feat_types, clust_feats, trans_type){
  cont_feats <- names(feat_types[feat_types == 'numeric'| feat_types == 'integer'])
  cate_feats <- names(feat_types[feat_types == 'factor'])

  n_conts <- sum(cont_feats %in% disp_feats)
  n_cates <- sum(cate_feats %in% disp_feats)

  if (n_conts > 0){
    df_cont <- dat %>%
      dplyr::select(- cate_feats) %>%
      dplyr::mutate_at(cont_feats, ~ scale_norm(., trans_type = trans_type)) %>%
      tidyr::pivot_longer(cont_feats, names_to = 'cont_feat') %>%
      dplyr::filter(cont_feat %in% disp_feats) %>%
      dplyr::mutate(cont_feat = as.factor(cont_feat))
  } else {
    df_cont <- NULL
  }

  if (n_cates > 0){
    df_cate <- dat %>%
      dplyr::select(- cont_feats) %>%
      dplyr::mutate_at(cate_feats, as.factor) %>%
      tidyr::pivot_longer(cate_feats, names_to = 'cate_feat') %>%
      dplyr::filter(cate_feat %in% disp_feats) %>%
      dplyr::mutate(cate_feat = as.factor(cate_feat))
  } else {
    df_cate <- NULL
  }

  if (clust_feats == TRUE) {
    if (n_conts > 1){
      clustered_conts <-
        clust(
          dat = dat,
          clust_vec = cont_feats[cont_feats %in% disp_feats],
          clust_samps = FALSE,
          clust_feats = clust_feats
        )
      df_cont$cont_feat <- factor(df_cont$cont_feat, levels = clustered_conts)
    }

    if (n_cates > 1){
      clustered_cates <-
        clust(
          dat = dat %>% dplyr::mutate_if(is.factor, as.numeric),
          clust_vec = cate_feats[cate_feats %in% disp_feats],
          clust_samps = FALSE,
          clust_feats = clust_feats
        )
      df_cate$cate_feat <- factor(df_cate$cate_feat, levels = clustered_cates)

    }
  }

  return(list(df_cont = df_cont, df_cate = df_cate))
}
