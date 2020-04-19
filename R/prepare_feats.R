prepare_feats <- function(disp_feats, dat, feat_types, clust_feats, trans_type){
  # This function prepares the feature dataframes for tiles.
  # It returns a list of two dataframes (continuous and categorical)
  # from the original dataset. 
  # If clust_feats = TRUE, the features are clustered in each dataframe
  # across all samples.
  # If R does not recognize a categorical feature (input from user)
  # as factor, this function converts it to factor.
  
  cont_feats <- names(feat_types[feat_types == 'numeric'| feat_types == 'integer'])
  cate_feats <- names(feat_types[feat_types == 'factor'])
  
  n_conts <- sum(cont_feats %in% disp_feats)
  n_cates <- sum(cate_feats %in% disp_feats)
  
  if (n_conts > 0){
    df_cont <- dat %>% 
      dplyr::select(- all_of(cate_feats)) %>%
      dplyr::mutate_at(cont_feats, ~ scale_stand(., trans_type = trans_type)) %>% 
      tidyr::pivot_longer(all_of(cont_feats), names_to = 'cont_feat') %>% 
      dplyr::filter(cont_feat %in% disp_feats) %>% 
      dplyr::mutate(cont_feat = as.factor(cont_feat))
  } else {
    df_cont <- NULL
  }
  
  if (n_cates > 0){
    df_cate <- dat %>% 
      dplyr::select(- all_of(cont_feats)) %>% 
      dplyr::mutate_at(cate_feats, as.factor) %>% 
      tidyr::pivot_longer(all_of(cate_feats), names_to = 'cate_feat') %>% 
      dplyr::filter(cate_feat %in% disp_feats) %>% 
      dplyr::mutate(cate_feat = as.factor(cate_feat))
  } else {
    df_cate <- NULL
  }
  
  if (clust_feats == TRUE) {
    if (n_conts > 1){
      levels(df_cont$cont_feat) <-
        clust(
          dat = dat,
          clust_vec = cont_feats[cont_feats %in% disp_feats],
          clust_samps = FALSE,
          clust_feats = clust_feats
        )
    }
    
    if (n_cates > 1){
      levels(df_cate$cate_feat) <-
        clust(
          dat = dat %>% mutate_if(is.factor, as.numeric),
          clust_vec = cate_feats[cate_feats %in% disp_feats],
          clust_samps = FALSE,
          clust_feats = clust_feats
        )
    }
  }
  
  return(list(df_cont = df_cont, df_cate = df_cate))
}