clust <- function(leaf_node = NULL, dat, clust_vec, clust_samps = TRUE, clust_feats = FALSE){
  # This function performs clustering of samples or features.
  # 
  # clust_samps = TRUE clusters samples within each leaf_node using the Gower metric
  # based on the values of the variables in clust_vec (can include class labels)
  # and returns the ordered dataframe for each leaf_node.
  # 
  # clust_feats = TRUE clusters displayed features (passed through clust_vec) 
  # using the the Gower metric based on the values of all samples 
  # and returns the ordered features.
  #
  # When clust_samps = FALSE and clust_feats = FALSE, no clustering is performed.
  
  if (clust_samps == TRUE){
    df <- dat[dat$node_id == leaf_node, ]
    
    if (length(clust_vec) > 1){
      new_samp_order <- df[, clust_vec] %>%
        cluster::daisy(metric = 'gower') %>% 
        hclust() %>%
        `[[`('order')
      
      df[new_samp_order, ] # return ordered dataframe
    } else {
      df
    }
  
  } else if (clust_feats == TRUE) {
    new_feat_order <- dat[, clust_vec] %>% 
      as.matrix() %>% 
      t() %>% 
      cluster::daisy(metric = 'gower') %>% 
      hclust() %>%
      `[[`('order')
    
    clust_vec[new_feat_order] # return ordered features, not dataframe
    
  } else { # no clustering
    dat
  }
}

