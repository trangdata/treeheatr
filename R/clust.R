#' Performs clustering of samples or features.
#'
#' @param leaf_node Integer value indicating terminal node id.
#' @param dat Dataframe of the original dataset. Samples may be reordered.
#' @param clust_vec Character vector of variable names to be applied clustering on.
#' Can include class labels.
#' @param clust_samps if TRUE, clusters samples within each leaf_node using Gower metric
#' based on the values of the variables in clust_vec
#' and returns the ordered dataframe for each leaf_node.
#' @param clust_feats if TRUE clusters displayed features (passed through clust_vec)
#' using the the Gower metric based on the values of all samples
#' and returns the ordered features.
#' When clust_samps = FALSE and clust_feats = FALSE, no clustering is performed.
#'
#' @return Dataframe of reordered original dataset when clust_samps == TRUE.
#' Character vector of reordered features when clust_feats == TRUE.
#' @export
#'
#' @examples
clust <- function(leaf_node = NULL, dat, clust_vec, clust_samps = TRUE, clust_feats = FALSE){
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

