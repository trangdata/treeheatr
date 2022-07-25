#' Performs clustering of samples.
#'
#' @import dplyr
#' @param leaf_node Integer value indicating terminal node id.
#' @param dat Dataframe of the original dataset. Samples may be reordered.
#' @param clust_vec Character vector of variable names to be applied clustering on.
#' Can include class labels.
#' @inheritParams heat_tree
#'
#' @return Dataframe of reordered original dataset when clust_samps == TRUE.
#'
clust_samp_func <- function(leaf_node = NULL, dat, clust_vec, clust_samps = TRUE) {
  df <- dat[dat$node_id == leaf_node, ]

  if (clust_samps && length(clust_vec) > 1 && nrow(df) > 1) {
    new_samp_order <- df[, clust_vec] %>%
      cluster::daisy(metric = "gower") %>%
      seriation::seriate(method = "ARSA") %>%
      seriation::get_order()

    df[new_samp_order, ] # return ordered dataframe
  } else {
    df
  }
}


# ----------------------------------------------------------------------
#'
#' Performs clustering or features.
#'
#' @import dplyr
#' @param dat Dataframe of the original dataset. Samples may be reordered.
#' @param clust_vec Character vector of variable names to be applied clustering on.
#' Can include class labels.
#' @param clust_feats if TRUE clusters displayed features (passed through `clust_vec`)
#' using the the Gower metric based on the values of all samples
#' and returns the ordered features.
#' When `clust_samps = FALSE` and `clust_feats = FALSE`, no clustering is performed.
#'
#' @return Character vector of reordered features when `clust_feats == TRUE`.
#'
clust_feat_func <- function(dat, clust_vec, clust_feats = TRUE) {
  if (clust_feats) {
    new_feat_order <- dat[, clust_vec] %>%
      as.matrix() %>%
      t() %>%
      cluster::daisy(metric = "gower") %>%
      seriation::seriate() %>%
      seriation::get_order()

    clust_vec[new_feat_order] # return ordered features, not dataframe
  } else {
    clust_vec
  }
}
