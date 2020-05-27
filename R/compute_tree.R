# ------------------------------------------------------------------------------------
#' Compute decision tree from data set
#'
#' @param dat Tidy dataset with dependent variable labelled 'my_target'.
#' @param print_eval Logical. If TRUE, print evaluation of the tree performance.
#' @inheritParams heat_tree
#' @return A list of results from `partykit::ctree`, smart layout data,
#' terminal data, node labels, and features to be displayed.
#' @export
#'

compute_tree <- function(dat, data_test, task, clust_samps, clust_target,
  print_eval, custom_tree, custom_layout, lev_fac, panel_space){

  if (is.null(custom_tree)){
    fit <- partykit::ctree(my_target ~ ., data = dat)
  } else {
    fit <- partykit::party(
      custom_tree, data = dat,
      fitted = data.frame(
        "(fitted)" = partykit::fitted_node(custom_tree, data = dat),
        "(response)" = dat$my_target,
        check.names = FALSE),
      terms = stats::terms(my_target ~ ., data = dat)) %>%
      partykit::as.constparty()
  }

  scaled_dat <- prediction_df(fit, dat, data_test, task, clust_samps, clust_target)

  ################################################################
  ##### Prepare layout, terminal data, add node labels:
  plot_data <- ggparty(fit)$data

  # node_size <- node_labels$n
  terminal_data <- term_node_pos(plot_data, scaled_dat)
  my_layout <- position_nodes(plot_data, terminal_data, custom_layout, lev_fac, panel_space)

  term_dat <- terminal_data %>%
    dplyr::select(- c(x, y)) %>%
    dplyr::left_join(my_layout, by = 'id') %>%
    dplyr::mutate(
      term_node = if (task == 'classification') as.factor(y_hat) else round(y_hat, 2))

  list(fit = fit,
       dat = scaled_dat,
       layout = my_layout,
       term_dat = term_dat)
}


# ------------------------------------------------------------------------------------
#' Apply the predicted tree on either new test data or training data.
#'
#' Select features with p-value (computed from decision tree) < `p_thres`
#' or all features if `show_all_feats == TRUE`.
#'
#' @param fit constparty object of the decision tree.
#' @inheritParams compute_tree
#' @return A dataframe of prediction values with scaled columns
#' and clustered samples.
#' @export
#'
prediction_df <- function(fit, dat, data_test, task, clust_samps, clust_target){
  node_pred <- stats::predict(fit, newdata = data_test, type = 'node')
  y_pred <- stats::predict(fit, newdata = data_test, type = 'response', simplify = FALSE) %>%
    .simplify_pred(id = node_pred, nam = as.character(node_pred))

  dat_ana <- data_test %||% dat

  scaled_dat <- dat_ana %>%
    cbind(node_id = node_pred, y_hat = y_pred) %>%
    lapply(
      unique(.$node_id), clust_samp_func, dat = .,
      clust_vec = if (clust_target) colnames(dat_ana) else setdiff(colnames(dat_ana), 'my_target'),
      clust_samps = clust_samps) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Sample = row_number())

  if (task == 'classification'){
    y_prob <- stats::predict(fit, newdata = data_test, type = 'prob', simplify = FALSE) %>%
      .simplify_pred(id = node_pred, nam = as.character(node_pred))
    scaled_dat <- cbind(scaled_dat, y_prob)
  }

  scaled_dat
}


# ------------------------------------------------------------------------------------
#' Determines terminal node position.
#'
#' Create node layout using a bottom-up approach (literally) and
#' overwrites ggarpty-precomputed positions in plot_data.
#'
#' @param plot_data Dataframe output of `ggparty:::get_plot_data()`.
#' @param scaled_dat Dataframe of prediction values with scaled columns
#' and clustered samples.
#'
#' @return Dataframe with terminal node information.
#' @export
#'
term_node_pos <- function(plot_data, scaled_dat){
  node_labels <- scaled_dat %>%
    dplyr::distinct(Sample, .keep_all = T) %>%
    dplyr::count(node_id, y_hat) %>%
    dplyr::rename(id = node_id)

  terminal_data <- plot_data %>%
    dplyr::filter(kids == 0) %>%
    left_join(node_labels, by = 'id') %>%
    mutate_at(vars(n), ~ replace(., is.na(.), 0))
}


