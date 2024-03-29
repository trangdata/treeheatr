# ------------------------------------------------------------------------------------
#' Compute decision tree from data set
#'
#' @inheritParams heat_tree
#' @return A list of results from `partykit::ctree` or provided custom tree, including
#' fit, estimates, smart layout and terminal data.
#' @export
#' @examples
#' fit_tree <- compute_tree(penguins, target_lab = "species")
#' fit_tree$fit
#' fit_tree$layout
#' dplyr::select(fit_tree$term_dat, -contains("nodedata"))
#'
compute_tree <- function(x, data_test = NULL, target_lab = NULL, task = c("classification", "regression"),
                         feat_types = NULL, label_map = NULL, clust_samps = TRUE, clust_target = TRUE,
                         custom_layout = NULL, lev_fac = 1.3, panel_space = 0.001) {
  task <- match.arg(task)
  fit <- get_fit(
    x = x, data_test = data_test, target_lab = target_lab,
    task = task, feat_types = feat_types
  )

  fit$autotree <- 'data.frame' %in% class(x)
  dat <- prediction_df(fit, task, clust_samps, clust_target)

  ################################################################
  ##### Prepare layout, terminal data, add node labels:
  plot_data <- ggparty::ggparty(fit)$data

  # node_size <- node_labels$n
  terminal_data <- term_node_pos(plot_data, dat)
  layout <- position_nodes(plot_data, terminal_data, custom_layout, lev_fac, panel_space)

  # browser()

  term_dat <- terminal_data %>%
    select(-c(x, y)) %>%
    left_join(layout, by = "id") %>%
    mutate(term_node = (
      if (task == "classification") {
        if (!is.null(label_map)) {
          recode(as.factor(y_hat), !!!label_map)
        } else {
          as.factor(y_hat)
        }
      } else {
        round(y_hat, 2)
      }))

  list(
    fit = fit,
    dat = dat,
    layout = layout,
    term_dat = term_dat
  )
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
#'
prediction_df <- function(fit, task, clust_samps, clust_target) {
  data <- fit$data_test %||% fit$data
  data <- stats::na.omit(data)
  node_pred <- stats::predict(fit, newdata = data, type = "node")
  y_pred <- stats::predict(fit, newdata = data, type = "response", simplify = FALSE) %>%
    .simplify_pred(id = node_pred, nam = as.character(node_pred))

  data_pred <- data %>%
    cbind(node_id = node_pred, y_hat = y_pred) %>%
    lapply(
      unique(.$node_id), clust_samp_func,
      dat = .,
      clust_vec = if (clust_target) colnames(data) else setdiff(colnames(data), fit$target_lab),
      clust_samps = clust_samps
    ) %>%
    bind_rows() %>%
    mutate(Sample = row_number())

  if (task == "classification") {
    y_prob <- stats::predict(fit, newdata = data, type = "prob", simplify = FALSE) %>%
      .simplify_pred(id = node_pred, nam = as.character(node_pred))
    data_pred <- cbind(data_pred, y_prob)
  }

  data_pred
}


# ------------------------------------------------------------------------------------
#' Determines terminal node position.
#'
#' Create node layout using a bottom-up approach (literally) and
#' overwrites ggparty-precomputed positions in plot_data.
#'
#' @param plot_data Dataframe output of `ggparty:::get_plot_data()`.
#' @param dat Dataframe of prediction values with scaled columns
#' and clustered samples.
#'
#' @return Dataframe with terminal node information.
#'
term_node_pos <- function(plot_data, dat) {
  node_labels <- dat %>%
    distinct(Sample, .keep_all = T) %>%
    count(node_id, y_hat) %>%
    rename(id = node_id)

  plot_data %>%
    filter(kids == 0) %>%
    left_join(node_labels, by = "id") %>%
    mutate_at(vars(n), ~ replace(., is.na(.), 0))
}


# ------------------------------------------------------------------------------------
#' Creates smart node layout.
#'
#' Create node layout using a bottom-up approach (literally) and
#' overwrites ggparty-precomputed positions in plot_data.
#'
#' @param plot_data Dataframe output of `ggparty:::get_plot_data()`.
#' @param terminal_data Dataframe of terminal node information including id
#' and raw terminal node size.
#' @inheritParams heat_tree
#'
#' @return Dataframe with 3 columns: id, x and y of smart layout
#' combined with custom_layout.
#'
position_nodes <- function(plot_data, terminal_data, custom_layout, lev_fac, panel_space) {
  node_size <- terminal_data$n

  # Determine terminal node position based on terminal node size:
  new_x <- vector(mode = "numeric")
  for (i in seq_along(terminal_data$id)) {
    i_id <- terminal_data$id[i]
    raw_pos <- (sum(node_size[0:i]) - node_size[i] / 2) / sum(node_size)
    # white space adjusting:
    new_x[i] <- raw_pos * (1 - (nrow(terminal_data) - 1) * panel_space) + (i - 1) * panel_space
  }

  # Traversing upward to the parents of terminal nodes:
  traverse <- terminal_data %>% mutate(x = new_x, y = 0)

  adj_plot_data <- plot_data %>%
    select(id, x, y, parent, level, kids) %>%
    filter(!id %in% terminal_data$id) %>%
    bind_rows(traverse)

  while (!is.na(traverse$parent[1])) { # when not at Node 1
    # Find pairs of node with the same parents:
    last_lev <- traverse %>%
      select(-n) %>%
      add_count(parent) %>%
      filter(n == 2)
    these_parents <- unique(last_lev$parent)

    for (p in these_parents) { # for each pair
      kids_df <- last_lev[last_lev$parent == p, ]

      # weigh kids according to their level
      # so that the parent is closer to the higher level one:
      kids_df$x_mod <- kids_df$x * lev_fac^kids_df$level / (sum(lev_fac^(kids_df$level)))

      par_id <- adj_plot_data$id == p
      adj_plot_data[par_id, "x"] <- sum(kids_df$x_mod)

      # remove the kids, add the parent
      traverse <- traverse %>%
        filter(!(id %in% kids_df$id)) %>%
        bind_rows(adj_plot_data[par_id, ])
    }
  }

  adj_plot_data %>%
    filter(!(id %in% custom_layout$id)) %>%
    select(id, x, y) %>%
    bind_rows(custom_layout)
}
