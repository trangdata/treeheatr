#' Draws the conditional decision tree.
#'
#' Draws the conditional decision tree output from partykit::ctree(),
#' utilizing ggparty geoms: geom_edge, geom_edge_label, geom_node_label.
#'
#' @import ggplot2
#' @param dat Dataframe with samples from original dataset ordered according to
#' the clustering within each leaf node.
#' @param fit party object, e.g., as output from partykit::ctree()
#' @param term_dat Dataframe for terminal nodes, must include these columns:
#' id, x, y and y_hat.
#' @param layout Dataframe of layout of all nodes, must include these columns:
#' id, x, y and y_hat.
#' @param title Character string for plot title.
#' @param tree_space_top Numeric value to pass to expand for top margin of tree.
#' @param tree_space_bottom Numeric value to pass to expand for bottom margin of tree.
#' @param print_eval Logical. If TRUE, print evaluation of the tree performance.
#' @param metrics A set of metric functions to evaluate decision tree,
#' defaults to common metrics for classification/regression problems.
#' Can be defined with `yardstick::metric_set`.
#' @param x_eval Numeric value indicating x position to print performance statistics.
#' @param y_eval Numeric value indicating y position to print performance statistics.
#' @param par_node_vars Named list containing arguments to be passed to the
#' `geom_node_label()` call for non-terminal nodes.
#' @param terminal_vars Named list containing arguments to be passed to the
#' `geom_node_label()` call for terminal nodes.
#' @param edge_vars Named list containing arguments to be passed to the
#' `geom_edge()` call for tree edges.
#' @param edge_text_vars Named list containing arguments to be passed to the
#' `geom_edge_label()` call for tree edge annotations.

#' @inheritParams heat_tree
#' @return A ggplot2 grob object of the decision tree.
#' @export
#' @examples
#' x <- compute_tree(penguins, target_lab = "species")
#' draw_tree(x$dat, x$fit, x$term_dat, x$layout)
#'
draw_tree <- function(
  dat,
  fit,
  term_dat,
  layout,
  target_cols = NULL,
  title = NULL,
  tree_space_top = 0.05,
  tree_space_bottom = 0.05,
  print_eval = FALSE,
  metrics = NULL,
  x_eval = 0,
  y_eval = 0.9,
  task = c("classification", "regression"),
  par_node_vars = list(
    label.padding = unit(0.15, "lines"),
    line_list = list(aes(label = splitvar)),
    line_gpar = list(list(size = 9)),
    ids = "inner"
  ),
  terminal_vars = list(
    label.padding = unit(0.25, "lines"),
    size = 3,
    col = "white"
  ),
  edge_vars = list(color = "grey70", linewidth = 0.5),
  edge_text_vars = list(
    color = "grey30",
    size = 3,
    mapping = aes(label = paste(breaks_label, "*NA"))
  )
) {
  text_eval <- if (print_eval) {
    eval_tree(
      dat,
      all.vars(stats::update(fit$terms, . ~ 0)),
      match.arg(task),
      metrics
    )
  } else {
    ""
  }

  ggparty::ggparty(fit, terminal_space = 0, layout = layout) +
    do.call(ggparty::geom_edge, edge_vars) +
    do.call(ggparty::geom_edge_label, edge_text_vars) +
    do.call(ggparty::geom_node_label, par_node_vars) +
    {
      if (!is.null(terminal_vars)) {
        do.call(
          ggparty::geom_node_label,
          c(
            list(
              data = term_dat,
              mapping = aes(label = term_node, fill = term_node)
            ),
            terminal_vars
          )
        )
      }
    } +
    target_cols +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(
      expand = expansion(c(0, 0), c(tree_space_bottom, tree_space_top))
    ) +
    labs(title = title) +
    coord_cartesian(xlim = c(0, 1)) +
    guides(fill = "none") +
    {
      if (print_eval) {
        annotate(
          "text",
          x = x_eval,
          y = y_eval,
          label = text_eval,
          hjust = 0,
          size = edge_text_vars$size %||% 5
        )
      }
    }
}
