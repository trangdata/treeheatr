#' Draws the conditional decision tree.
#'
#' Draws the conditional decision tree output from partykit::ctree(),
#' utilizing ggparty geoms: geom_edge, geom_edge_label, geom_node_label.
#'
#' @import ggparty
#' @param fit ggparty object as output from partykit::ctree()
#' @param layout Dataframe of layout of all nodes, must include these columns:
#' id, x, y and y_hat.
#' @param term_dat Dataframe for terminal nodes, must include these columns:
#' id, x, y and y_hat.
#' @param text_eval Character string of the decision tree evaluation.
#'
#' @inheritParams heat_tree
#' @return A ggplot2 grob object of the decision tree.
#' @export
#'
#'
draw_tree <- function(
  fit, layout, term_dat, target_cols, tree_space_top, tree_space_bottom,
  par_node_vars, terminal_vars, edge_vars, edge_text_vars,
  print_eval, x_eval, y_eval, text_eval){

  ggparty::ggparty(fit, terminal_space = 0, layout = layout) +
    do.call(ggparty::geom_edge, edge_vars) +
    do.call(ggparty::geom_edge_label, edge_text_vars) +
    do.call(ggparty::geom_node_label, par_node_vars) +
    {if (!is.null(terminal_vars)) do.call(
      ggparty::geom_node_label,
      c(list(data = term_dat, mapping = ggplot2::aes(label = term_node, fill = term_node)),
        terminal_vars))} +
    target_cols +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0,0), c(tree_space_bottom, tree_space_top))) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::guides(fill = FALSE) +
    {if (print_eval) ggplot2::annotate(
      'text', x = x_eval, y = y_eval, label = text_eval,
      hjust = 0, size = edge_text_vars$size %||% 5)}
}
