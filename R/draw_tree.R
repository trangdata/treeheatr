#' Draws the conditional decision tree.
#'
#' Draws the conditional decision tree output from partykit::ctree(),
#' utilizing ggparty geoms: geom_edge, geom_edge_label, geom_node_label.
#'
#' @import ggparty
#' @param fit ggparty object as output from partykit::ctree()
#' @param class_cols Vector of RGBs for the class colors,
#' defaults to a colorblind friendly palette.
#' @param layout Dataframe of layout of all nodes, must include these columns:
#' id, x, y and y_hat.
#' @param term_dat Dataframe for terminal nodes, must include these columns:
#' id, x, y and y_hat.
#' @param tree_space_top Numeric value to pass to expand for top margin of tree.
#' @param tree_space_bottom Numeric value to pass to expand for bottom margin of tree.
#' @param par_node_vars Named list containing arguments to be passed to the
#' `geom_node_label()` call for non-terminal nodes.
#' @param terminal_vars Named list containing arguments to be passed to the
#' `geom_node_label()` call for terminal nodes.
#' @param edge_vars Named list containing arguments to be passed to the
#' `geom_edge()` call for tree edges.
#' @param edge_text_vars Named list containing arguments to be passed to the
#' `geom_edge_label()` call for tree edge annotations.
#'
#' @return A ggplot2 grob object of the decision tree.
#' @export
#'
#' @examples
draw_tree <- function(
  fit, class_cols, layout, term_dat,
  tree_space_top = 0.05,
  tree_space_bottom = 0.035,
  par_node_vars = list(
    label.size = 0, # no border around labels, unlike terminal nodes
    label.padding = ggplot2::unit(0.15, "lines"),
    line_list = list(ggplot2::aes(label = splitvar)),
    line_gpar = list(list(size = 9))),
  terminal_vars = list(label.padding = ggplot2::unit(0.25, "lines"), size = 3),
  edge_vars = list(color = 'grey70', size = 0.5),
  edge_text_vars = list(color = 'grey30', size = 3)
){

  ggparty::ggparty(fit, terminal_space = 0, layout = layout) +
  do.call(ggparty::geom_edge, edge_vars) +
  do.call(ggparty::geom_edge_label, edge_text_vars) +
  do.call(ggparty::geom_node_label, par_node_vars) +
  do.call(ggparty::geom_node_label,
          c(list(data = term_dat,
                 mapping = ggplot2::aes(label = y_hat, fill = y_hat),
                 col = 'white'),
            terminal_vars)) +
  ggplot2::scale_x_continuous(expand = c(0,0)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0,0), c(tree_space_bottom, tree_space_top))) +
  ggplot2::scale_fill_manual(values = class_cols, drop = F) +
  ggplot2::coord_cartesian(xlim = c(0, 1)) +
  ggplot2::guides(fill = FALSE)
}
