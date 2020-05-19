#' Creates smart node layout.
#'
#' Create node layout using a bottom-up approach (literally) and
#' overwrites ggarpty-precomputed positions in plot_data.
#'
#' @import dplyr
#' @param plot_data Dataframe output of `ggparty:::get_plot_data()`.
#' @param node_size Numeric vector of raw terminal node size.
#' @inheritParams heat_tree
#'
#' @return Dataframe with 3 columns: id, x and y of smart layout
#' combined with custom_layout.
#' @export
#'
position_nodes <- function(plot_data, node_size, custom_layout, lev_fac, panel_space){

  terminal_data <- plot_data %>%
    dplyr::filter(kids == 0) %>%
    dplyr::select(id, x, y, parent, level, kids)

  # Determine terminal node position based on terminal node size:
  new_x <- vector(mode = 'numeric')
  for (i in seq_along(terminal_data$id)) {
    i_id <- terminal_data$id[i]
    raw_pos <- (sum(node_size[0:i]) - node_size[i]/2)/sum(node_size)
    # white space adjusting:
    new_x[i] <- raw_pos*(1-(nrow(terminal_data) - 1)*panel_space) + (i-1)*panel_space
  }

  # Traversing upward to the parents of terminal nodes:
  traverse <- terminal_data %>% dplyr::mutate(x = new_x, y = 0)

  adj_plot_data <- plot_data %>%
    dplyr::select(id, x, y, parent, level, kids) %>%
    dplyr::filter(!id %in%terminal_data$id) %>%
    dplyr::bind_rows(traverse)

  while (!is.na(traverse$parent[1])){ # when not at Node 1
    # Find pairs of node with the same parents:
    last_lev <- traverse %>%
      dplyr::add_count(parent) %>%
      dplyr::filter(n == 2)
    these_parents <- unique(last_lev$parent)

    for (p in these_parents){ # for each pair
      kids_df <- last_lev[last_lev$parent == p, ]

      # weigh kids according to their level
      # so that the parent is closer to the higher level one:
      kids_df$x_mod <- kids_df$x*lev_fac^kids_df$level/(sum(lev_fac^(kids_df$level)))

      par_id <- adj_plot_data$id == p
      adj_plot_data[par_id, 'x'] <- sum(kids_df$x_mod)

      # remove the kids, add the parent
      traverse <- traverse %>%
        dplyr::filter(!(id %in% kids_df$id)) %>%
        dplyr::bind_rows(adj_plot_data[par_id, ])
    }
  }

  my_layout <- adj_plot_data %>%
    dplyr::filter(!(id %in% custom_layout$id)) %>%
    dplyr::select(id, x, y) %>%
    dplyr::bind_rows(custom_layout)

  return(my_layout)
}
