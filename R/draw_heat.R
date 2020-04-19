draw_heat <- function(
  # This function draws the heatmap to be placed below the decision tree.
  
  disp_feats, class_cols, panel_space, dat, feat_names,
  
  feat_types = NULL,
  # Named vector indicating the type of each features, e.g., c(sex = 'factor', age = 'numeric').
  # If feature types are not supplied, infer from column type.
  trans_type = 'normalize',
  # transformation type, can be 'scale' or 'normalize'
  cont_cols = ggplot2::scale_fill_viridis_c(),
  # function determine color scale for continuous variable
  cate_cols = ggplot2::scale_fill_viridis_d(option = 'D', begin = 0.3, end = 0.9),
  # function determine color scale for nominal categorical variable
  clust_feats = TRUE, # performs cluster on the features
  class_space = 0.03, # spacing between the class label and the rest of the features
  class_pos = 'top' # position of the class label on heatmap c('top', 'bottom', 'none'))
){
  
  # if feature types are not supplied, infer from column type:
  if (is.null(feat_types)){feat_types <- sapply(dat[, feat_names], class)}
  
  # prepare feature orders:
  feat_list <- prepare_feats(disp_feats, dat, feat_types, clust_feats, trans_type)
  tile_cont <- feat_list[['df_cont']]
  tile_cate <- feat_list[['df_cate']]
  
  # number of features displayed:
  n_feats <- length(disp_feats)
  
  dheat <- ggplot2::ggplot(dat) +
    ggplot2::facet_grid(cols = vars(node_id), scales = 'free_x', space = 'free') +
    ggplot2::geom_tile(
      aes(y = dplyr::case_when(
        class_pos == 'top' ~ (n_feats + 1 + class_space), 
        class_pos == 'bottom' ~ (- class_space)), # if 'none', returns NA
          x = Sample, fill = my_class)) +
    ggplot2::scale_fill_manual(values = class_cols) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'None',
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(panel_space, 'npc'),
      plot.margin = unit(c(0, 5.5, 5.5, 5.5), 'pt')
    )
  
  if (!is.null(tile_cate)){
    for (i in levels(tile_cate$cate_feat)){
      dheat <- dheat +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_tile(data = tile_cate %>% filter(cate_feat == i), 
                           aes(y = cate_feat, x = Sample, fill = value)) +
        cate_cols
    }
  }
  
  if (!is.null(tile_cont)){
    dheat <- dheat +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_tile(data = tile_cont, aes(y = cont_feat, x = Sample, fill = value)) +
      cont_cols
  }
  
  return(dheat)
}