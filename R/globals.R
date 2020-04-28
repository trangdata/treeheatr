## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c('Sample', 'cate_feat', 'cont_feat', 'kids',
                         'level', 'my_target', 'node_id', 'parent',
                         'splitvar', 'value', 'x', 'y', 'y_hat', '.'))
}
