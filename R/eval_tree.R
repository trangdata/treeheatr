eval_tree <- function(dat){
  switch(
    task,
    classification = eval_classification(dat),
    regression = eval_regression(dat),
  )
}


eval_classification <- function(dat){
  my_levs <- levels(dat$my_target)
  if (length(my_levs) > 2){
    stats <- metrics(dat, my_target, y_hat, !! my_levs) %>%
      mutate(print_metric = paste(toupper(.metric), format(.estimate, digits = 3), sep = ': '))
  } else {
    stats <- metrics(dat, my_target, y_hat, !! my_levs[1]) %>%
      mutate(print_metric = paste(toupper(.metric), format(.estimate, digits = 3), sep = ': '))
  }

  text_eval <- paste(stats$print_metric, collapse = '\n')

}


eval_regression <- function(dat){
}



