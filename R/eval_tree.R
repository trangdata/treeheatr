#' Print decision tree performance according to different metrics.
#'
#' @param dat Dataframe with samples from original dataset.
#' @inheritParams heat_tree
#'
#' @return Character string of the decision tree evaluation.
#' @export
#'
#'
eval_tree <- function(dat, task, my_metrics){
  switch(
    task,
    classification = eval_class(dat, my_metrics),
    regression = eval_reg(dat, my_metrics),
  )
}

eval_class <- function(dat, my_metrics){
  # Classification metrics:
  my_metrics <- my_metrics %||%
    yardstick::metric_set(
      yardstick::accuracy,
      yardstick::bal_accuracy,
      yardstick::kap,
      yardstick::roc_auc,
      yardstick::pr_auc)

  my_levs <- levels(dat$my_target)
  text_eval <- my_metrics(
    dat,
    truth = my_target,
    estimate = y_hat,
    !! if (length(my_levs) > 2) my_levs else my_levs[1]) %>%
    transmute(print_metric = paste(
      toupper(.metric),
      format(.estimate, digits = 3),
      sep = ': ')) %>%
    pull() %>%
    paste(collapse = '\n')

}


eval_reg <- function(dat, my_metrics){
  # Regression metrics:
  my_metrics <- my_metrics %||%
    yardstick::metric_set(
      yardstick::rsq,
      yardstick::mae,
      yardstick::rmse,
      yardstick::ccc)

  text_eval <- my_metrics(dat, truth = my_target, estimate = y_hat) %>%
    transmute(print_metric = paste(
      toupper(.metric),
      format(.estimate, digits = 3),
      sep = ': ')) %>%
    pull() %>%
    paste(collapse = '\n')
}
