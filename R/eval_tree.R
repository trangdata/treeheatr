#' Print decision tree performance according to different metrics.
#'
#' @param dat Dataframe with truths and estimates of samples from original dataset.
#' @inheritParams draw_tree
#'
#' @return Character string of the decision tree evaluation.
#' @export
#'
#'
eval_tree <- function(dat, task = c('classification', 'regression'), metrics = NULL){
  task <- match.arg(task)
  switch(
    task,
    classification = eval_class(dat, metrics),
    regression = eval_reg(dat, metrics),
  )
}

eval_class <- function(dat, metrics = NULL){
  if (!('my_target' %in% colnames(dat) && 'y_hat' %in% colnames(dat)))
    stop('Prediction dataframes must have columns `my_target` and `y_hat`')

  # Classification metrics:
  metrics <- metrics %||%
    yardstick::metric_set(
      yardstick::accuracy,
      yardstick::bal_accuracy,
      yardstick::kap,
      yardstick::roc_auc,
      yardstick::pr_auc)

  my_levs <- levels(dat$my_target)
  text_eval <- metrics(
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

eval_reg <- function(dat, metrics = NULL){
  if (!('my_target' %in% colnames(dat) && 'y_hat' %in% colnames(dat)))
    stop('Prediction dataframes must have columns `my_target` and `y_hat`')

  # Regression metrics:
  metrics <- metrics %||%
    yardstick::metric_set(
      yardstick::rsq,
      yardstick::mae,
      yardstick::rmse,
      yardstick::ccc)

  text_eval <- metrics(dat, truth = my_target, estimate = y_hat) %>%
    transmute(print_metric = paste(
      toupper(.metric),
      format(.estimate, digits = 3),
      sep = ': ')) %>%
    pull() %>%
    paste(collapse = '\n')
}
