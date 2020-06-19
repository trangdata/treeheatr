#' Print decision tree performance according to different metrics.
#'
#' @param dat Dataframe with truths (column `target_lab`) and estimates (column `y_hat`)
#' of samples from original dataset.
#' @param target_lab Name of the column in data that contains target/label information.
#' @inheritParams draw_tree
#'
#' @return Character string of the decision tree evaluation.
#' @export
#' @examples eval_tree(compute_tree(penguins, target_lab = 'species')$dat)
#'
eval_tree <- function(
  dat, target_lab = colnames(dat)[1], task = c('classification', 'regression'),
  metrics = NULL){

  task <- match.arg(task)
  switch(
    task,
    classification = eval_class(dat, target_lab, metrics),
    regression = eval_reg(dat, target_lab, metrics),
  )
}

eval_class <- function(dat, target_lab, metrics = NULL){
  if (!(target_lab %in% colnames(dat) && 'y_hat' %in% colnames(dat)))
    stop(sprintf('Prediction dataframes must have columns %s and `y_hat`', target_lab))

  # Classification metrics:
  metrics <- metrics %||%
    yardstick::metric_set(
      yardstick::accuracy,
      yardstick::bal_accuracy,
      yardstick::kap,
      yardstick::roc_auc,
      yardstick::pr_auc)

  my_levs <- levels(dat[, target_lab])
  text_eval <- metrics(
    dat,
    truth = !! target_lab,
    estimate = y_hat,
    !! if (length(my_levs) > 2) my_levs else my_levs[1]) %>%
    transmute(print_metric = paste(
      toupper(.metric),
      format(.estimate, digits = 3),
      sep = ': ')) %>%
    pull() %>%
    paste(collapse = '\n')
  text_eval
}

eval_reg <- function(dat, target_lab, metrics = NULL){
  if (!(target_lab %in% colnames(dat) && 'y_hat' %in% colnames(dat)))
    stop(sprintf('Prediction dataframes must have columns %s and `y_hat`', target_lab))

  # Regression metrics:
  metrics <- metrics %||%
    yardstick::metric_set(
      yardstick::rsq,
      yardstick::mae,
      yardstick::rmse,
      yardstick::ccc)

  text_eval <- metrics(dat, truth = !!target_lab, estimate = y_hat) %>%
    transmute(print_metric = paste(
      toupper(.metric),
      format(.estimate, digits = 3),
      sep = ': ')) %>%
    pull() %>%
    paste(collapse = '\n')
  text_eval
}
