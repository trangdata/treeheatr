#' ------------------------------------------------------------------------------------
#' Get the fitted tree depending on the input `x`.
#'
#' If `x` is a data.frame object, computes conditional tree from partkit::ctree().
#' If `x` is a partynode object specifying the customized tree, fit `x` on `data_test`.
#' If `x` is a party (or constparty) object specifying the precomputed tree, simply coerce
#' `x` to have class constparty.
#'
#' @rdname get_fit-methods
#' @inheritParams compute_tree
#' @param \dots Further arguments passed to each method.
#'
#' @return Fitted object as a list with prepped `data_test` if available.
#'
get_fit <- function(x, ...){
  UseMethod('get_fit', x)
}


#' @rdname get_fit-methods
get_fit.default <- function(x, ...){
  stop('`x` must be of class `data.frame` or `party` or `party_node`.')
}


#' @rdname get_fit-methods
get_fit.partynode <- function(x, data_test, target_lab, ...){
  if (is.null(data_test))
    stop('`data_test` must be provided when `x` is of class partynode.')

  if (is.null(target_lab))
    stop('`target_lab` must be provided when `x` is of class partynode.')

  stopifnot(target_lab %in% colnames(data_test))
  data_test <- prep_data(data_test, target_lab = target_lab, ...)
  my_formula <- stats::as.formula(paste0('`', target_lab, '` ~ .'))

  fit <- partykit::party(
    x, data = data_test,
    fitted = data.frame(
      "(fitted)" = partykit::fitted_node(x, data = data_test),
      "(response)" = data_test[, target_lab],
      check.names = FALSE),
    terms = stats::terms(my_formula, data = data_test)) %>%
    partykit::as.constparty()

  fit$target_lab <- target_lab
  fit
}


#' @rdname get_fit-methods
get_fit.party <- function(x, data_test, target_lab, task, ...){
  fit <- partykit::as.constparty(x)
  infer_target_lab <- all.vars(stats::update(fit$terms, . ~ 0))
  if (is.null(target_lab))
    target_lab <- infer_target_lab
  else if (target_lab != infer_target_lab)
    stop('Target label provided is not the same as in decision tree.')

  if (!('factor' %in% class(fit$data[, target_lab])) && task == 'classification')
    stop('Please ensure the tree was trained on a dataset with dependent variable of class factor or switch task to regression.')

  if (!is.null(data_test))
    fit$data_test <- prep_data(data_test, target_lab = target_lab, task = task, ...)
  else
    fit$data_test <- prep_data(fit$data, target_lab = target_lab, task = task, ...)

  # print(head(fit$data_test))
  fit$target_lab <- target_lab
  fit
}


#' @rdname get_fit-methods
get_fit.data.frame <- function(x, data_test, target_lab, ...) {

  stopifnot(target_lab %in% colnames(x))
  if (is.null(target_lab))
    stop('`target_lab` must be provided when `x` is of class data.frame.')

  dat <- prep_data(data = x, target_lab = target_lab, ...)
  my_formula <- stats::as.formula(paste0('`', target_lab, '` ~ .'))
  fit <- partykit::ctree(my_formula, data = dat)

  if (length(fit$node) == 0)
    stop('The computed conditional tree has only one node. It is likely that more observations or more features are needed.')

  if (!is.null(data_test))
    fit$data_test <- prep_data(data_test, target_lab = target_lab, ...)

  fit$target_lab <- target_lab
  fit
}


#' ------------------------------------------------------------------------------------
#' Prepare dataset
#' @inheritParams compute_tree
#' @param data Original data frame with features to be converted to correct types.
#'
#' @import dplyr
#'
#' @return List of dataframes (training + test) with proper feature types and target name.
#'
prep_data <- function(
  data, target_lab, task, feat_types = NULL){

  data <- as.data.frame(data)

  if (task == 'classification')
    data[, target_lab] <- as.factor(data[, target_lab])

  # convert character features to categorical:
  data <- data %>%
    mutate_if(is.logical, ~ ifelse(.x, "Yes", "No")) %>%
    mutate_if(is.character, as.factor)

  if (any(feat_types[names(which(sapply(data, class) == 'character'))] != 'factor'))
    warning('Character and variables are considered categorical.')

  data
}

