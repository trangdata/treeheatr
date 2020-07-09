# treeheatr (development version)
* Default of `print_eval` is TRUE when `data_test` is supplied.
* `covid_train` and `covid_test` datasets are pre-processed.

Bug fixes:
* requires custom tree to be trained on dataset with dependent variable as factor when task is classification.
* `label_map` works again

treeheatr 0.2.0 (2020-07-08)
==============================

Significant changes:
* `data` argument is now replaced with `x`, which can be a dataframe (or tibble), a party (or constparty) object specifying the precomputed tree, or partynode object specifying the customized tree.
`custom_tree` argument is no longer needed.
* treeheatr() is now an alias for heat_tree()

Others:
* include the diabetes dataset
* reduce legend margin
* remove `my_target` as column names within functions
* allow wrapping as.party() around rpart object
* use ARSA as seriation method for samples
* swap the position of two arguments: `target_lab` and `data_test`
* allow the user to choose features to show in the heatmap


treeheatr 0.1.0 (2020-06-17)
==============================

### ANNOUNCEMENTS
* treeheatr 0.1.0 - first CRAN release!
