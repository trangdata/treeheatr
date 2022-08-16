test_that("get_cols works", {
  out <- get_cols(c("#1DCE9B", "#947564", "#4F5158", "#FFAC25"), "regression")
  expect_true(all(c("Scale", "ggproto", "gg") %in% class(out)))
  expect_equal(out$aesthetics, "fill")

  out <- get_cols(c("#1DCE9B", "#947564", "#4F5158", "#FFAC25"), "classification")
  expect_true(all(c("Scale", "ggproto", "gg") %in% class(out)))
  expect_equal(out$aesthetics, "fill")

  out <- get_cols(NULL, "classification")
  expect_true(all(c("Scale", "ggproto", "gg") %in% class(out)))
  expect_equal(out$aesthetics, "fill")

  out <- get_cols(NULL, "regression")
  expect_true(all(c("Scale", "ggproto", "gg") %in% class(out)))
  expect_equal(out$aesthetics, "fill")
})
