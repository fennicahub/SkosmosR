test_that("Skosmos JSON conversion returns tibbles", {
  x <- .as_skosmos_tibble(data.frame(id = c("a", "b"), title = c("A", "B")))
  expect_s3_class(x, "tbl_df")
  expect_equal(nrow(x), 2)

  y <- .as_skosmos_tibble(c("A", "B"), value_name = "label")
  expect_s3_class(y, "tbl_df")
  expect_named(y, "label")
  expect_equal(nrow(y), 2)
})

test_that("scalar identifiers fail clearly", {
  expect_error(.scalar_chr(c("a", "b"), "vocid"), "exactly one")
  expect_error(.scalar_chr("", "vocid"), "exactly one")
})
