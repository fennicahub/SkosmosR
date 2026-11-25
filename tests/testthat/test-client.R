test_that("client normalizes base URL", {
  x <- skosmos_client("https://example.org/rest/v1")
  expect_s3_class(x, "skosmos_client")
  expect_equal(x$api_base, "https://example.org/rest/v1/")
})

test_that("invalid base URLs fail early", {
  expect_error(skosmos_client("example.org/rest/v1"), "http")
})

test_that("registry contains valid-looking API URLs", {
  x <- skosmos_instances()
  expect_true(nrow(x) >= 5)
  expect_true(all(grepl("^https://", x$api_base)))
  expect_true(all(grepl("/rest/v1/$", x$api_base)))
})
