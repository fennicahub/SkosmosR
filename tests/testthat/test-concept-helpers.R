test_that("concept handles preserve endpoint, vocabulary and URI", {
  cli <- skosmos_client("https://example.org/rest/v1/")
  x <- skosmos_concept(cli, "demo", "https://example.org/concept/1")
  expect_s3_class(x, "skosmos_concept")
  expect_equal(x$vocid, "demo")
  expect_equal(x$uri, "https://example.org/concept/1")
})
