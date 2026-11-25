test_that("documented public Skosmos instances answer the vocabulary endpoint", {
  skip_on_cran()
  skip_if(Sys.getenv("SKOSMOSR_LIVE_TESTS") != "true", "Set SKOSMOSR_LIVE_TESTS=true for live tests")
  reg <- skosmos_instances()
  results <- lapply(reg$api_base, skosmos_test_instance)
  results <- do.call(rbind, results)
  # External services are outside package control: report failures but require
  # at least one public instance to succeed in a normal connected environment.
  expect_true(any(results$ok))
})
