test_that("Finto supports core Skosmos operations", {
  skip_on_cran()
  skip_if(Sys.getenv("SKOSMOSR_LIVE_TESTS") != "true", "Set SKOSMOSR_LIVE_TESTS=true for live tests")

  cli <- skosmos_client("https://api.finto.fi/rest/v1/")
  vocs <- skosmos_vocabularies(cli, "en")
  expect_gt(NROW(vocs), 0)

  yso <- skosmos_vocabulary(cli, "yso", "en")
  expect_equal(yso$id, "yso")

  hits <- skosmos_vocabulary_search(cli, "yso", "cosmology", lang = "en", maxhits = 5)
  expect_gt(NROW(hits), 0)

  concept <- skosmos_concept(cli, "yso", "http://www.yso.fi/onto/yso/p7160")
  expect_true(length(skosmos_label(concept, "en")) >= 1)
  expect_true(length(skosmos_broader(concept, "en")) >= 1)
})
