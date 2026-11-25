#' List known public Skosmos REST API instances
#'
#' Returns a curated convenience registry of public Skosmos deployments used in
#' examples and compatibility testing. It is intentionally not presented as an
#' exhaustive worldwide census because Skosmos is self-hosted and public
#' installations can move, disappear, or change configuration.
#'
#' @return A tibble with columns describing the registry identifier,
#'   organization, REST API base URL, domain, and status.
#'
#' @examples
#' \dontrun{
#' instances <- skosmos_instances()
#' instances
#'
#' # Build a client from one registry row.
#' finto_url <- instances$api_base[instances$id == "finto"]
#' client <- skosmos_client(finto_url)
#' skosmos_vocabularies(client, "en")
#' }
#' @export
skosmos_instances <- function() {
  tibble::tibble(
    id = c(
      "finto", "unesco", "agrovoc", "nffa", "metno", "ilc4clarin",
      "clarin-clavas", "loterre", "actris", "uio", "zpid", "dharc"
    ),
    organization = c(
      "National Library of Finland / Finto",
      "UNESCO Vocabulary Services",
      "FAO / AGROVOC",
      "NFFA-Europe / MetaRepo",
      "MET Norway Vocabulary Service",
      "ILC4CLARIN / CNR",
      "CLARIN ERIC / CLAVAS",
      "INIST-CNRS / Loterre",
      "ACTRIS / NILU",
      "University of Oslo Library",
      "Leibniz Institute for Psychology (ZPID)",
      "University of Bologna /DH.arc"
    ),
    api_base = c(
      "https://api.finto.fi/rest/v1/",
      "https://vocabularies.unesco.org/rest/v1/",
      "https://agrovoc.fao.org/browse/rest/v1/",
      "https://metarepo.nffa.eu/skosmos/rest/v1/",
      "https://vocab.met.no/rest/v1/",
      "https://vocabs.ilc4clarin.ilc.cnr.it/skosmos/rest/v1/",
      "https://vocabularies.clarin.eu/clavas/rest/v1/",
      "https://skosmos.loterre.fr/rest/v1/",
      "https://vocabulary.actris.nilu.no/skosmos/rest/v1/",
      "https://data.ub.uio.no/skosmos/rest/v1/",
      "https://vocabs.leibniz-psychology.org/rest/v1/",
      "https://lab.dharc.unibo.it/skosmos/rest/v1/"
    ),
    domain = c(
      "General / cultural heritage", "Education / culture / science",
      "Agriculture / food", "Nanoscience / materials", "Meteorology / climate",
      "Linguistics / terminology", "Research infrastructure / linguistics",
      "Research / multidisciplinary", "Atmospheric research",
      "Library / humanities", "Psychology", "Digital humanities"
    ),
    status = "publicly documented/discoverable"
  )
}

#' Smoke-test one Skosmos REST API instance
#'
#' Creates a temporary client, requests the vocabulary list, and summarizes
#' whether the endpoint responded successfully. This is designed for quick
#' compatibility checks rather than exhaustive API validation.
#'
#' @param api_base A single Skosmos REST v1 base URL.
#' @param lang Preferred language used when requesting vocabularies.
#'
#' @return A one-row tibble containing the URL, success flag, vocabulary count,
#'   error message when relevant, and elapsed time in seconds.
#'
#' @examples
#' \dontrun{
#' skosmos_test_instance("https://api.finto.fi/rest/v1/")
#'
#' skosmos_test_instance(
#'   "https://vocabs.ilc4clarin.ilc.cnr.it/skosmos/rest/v1/"
#' )
#' }
#' @export
skosmos_test_instance <- function(api_base, lang = "en") {
  api_base <- .scalar_chr(api_base, "api_base")
  started <- Sys.time()
  tryCatch({
    cli <- skosmos_client(api_base, timeout = 15, retries = 1)
    v <- skosmos_vocabularies(cli, lang)
    tibble::tibble(
      api_base = api_base, ok = TRUE, vocabularies = nrow(v),
      message = NA_character_,
      elapsed = as.numeric(difftime(Sys.time(), started, units = "secs"))
    )
  }, error = function(e) {
    tibble::tibble(
      api_base = api_base, ok = FALSE, vocabularies = NA_integer_,
      message = conditionMessage(e),
      elapsed = as.numeric(difftime(Sys.time(), started, units = "secs"))
    )
  })
}

#' Smoke-test all Skosmos instances in the bundled registry
#'
#' Runs [skosmos_test_instance()] sequentially over every endpoint returned by
#' [skosmos_instances()]. Sequential execution is intentional because public
#' terminology services may have usage policies that discourage parallel
#' request bursts.
#'
#' @param lang Preferred language used when requesting vocabularies.
#'
#' @return A tibble with one row per registered Skosmos deployment and columns
#'   for endpoint identity, response status, vocabulary count, message, and
#'   elapsed time.
#'
#' @examples
#' \dontrun{
#' results <- skosmos_test_registry(lang = "en")
#' results
#'
#' # Inspect only failures.
#' results[!results$ok, ]
#' }
#' @export
skosmos_test_registry <- function(lang = "en") {
  reg <- skosmos_instances()
  rows <- lapply(reg$api_base, skosmos_test_instance, lang = lang)
  result <- .bind_rows_base(lapply(rows, as.list))
  result$id <- reg$id
  result$organization <- reg$organization
  tibble::as_tibble(result[, c("id", "organization", "api_base", "ok", "vocabularies", "message", "elapsed")])
}
