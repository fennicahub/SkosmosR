#' Create a Skosmos REST API client
#'
#' Creates a reusable client object pointing to one Skosmos REST API v1
#' installation. All other high-level functions take this object as their first
#' argument. A single package installation can therefore work with Finto,
#' UNESCO, AGROVOC, NFFA, CLARIN, or any other compatible Skosmos server simply
#' by changing `api_base`.
#'
#' @param api_base A single character string containing the base URL of a
#'   Skosmos REST API v1 service. It normally ends in `/rest/v1/`, although the
#'   path before `rest/v1` depends on the deployment.
#' @param token Optional bearer token used by protected Skosmos deployments.
#' @param headers Optional named character vector or named list of extra HTTP
#'   request headers.
#' @param timeout Request timeout in seconds. Defaults to 30.
#' @param retries Number of retries attempted for transient HTTP failures.
#'   Defaults to 2.
#' @param user_agent User-Agent header sent with requests.
#'
#' @return An object of class `skosmos_client`. Print the object to inspect the
#'   configured API base URL.
#'
#' @details
#' `SkosmosR` never assumes that a particular provider is being used. The
#' client only stores connection settings; no HTTP request is made until another
#' function such as [skosmos_vocabularies()] or [skosmos_search()] is called.
#'
#' @examples
#' \dontrun{
#' # Generic pattern: replace the URL with any Skosmos REST v1 endpoint.
#' client <- skosmos_client("https://example.org/rest/v1/")
#'
#' # Finto example.
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' finto
#'
#' # A deployment that requires a bearer token can be configured as follows.
#' private_client <- skosmos_client(
#'   "https://example.org/rest/v1/",
#'   token = Sys.getenv("SKOSMOS_TOKEN")
#' )
#' }
#' @export
skosmos_client <- function(api_base, token = NULL, headers = NULL,
                           timeout = 30, retries = 2,
                           user_agent = "SkosmosR/0.1.0") {
  if (missing(api_base) || !is.character(api_base) || length(api_base) != 1L || !nzchar(api_base)) {
    stop("`api_base` must be a single non-empty URL.", call. = FALSE)
  }
  if (!grepl("^https?://", api_base, ignore.case = TRUE)) {
    stop("`api_base` must start with http:// or https://.", call. = FALSE)
  }
  api_base <- paste0(sub("/+$", "", api_base), "/")
  structure(list(api_base = api_base, token = token, headers = headers,
                 timeout = timeout, retries = retries, user_agent = user_agent),
            class = "skosmos_client")
}

#' Print a Skosmos client
#'
#' Displays the REST API base URL stored in a [skosmos_client()] object.
#'
#' @param x A `skosmos_client` object.
#' @param ... Additional arguments, currently ignored.
#' @return `x`, invisibly.
#' @examples
#' \dontrun{
#' client <- skosmos_client("https://api.finto.fi/rest/v1/")
#' print(client)
#' }
#' @export
print.skosmos_client <- function(x, ...) {
  cat("<skosmos_client>\n", "  API: ", x$api_base, "\n", sep = "")
  invisible(x)
}

.assert_client <- function(client) {
  if (!inherits(client, "skosmos_client")) {
    stop("`client` must be created with skosmos_client().", call. = FALSE)
  }
}

.compact_query <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.build_request <- function(client, path, query = list(), accept = "application/json") {
  .assert_client(client)
  path <- sub("^/+", "", path)
  req <- httr2::request(paste0(client$api_base, path)) |>
    httr2::req_user_agent(client$user_agent) |>
    httr2::req_timeout(client$timeout) |>
    httr2::req_retry(max_tries = client$retries + 1L) |>
    httr2::req_headers(Accept = accept)
  if (!is.null(client$token)) req <- httr2::req_auth_bearer_token(req, client$token)
  if (!is.null(client$headers) && length(client$headers)) {
    req <- do.call(httr2::req_headers, c(list(req), as.list(client$headers)))
  }
  query <- .compact_query(query)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  req
}

.perform <- function(client, path, query = list(), accept = "application/json") {
  req <- .build_request(client, path, query, accept)
  tryCatch(
    httr2::req_perform(req),
    httr2_http = function(e) stop("Skosmos request failed: ", conditionMessage(e), call. = FALSE),
    error = function(e) stop("Skosmos request failed: ", conditionMessage(e), call. = FALSE)
  )
}

#' Perform a low-level Skosmos REST request
#'
#' Sends a request to an arbitrary path relative to a Skosmos REST API base.
#' This is the escape hatch for Skosmos extensions or deployment-specific
#' routes that do not have a dedicated `SkosmosR` wrapper.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param path API path relative to the configured REST base, for example
#'   `"vocabularies"`, `"search"`, or `"yso/topConcepts"`.
#' @param query Named list of URL query parameters. `NULL` elements are omitted.
#' @param accept MIME type requested in the HTTP `Accept` header.
#' @param parse Response parser. Use `"json"` for the original parsed JSON
#'   structure, `"text"` for a character string, or `"raw"` for raw bytes.
#'
#' @return The low-level response body. Unlike high-level `SkosmosR` functions,
#'   JSON responses are not converted to tibbles.
#'
#' @details
#' Prefer the high-level functions when a standard Skosmos REST v1 route is
#' available. Use this function when you need exact access to the source JSON
#' or a server exposes an additional route.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#'
#' # Equivalent to the high-level skosmos_vocabularies() request, but returns
#' # the original parsed JSON structure.
#' raw_vocabs <- skosmos_request(
#'   finto,
#'   path = "vocabularies",
#'   query = list(lang = "en")
#' )
#'
#' # Call a vocabulary-specific route directly.
#' raw_top <- skosmos_request(
#'   finto,
#'   path = "yso/topConcepts",
#'   query = list(lang = "en")
#' )
#' }
#' @export
skosmos_request <- function(client, path, query = list(),
                            accept = "application/json",
                            parse = c("json", "text", "raw")) {
  parse <- match.arg(parse)
  resp <- .perform(client, path, query, accept)
  switch(parse,
         json = httr2::resp_body_json(resp, simplifyVector = TRUE),
         text = httr2::resp_body_string(resp),
         raw = httr2::resp_body_raw(resp))
}

#' Check whether a Skosmos REST endpoint responds
#'
#' Performs a lightweight vocabulary-list request and converts success or
#' failure to a logical value. This is useful before running a larger test
#' suite against a public Skosmos server.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param lang Language code used for the vocabulary probe, for example `"en"`.
#'
#' @return `TRUE` if the vocabulary request succeeds; otherwise `FALSE`.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_ping(finto)
#'
#' ilc <- skosmos_client(
#'   "https://vocabs.ilc4clarin.ilc.cnr.it/skosmos/rest/v1/"
#' )
#' skosmos_ping(ilc)
#' }
#' @export
skosmos_ping <- function(client, lang = "en") {
  tryCatch({ skosmos_vocabularies(client, lang = lang); TRUE }, error = function(e) FALSE)
}
