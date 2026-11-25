#' Retrieve RDF data from Skosmos
#'
#' Calls the global or vocabulary-specific Skosmos `data` route and returns the
#' RDF serialization requested with `format`.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param uri Optional URI of a concept or other resource. When supplied, the
#'   response describes that resource.
#' @param vocid Optional single vocabulary identifier. When supplied without
#'   `uri`, the function requests RDF for the complete vocabulary where the
#'   deployment supports vocabulary downloads.
#' @param format RDF MIME type requested from Skosmos, for example
#'   `"text/turtle"`, `"application/rdf+xml"`, or `"application/ld+json"`.
#' @param parse Return type: `"text"` for a character string or `"raw"` for
#'   raw bytes.
#'
#' @return RDF content as a character string or raw vector.
#'
#' @details
#' Use [skosmos_vocabulary_data()] when retrieving an entire vocabulary and
#' [skosmos_concept_data()] when you already have a concept handle. Use
#' [skosmos_rdf()] if you want the result parsed into an `rdflib` RDF object.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#'
#' # Retrieve one concept as Turtle.
#' ttl <- skosmos_data(
#'   finto,
#'   uri = "http://www.yso.fi/onto/yso/p7160",
#'   vocid = "yso",
#'   format = "text/turtle"
#' )
#'
#' # Retrieve the same resource as JSON-LD.
#' jsonld <- skosmos_data(
#'   finto,
#'   uri = "http://www.yso.fi/onto/yso/p7160",
#'   vocid = "yso",
#'   format = "application/ld+json"
#' )
#' }
#' @export
skosmos_data <- function(client, uri = NULL, vocid = NULL,
                         format = "text/turtle", parse = c("text", "raw")) {
  parse <- match.arg(parse)
  if (!is.null(vocid)) vocid <- .scalar_chr(vocid, "vocid")
  if (!is.null(uri)) uri <- .scalar_chr(uri, "uri")
  path <- if (is.null(vocid)) "data" else paste0(vocid, "/data")
  query <- list(uri = uri, format = format)
  skosmos_request(client, path, query = query, accept = format, parse = parse)
}

#' Retrieve RDF from Skosmos and parse it with rdflib
#'
#' Convenience wrapper around [skosmos_data()] that parses returned RDF using
#' the optional `rdflib` package.
#'
#' @param ... Arguments passed to [skosmos_data()], such as `client`, `uri`,
#'   `vocid`, and `format`.
#' @param rdf_format Format name understood by `rdflib::rdf_parse()`, such as
#'   `"turtle"` or `"rdfxml"`.
#'
#' @return An RDF object created by `rdflib::rdf_parse()`.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' graph <- skosmos_rdf(
#'   finto,
#'   uri = "http://www.yso.fi/onto/yso/p7160",
#'   vocid = "yso",
#'   format = "text/turtle",
#'   rdf_format = "turtle"
#' )
#' graph
#' }
#' @export
skosmos_rdf <- function(..., rdf_format = "turtle") {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    stop("Install the suggested package `rdflib` to parse RDF.", call. = FALSE)
  }
  txt <- skosmos_data(..., parse = "text")
  rdflib::rdf_parse(txt, format = rdf_format)
}

#' Retrieve RDF for a complete vocabulary
#'
#' Convenience wrapper around [skosmos_data()] for vocabulary-level RDF
#' downloads.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param format RDF MIME type, for example `"text/turtle"`.
#' @param parse Return `"text"` or `"raw"`.
#'
#' @return RDF content as text or raw bytes.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' yso_turtle <- skosmos_vocabulary_data(
#'   finto,
#'   vocid = "yso",
#'   format = "text/turtle"
#' )
#' }
#' @export
skosmos_vocabulary_data <- function(client, vocid, format = "text/turtle",
                                    parse = c("text", "raw")) {
  vocid <- .scalar_chr(vocid, "vocid")
  skosmos_data(client, vocid = vocid, format = format, parse = match.arg(parse))
}

#' Retrieve RDF for a concept handle
#'
#' Convenience wrapper around [skosmos_data()] that uses the vocabulary ID and
#' URI stored in a [skosmos_concept()] object.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param format RDF MIME type, for example `"text/turtle"` or
#'   `"application/ld+json"`.
#' @param parse Return `"text"` or `"raw"`.
#'
#' @return RDF content as text or raw bytes.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_concept_data(concept, format = "text/turtle")
#' }
#' @export
skosmos_concept_data <- function(concept, format = "text/turtle",
                                 parse = c("text", "raw")) {
  if (!inherits(concept, "skosmos_concept")) {
    stop("`concept` must be created with skosmos_concept().", call. = FALSE)
  }
  skosmos_data(concept$client, uri = concept$uri, vocid = concept$vocid,
               format = format, parse = match.arg(parse))
}
