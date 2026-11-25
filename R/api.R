.get_key <- function(client, path, query, key, value_name = key) {
  x <- skosmos_request(client, path, query = query)
  if (is.null(x[[key]])) {
    stop("Expected field `", key, "` was not returned by the Skosmos server.", call. = FALSE)
  }
  .as_skosmos_tibble(x[[key]], value_name = value_name)
}

#' List vocabularies available from a Skosmos server
#'
#' Retrieves the vocabularies configured in a Skosmos installation. This is
#' usually the first function to call after creating a client because the
#' returned `id` column supplies the `vocid` values used by vocabulary-specific
#' functions.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param lang Preferred language code for vocabulary labels, for example
#'   `"en"`, `"fi"`, or `"fr"`.
#'
#' @return A tibble with one row per vocabulary. Common columns include `uri`,
#'   `id`, and `title`; exact columns depend on the Skosmos deployment.
#'
#' @examples
#' \dontrun{
#' client <- skosmos_client("https://api.finto.fi/rest/v1/")
#' vocabs <- skosmos_vocabularies(client, lang = "en")
#' vocabs
#'
#' # Select one vocabulary ID for subsequent calls.
#' vocid <- vocabs$id[1]
#' }
#' @export
skosmos_vocabularies <- function(client, lang = "en") {
  .get_key(client, "vocabularies", list(lang = lang), "vocabularies")
}

#' Get metadata for one vocabulary
#'
#' Retrieves descriptive metadata for a vocabulary configured in the selected
#' Skosmos installation.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier, normally taken from the `id`
#'   column returned by [skosmos_vocabularies()].
#' @param lang Optional language code for localized metadata.
#'
#' @return A tibble containing vocabulary metadata. Nested values are retained
#'   as list-columns when necessary.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_vocabulary(finto, vocid = "yso", lang = "en")
#'
#' # Generic discovery pattern.
#' vocabs <- skosmos_vocabularies(finto, "en")
#' skosmos_vocabulary(finto, vocabs$id[1], "en")
#' }
#' @export
skosmos_vocabulary <- function(client, vocid, lang = NULL) {
  vocid <- .scalar_chr(vocid, "vocid")
  .as_skosmos_tibble(skosmos_request(client, paste0(vocid, "/"), list(lang = lang)))
}

.search_query <- function(query, lang = NULL, labellang = NULL, vocabs = NULL,
                          type = NULL, parent = NULL, group = NULL,
                          maxhits = 100, offset = 0, fields = NULL, unique = FALSE) {
  list(query = query, lang = lang, labellang = labellang,
       vocab = if (is.null(vocabs)) NULL else paste(vocabs, collapse = " "),
       type = type, parent = parent, group = group, maxhits = maxhits,
       offset = offset, fields = if (is.null(fields)) NULL else paste(fields, collapse = " "),
       unique = tolower(as.character(isTRUE(unique))))
}

#' Search concepts across vocabularies
#'
#' Performs the global Skosmos concept search. Use `vocabs` to restrict the
#' request to one or more vocabularies, or leave it `NULL` to search all
#' vocabularies exposed by the server.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param query Search expression. Skosmos deployments commonly support
#'   wildcard characters such as `*`; exact search syntax is server-dependent.
#' @param lang Optional language of the search query.
#' @param labellang Optional preferred language for labels in returned hits.
#' @param vocabs Optional character vector of vocabulary IDs. Multiple values
#'   are sent as a space-separated Skosmos `vocab` parameter.
#' @param type Optional concept/resource type filter.
#' @param parent Optional parent concept URI used to limit results.
#' @param group Optional concept-group URI used to limit results.
#' @param maxhits Maximum number of hits requested from the server.
#' @param offset Number of hits to skip, useful for pagination.
#' @param fields Optional character vector of fields requested from Skosmos.
#' @param unique Logical; request unique search results where supported.
#'
#' @return A tibble of matching concepts. Typical columns include `uri`,
#'   `prefLabel`, `vocab`, and `type`, depending on the deployment and fields
#'   requested.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#'
#' # Search one vocabulary.
#' skosmos_search(
#'   finto,
#'   query = "forest",
#'   vocabs = "yso",
#'   lang = "en",
#'   maxhits = 10
#' )
#'
#' # Search all vocabularies on the same Skosmos server.
#' skosmos_search(finto, query = "forest", lang = "en", maxhits = 10)
#' }
#' @export
skosmos_search <- function(client, query, lang = NULL, labellang = NULL,
                           vocabs = NULL, type = NULL, parent = NULL, group = NULL,
                           maxhits = 100, offset = 0, fields = NULL, unique = FALSE) {
  q <- .search_query(query, lang, labellang, vocabs, type, parent, group,
                     maxhits, offset, fields, unique)
  .get_key(client, "search", q, "results")
}

#' Search concepts within one vocabulary
#'
#' Uses the vocabulary-specific Skosmos search route. This is convenient when
#' the vocabulary is already known and avoids passing `vocabs` separately.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param query Search expression.
#' @param lang Optional language of the search query.
#' @param labellang Optional preferred language for returned labels.
#' @param type Optional resource-type filter.
#' @param parent Optional parent concept URI filter.
#' @param group Optional concept-group URI filter.
#' @param maxhits Maximum number of requested hits.
#' @param offset Number of matching records to skip.
#' @param fields Optional character vector of fields requested from Skosmos.
#' @param unique Logical; request unique results where supported.
#'
#' @return A tibble of matching concepts.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_vocabulary_search(
#'   finto,
#'   vocid = "yso",
#'   query = "forest",
#'   lang = "en",
#'   maxhits = 10
#' )
#' }
#' @export
skosmos_vocabulary_search <- function(client, vocid, query, lang = NULL,
                                      labellang = NULL, type = NULL,
                                      parent = NULL, group = NULL,
                                      maxhits = 100, offset = 0,
                                      fields = NULL, unique = FALSE) {
  vocid <- .scalar_chr(vocid, "vocid")
  q <- .search_query(query, lang, labellang, NULL, type, parent, group,
                     maxhits, offset, fields, unique)
  .get_key(client, paste0(vocid, "/search"), q, "results")
}

#' Look up a concept by label
#'
#' Resolves an exact or near-exact label to a concept within one vocabulary
#' using the Skosmos `lookup` route.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param label Label text to resolve.
#' @param lang Optional language code of the label.
#'
#' @return A tibble containing the lookup result returned by Skosmos.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_lookup(finto, vocid = "yso", label = "cat", lang = "en")
#' }
#' @export
skosmos_lookup <- function(client, vocid, label, lang = NULL) {
  vocid <- .scalar_chr(vocid, "vocid")
  .get_key(client, paste0(vocid, "/lookup"), list(label = label, lang = lang), "result")
}

#' Get available Skosmos resource types
#'
#' Lists resource types globally or for a specific vocabulary.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param lang Preferred language code for labels.
#' @param vocid Optional single vocabulary identifier. If omitted, the global
#'   `/types` route is used; otherwise `/{vocid}/types` is used.
#'
#' @return A tibble of resource types available from the server.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#'
#' # Global types.
#' skosmos_types(finto, lang = "en")
#'
#' # Types used by one vocabulary.
#' skosmos_types(finto, lang = "en", vocid = "yso")
#' }
#' @export
skosmos_types <- function(client, lang = "en", vocid = NULL) {
  if (!is.null(vocid)) vocid <- .scalar_chr(vocid, "vocid")
  path <- if (is.null(vocid)) "types" else paste0(vocid, "/types")
  .get_key(client, path, list(lang = lang), "types")
}

#' Get top concepts of a vocabulary
#'
#' Retrieves concepts at the top of a SKOS concept hierarchy. If a vocabulary
#' contains multiple concept schemes, `scheme` can be used to select one where
#' the server supports this parameter.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param lang Optional language for returned labels.
#' @param scheme Optional concept-scheme URI.
#'
#' @return A tibble of top concepts.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_top_concepts(finto, vocid = "yso", lang = "en")
#' }
#' @export
skosmos_top_concepts <- function(client, vocid, lang = NULL, scheme = NULL) {
  vocid <- .scalar_chr(vocid, "vocid")
  .get_key(client, paste0(vocid, "/topConcepts"), list(lang = lang, scheme = scheme), "topconcepts")
}

#' Get concept groups in a vocabulary
#'
#' Retrieves SKOS concept groups or collections exposed by the selected
#' vocabulary. Not every vocabulary defines groups; an empty result is valid.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier, typically taken from
#'   [skosmos_vocabularies()].
#' @param lang Optional language for returned labels.
#'
#' @return A tibble of concept groups. Common fields include a group URI and a
#'   label, but the exact columns depend on the Skosmos deployment.
#'
#' @examples
#' \dontrun{
#' # General pattern.
#' client <- skosmos_client("https://example.org/rest/v1/")
#' vocabs <- skosmos_vocabularies(client, "en")
#' groups <- skosmos_groups(client, vocid = vocabs$id[1], lang = "en")
#' groups
#'
#' # Finto example.
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_groups(finto, vocid = "yso", lang = "en")
#' }
#' @export
skosmos_groups <- function(client, vocid, lang = NULL) {
  vocid <- .scalar_chr(vocid, "vocid")
  .get_key(client, paste0(vocid, "/groups"), list(lang = lang), "groups")
}

#' Get vocabulary statistics
#'
#' Retrieves the statistics object produced by Skosmos for one vocabulary.
#' Available statistics depend on the Skosmos version and vocabulary.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param lang Preferred language code.
#'
#' @return A tibble containing vocabulary-level statistics. Nested structures
#'   are preserved as list-columns.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_vocabulary_statistics(finto, vocid = "yso", lang = "en")
#' }
#' @export
skosmos_vocabulary_statistics <- function(client, vocid, lang = "en") {
  vocid <- .scalar_chr(vocid, "vocid")
  .as_skosmos_tibble(skosmos_request(client, paste0(vocid, "/vocabularyStatistics"), list(lang = lang)))
}

#' Get vocabulary label statistics
#'
#' Retrieves statistics about labels and languages for a vocabulary where the
#' selected Skosmos deployment provides them.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param lang Preferred language code.
#'
#' @return A tibble containing label statistics, preserving nested structures
#'   as list-columns.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_label_statistics(finto, vocid = "yso", lang = "en")
#' }
#' @export
skosmos_label_statistics <- function(client, vocid, lang = "en") {
  vocid <- .scalar_chr(vocid, "vocid")
  .as_skosmos_tibble(skosmos_request(client, paste0(vocid, "/labelStatistics"), list(lang = lang)))
}

#' List available alphabetical index letters
#'
#' Returns letters or other index buckets that can be passed to
#' [skosmos_index()] for browsing a vocabulary alphabetically.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param lang Optional language code for the index.
#'
#' @return A tibble containing index letters or buckets.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' letters <- skosmos_index_letters(finto, vocid = "yso", lang = "en")
#' letters
#' }
#' @export
skosmos_index_letters <- function(client, vocid, lang = NULL) {
  vocid <- .scalar_chr(vocid, "vocid")
  x <- skosmos_request(client, paste0(vocid, "/index/"), list(lang = lang))
  .as_skosmos_tibble(if (!is.null(x$letters)) x$letters else x, "letter")
}

#' Browse a vocabulary by alphabetical index
#'
#' Retrieves concepts that belong to one index letter or bucket.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param letter A single index letter or bucket, ideally selected from
#'   [skosmos_index_letters()].
#' @param lang Optional language code for the index.
#'
#' @return A tibble of indexed concepts.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_index(finto, vocid = "yso", letter = "A", lang = "en")
#' }
#' @export
skosmos_index <- function(client, vocid, letter, lang = NULL) {
  vocid <- .scalar_chr(vocid, "vocid")
  letter <- .scalar_chr(letter, "letter")
  x <- skosmos_request(client, paste0(vocid, "/index/", utils::URLencode(letter, reserved = TRUE)), list(lang = lang))
  .as_skosmos_tibble(if (!is.null(x$index)) x$index else x)
}

#' Get recently added concepts
#'
#' Retrieves recently created concepts where the Skosmos installation supports
#' change tracking for the vocabulary.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param lang Optional language for labels.
#' @param limit Maximum number of records requested.
#' @param offset Number of records to skip for pagination.
#'
#' @return A tibble of recently added concepts. Some servers or vocabularies may
#'   not provide this information.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_new_concepts(finto, vocid = "yso", lang = "en", limit = 20)
#' }
#' @export
skosmos_new_concepts <- function(client, vocid, lang = NULL, limit = 100,
                                 offset = 0) {
  vocid <- .scalar_chr(vocid, "vocid")
  x <- skosmos_request(client, paste0(vocid, "/new"), list(lang = lang, limit = limit, offset = offset))
  .as_skosmos_tibble(if (!is.null(x$new)) x$new else x)
}

#' Get recently modified concepts
#'
#' Retrieves recently changed concepts where the Skosmos installation supports
#' modification tracking for the vocabulary.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param lang Optional language for labels.
#' @param limit Maximum number of records requested.
#' @param offset Number of records to skip for pagination.
#'
#' @return A tibble of recently modified concepts. Some servers or vocabularies
#'   may not provide this information.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_modified_concepts(finto, vocid = "yso", lang = "en", limit = 20)
#' }
#' @export
skosmos_modified_concepts <- function(client, vocid, lang = NULL, limit = 100,
                                      offset = 0) {
  vocid <- .scalar_chr(vocid, "vocid")
  x <- skosmos_request(client, paste0(vocid, "/modified"), list(lang = lang, limit = limit, offset = offset))
  .as_skosmos_tibble(if (!is.null(x$modified)) x$modified else x)
}
