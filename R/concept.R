#' Create a Skosmos concept handle
#'
#' Creates a small reusable object that stores the Skosmos client, vocabulary
#' identifier, and concept URI. Concept-level functions such as
#' [skosmos_broader()], [skosmos_narrower()], and [skosmos_mappings()] operate
#' on this handle.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param vocid A single vocabulary identifier.
#' @param uri A single URI identifying the concept or concept group.
#'
#' @return An object of class `skosmos_concept`.
#'
#' @details
#' Creating a concept handle does not make an HTTP request. It only stores the
#' information needed by subsequent concept-level calls.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto,
#'   vocid = "yso",
#'   uri = "http://www.yso.fi/onto/yso/p7160"
#' )
#' concept
#' }
#' @export
skosmos_concept <- function(client, vocid, uri) {
  .assert_client(client)
  vocid <- .scalar_chr(vocid, "vocid")
  uri <- .scalar_chr(uri, "uri")
  structure(list(client = client, vocid = vocid, uri = uri), class = "skosmos_concept")
}

#' Print a Skosmos concept handle
#'
#' Displays the vocabulary identifier and concept URI stored in a
#' [skosmos_concept()] object.
#'
#' @param x A `skosmos_concept` object.
#' @param ... Additional arguments, currently ignored.
#' @return `x`, invisibly.
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' print(concept)
#' }
#' @export
print.skosmos_concept <- function(x, ...) {
  cat("<skosmos_concept>\n", "  vocabulary: ", x$vocid, "\n", "  URI: ", x$uri, "\n", sep = "")
  invisible(x)
}

.concept_call <- function(concept, route, key, lang = NULL, limit = NULL, value_name = key) {
  if (!inherits(concept, "skosmos_concept")) {
    stop("`concept` must be created with skosmos_concept().", call. = FALSE)
  }
  .get_key(concept$client, paste0(concept$vocid, "/", route),
           list(uri = concept$uri, lang = lang, limit = limit), key,
           value_name = value_name)
}

#' Get the preferred label of a concept
#'
#' Retrieves the preferred label for the concept URI stored in a
#' [skosmos_concept()] handle.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code of the preferred label.
#'
#' @return A tibble containing the preferred label returned by Skosmos.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_label(concept, lang = "en")
#' }
#' @export
skosmos_label <- function(concept, lang = NULL) {
  .concept_call(concept, "label", "prefLabel", lang, value_name = "prefLabel")
}

#' Get direct broader concepts
#'
#' Retrieves concepts connected to the selected concept through the direct
#' SKOS broader relationship.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble of direct broader concepts. An empty tibble is possible for
#'   a top concept.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_broader(concept, lang = "en")
#' }
#' @export
skosmos_broader <- function(concept, lang = NULL) .concept_call(concept, "broader", "broader", lang)

#' Get all transitive broader concepts
#'
#' Traverses the broader hierarchy and returns ancestors of the selected
#' concept where the Skosmos deployment supports `broaderTransitive`.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#' @param limit Optional maximum number of returned concepts.
#'
#' @return A tibble of transitive broader concepts.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_broader_transitive(concept, lang = "en", limit = 20)
#' }
#' @export
skosmos_broader_transitive <- function(concept, lang = NULL, limit = NULL) {
  .concept_call(concept, "broaderTransitive", "broaderTransitive", lang, limit)
}

#' Get direct narrower concepts
#'
#' Retrieves concepts connected to the selected concept through the direct
#' SKOS narrower relationship.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble of direct narrower concepts. An empty tibble is possible for
#'   a leaf concept.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_narrower(concept, lang = "en")
#' }
#' @export
skosmos_narrower <- function(concept, lang = NULL) .concept_call(concept, "narrower", "narrower", lang)

#' Get all transitive narrower concepts
#'
#' Traverses the narrower hierarchy and returns descendants of the selected
#' concept where the Skosmos deployment supports `narrowerTransitive`.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#' @param limit Optional maximum number of returned concepts.
#'
#' @return A tibble of transitive narrower concepts.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_narrower_transitive(concept, lang = "en", limit = 20)
#' }
#' @export
skosmos_narrower_transitive <- function(concept, lang = NULL, limit = NULL) {
  .concept_call(concept, "narrowerTransitive", "narrowerTransitive", lang, limit)
}

#' Get related concepts
#'
#' Retrieves concepts connected through the SKOS related relationship.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble of related concepts. An empty tibble is valid when no
#'   related concepts are defined.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_related(concept, lang = "en")
#' }
#' @export
skosmos_related <- function(concept, lang = NULL) .concept_call(concept, "related", "related", lang)

#' Get a preferred label directly from a concept URI
#'
#' Uses the global Skosmos label endpoint, so the caller does not need to know
#' which vocabulary contains the URI.
#'
#' @param client A `skosmos_client` created with [skosmos_client()].
#' @param uri A single concept URI.
#' @param lang Optional language code for the returned label.
#'
#' @return A tibble containing the preferred label.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' skosmos_label_uri(
#'   finto,
#'   uri = "http://www.yso.fi/onto/yso/p7160",
#'   lang = "en"
#' )
#' }
#' @export
skosmos_label_uri <- function(client, uri, lang = NULL) {
  uri <- .scalar_chr(uri, "uri")
  .get_key(client, "label", list(uri = uri, lang = lang), "prefLabel", value_name = "prefLabel")
}

#' Get direct child concepts
#'
#' Retrieves the child concepts reported by Skosmos for the selected concept.
#' This route can differ semantically from the plain SKOS `narrower` route on
#' some deployments because Skosmos may use it for hierarchy presentation.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble of child concepts.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_children(concept, lang = "en")
#' }
#' @export
skosmos_children <- function(concept, lang = NULL) .concept_call(concept, "children", "children", lang)

#' Get members of a concept group
#'
#' Retrieves concepts that belong to the SKOS concept group represented by the
#' supplied concept handle. Use a group URI returned by [skosmos_groups()] when
#' possible.
#'
#' @param concept A `skosmos_concept` whose URI identifies a concept group.
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble of group members.
#'
#' @examples
#' \dontrun{
#' client <- skosmos_client("https://api.finto.fi/rest/v1/")
#' groups <- skosmos_groups(client, vocid = "yso", lang = "en")
#'
#' # Inspect the group tibble and choose a real group URI returned by the
#' # server. The exact URI column name can vary by deployment.
#' group_uri <- groups$uri[1]
#' group <- skosmos_concept(client, "yso", group_uri)
#' skosmos_group_members(group, lang = "en")
#' }
#' @export
skosmos_group_members <- function(concept, lang = NULL) .concept_call(concept, "groupMembers", "members", lang)

#' Get hierarchical context for a concept
#'
#' Retrieves the hierarchy object generated by Skosmos for the selected concept.
#' Because hierarchy responses are naturally nested, nested values are retained
#' as list-columns rather than being destructively flattened.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble describing the concept's hierarchical context, potentially
#'   containing list-columns for nested branches.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' hierarchy <- skosmos_hierarchy(concept, lang = "en")
#' hierarchy
#' }
#' @export
skosmos_hierarchy <- function(concept, lang = NULL) .concept_call(concept, "hierarchy", "hierarchy", lang)

#' Get mappings for a concept
#'
#' Retrieves mapping relationships associated with a concept, for example SKOS
#' exact, close, broad, narrow, or related mappings when they are exposed by
#' the selected vocabulary.
#'
#' @param concept A `skosmos_concept` created with [skosmos_concept()].
#' @param lang Optional language code for returned labels.
#'
#' @return A tibble of concept mappings. An empty tibble is valid when the
#'   vocabulary does not define mappings for the concept.
#'
#' @examples
#' \dontrun{
#' finto <- skosmos_client("https://api.finto.fi/rest/v1/")
#' concept <- skosmos_concept(
#'   finto, "yso", "http://www.yso.fi/onto/yso/p7160"
#' )
#' skosmos_mappings(concept, lang = "en")
#' }
#' @export
skosmos_mappings <- function(concept, lang = NULL) .concept_call(concept, "mappings", "mappings", lang)
