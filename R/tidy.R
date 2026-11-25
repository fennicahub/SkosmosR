# Internal conversion helpers -------------------------------------------------

# Convert Skosmos JSON payload fragments to tibbles while preserving nested
# structures as list-columns. Low-level `skosmos_request()` intentionally does
# not use this helper so advanced users can still access the original payload.
.as_skosmos_tibble <- function(x, value_name = "value") {
  if (inherits(x, "tbl_df")) return(x)
  if (is.data.frame(x)) return(tibble::as_tibble(x))
  if (is.null(x)) return(tibble::tibble())

  if (is.atomic(x)) {
    if (length(x) == 0L) return(tibble::tibble())
    if (!is.null(names(x)) && all(nzchar(names(x)))) {
      vals <- lapply(x, identity)
      return(tibble::as_tibble_row(vals))
    }
    out <- list(x)
    names(out) <- value_name
    return(tibble::as_tibble(out))
  }

  if (is.list(x)) {
    # A JSON array of objects often simplifies to a list of named lists.
    if (length(x) > 0L && is.null(names(x)) && all(vapply(x, is.list, logical(1)))) {
      rows <- lapply(x, .list_to_row)
      return(.bind_rows_base(rows))
    }

    # Named JSON object -> one-row tibble, preserving non-scalar values as
    # list-columns.
    if (!is.null(names(x))) {
      return(tibble::as_tibble_row(.list_to_row(x)))
    }

    out <- list(x)
    names(out) <- value_name
    return(tibble::as_tibble(out))
  }

  out <- list(x)
  names(out) <- value_name
  tibble::as_tibble(out)
}

.list_to_row <- function(x) {
  if (is.null(names(x))) {
    return(list(value = x))
  }
  out <- lapply(x, function(v) {
    if (is.null(v)) return(NA)
    if (is.atomic(v) && length(v) <= 1L) return(v)
    list(v)
  })
  names(out) <- names(x)
  out
}

.bind_rows_base <- function(rows) {
  if (!length(rows)) return(tibble::tibble())
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  normalized <- lapply(rows, function(r) {
    missing <- setdiff(all_names, names(r))
    for (nm in missing) r[[nm]] <- NA
    r[all_names]
  })
  cols <- setNames(vector("list", length(all_names)), all_names)
  for (nm in all_names) {
    vals <- lapply(normalized, `[[`, nm)
    scalar <- all(vapply(vals, function(v) !is.list(v) && length(v) <= 1L, logical(1)))
    cols[[nm]] <- if (scalar) unlist(vals, recursive = FALSE, use.names = FALSE) else vals
  }
  tibble::as_tibble(cols)
}

.scalar_chr <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be exactly one non-empty character string.", call. = FALSE)
  }
  x
}
