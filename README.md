# SkosmosR

`SkosmosR` is a general R client for standards-compatible **Skosmos REST API v1** deployments.
It is not tied to Finto: users supply the REST v1 base URL for any Skosmos installation.

High-level JSON functions return **tibbles**. Nested Skosmos structures are preserved as list-columns. RDF helpers return RDF text/raw data or `rdflib` graphs, while `skosmos_request()` gives direct access to the original JSON payload for deployment-specific routes.

```r
library(SkosmosR)

finto <- skosmos_client("https://api.finto.fi/rest/v1/")

skosmos_vocabularies(finto, "en")
skosmos_search(finto, "forest", vocabs = "yso", lang = "en", maxhits = 10)

concept <- skosmos_concept(
  finto,
  "yso",
  "http://www.yso.fi/onto/yso/p7160"
)

skosmos_label(concept, "en")
skosmos_broader(concept, "en")
skosmos_narrower(concept, "en")
```

## Public compatibility examples

```r
skosmos_instances()
```

The bundled convenience registry currently includes examples from Finto, UNESCO Vocabulary Services, FAO AGROVOC, NFFA-Europe, MET Norway, ILC4CLARIN, CLARIN CLAVAS, Loterre, ACTRIS, University of Oslo, ZPID and University of Bologna /DH.arc. Public deployments may move or change, so the registry is not an exhaustive census.

See `vignette("getting-started", package = "SkosmosR")` for examples covering the full high-level API surface and the public services discussed during development.
