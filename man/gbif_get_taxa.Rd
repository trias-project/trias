% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbif_get_taxa.R
\name{gbif_get_taxa}
\alias{gbif_get_taxa}
\title{Get taxa information from GBIF}
\usage{
gbif_get_taxa(
  taxon_keys = NULL,
  checklist_keys = NULL,
  origin = NULL,
  limit = NULL
)
}
\arguments{
\item{taxon_keys}{(single numeric or character or a vector) a single key or a
vector of keys. Not to use together with \code{checklist_keys}.}

\item{checklist_keys}{(single character or a vector) a datasetKey (character)
or a vector of datasetkeys. Not to use together with \code{checklist_keys}.}

\item{origin}{(single character or a vector) filter by origin.
It can take many inputs, and treated as OR (e.g., a or b or c)
To be used only in combination with \code{checklist_keys}. Ignored otherwise.}

\item{limit}{With taxon_keys: limit number of taxa.
With checklist_keys: limit number of taxa per each dataset.
A warning is given if limit is higher than the length of taxon_keys or number of records
in the checklist_keys (if string) or any of the checklist_keys (if vector)}
}
\value{
A data.frame with all returned attributes for any taxa
}
\description{
This function retrieves taxa information from GBIF. It is a higher level function built
on rgbif functions \code{name_usage()} and \code{name_lookup()}.
}
\examples{
\dontrun{
# A single numeric taxon_keys
gbif_get_taxa(taxon_keys = 1)
# A single character taxon_keys
gbif_get_taxa(taxon_keys = "1")
# Multiple numeric taxon_keys (vector)
gbif_get_taxa(taxon_keys = c(1, 2, 3, 4, 5, 6))
# Multiple character taxon_keys (vector)
gbif_get_taxa(taxon_keys = c("1", "2", "3", "4", "5", "6"))
# Limit number of taxa (coupled with taxon_keys)
gbif_get_taxa(taxon_keys = c(1, 2, 3, 4, 5, 6), limit = 3)
# A single checklist_keys (character)
gbif_get_taxa(checklist_keys = "b3fa7329-a002-4243-a7a7-cd066092c9a6")
# Multiple checklist_keys (vector)
gbif_get_taxa(checklist_keys = c(
  "e4746398-f7c4-47a1-a474-ae80a4f18e92",
  "b3fa7329-a002-4243-a7a7-cd066092c9a6"
))
# Limit number of taxa (coupled with checklist_keys)
gbif_get_taxa(
  checklist_keys = c(
    "e4746398-f7c4-47a1-a474-ae80a4f18e92",
    "b3fa7329-a002-4243-a7a7-cd066092c9a6"
  ),
  limit = 30
)
# Filter by origin
gbif_get_taxa(
  checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
  origin = "source", limit = 3000
)
gbif_get_taxa(
  checklist_keys = "9ff7d317-609b-4c08-bd86-3bc404b77c42",
  origin = c("source", "denormed_classification"), limit = 3000
)
}
}
