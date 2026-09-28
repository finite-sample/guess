# Regenerate CITATION.cff from DESCRIPTION and inst/CITATION.
#
# The top-level DOI is the software's; the paper's is the preferred citation.
# Left alone, cffr promotes the paper's DOI and lists the manual entry as a
# reference to itself. Email addresses are dropped: a citation needs none.

drop_email <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- unclass(x)
  if (!is.null(names(x))) {
    x <- x[names(x) != "email"]
  }
  lapply(x, drop_email)
}

cff <- cffr::cff_create(
  dependencies = FALSE,
  keys = list(
    doi = "10.32614/CRAN.package.guess",
    identifiers = NULL,
    references = NULL
  )
)
cff <- cffr::as_cff(drop_email(cff))
cffr::cff_write(cff)
