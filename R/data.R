#' An example explor tibble
#'
#' A complete dataset for a search of the ERIC database for the following search term sets:
#'   - math OR science OR engineering OR computing
#'   - online education OR virtual learning OR distance education
#'   - two year institution OR community college
#'
#' @format A tibble with 511 rows and 11 colums:
#' \describe{
#' \item{math}
#' \item{science}
#' \item{engineering}
#' \item{computing}
#' \item{online education}
#' \item{virtual learning}
#' \item{distance education}
#' \item{two year institution}
#' \item{community college} the search terms
#' \item{query} the english query translations
#' \item{eric} the counts from ERIC
#' \item{total_count} total counts across all databases (here only ERIC)
#' }
#' @source Data scraped from the ERIC database
"explor"
