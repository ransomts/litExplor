#' An example explor tibble
#'
#' A complete dataset for a search of the ERIC database for the following search term sets:
#'   - math OR science OR engineering OR computing
#'   - online education OR virtual learning OR distance education
#'   - two year institution OR community college
#'
#' @format A tibble with 511 rows and 11 colums:
#' \describe{
#'   \item{math}{search term}
#'   \item{science}{search term}
#'   \item{engineering}{search term}
#'   \item{computing}{search term}
#'   \item{online education}{search term}
#'   \item{virtual learning}{search term}
#'   \item{distance education}{search term}
#'   \item{two year institution}{search term}
#'   \item{community college}{search term}
#'   \item{query}{the english query translations}
#'   \item{eric}{the counts from ERIC}
#'   \item{total_count}{total counts across all databases (here only ERIC)}
#' }
#' @source Data scraped from the ERIC database
"explor"
