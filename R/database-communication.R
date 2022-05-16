# TODO fill in param descriptions
#' create_eric_url
#' create_eric_url
#' @param search_terms
#'
#' @param start
#' @param rows
#' @param fields
#'
#' @return string of html encoded eric query
create_eric_url <- function(search_terms, start = 0, rows = 0,
                            fields = list(list("*"), list("peerreviewed", "'T'"))) {

  # unlikely to change, we only care about ERIC at the moment
  eric_base_url <- "https://api.ies.ed.gov/eric/"
  # we've selected json over xml for parsing and efficiency purposes
  format <- "json"

  # two one liners to form our search term logical expression and the field modification encoding
  formatted_terms <- paste(mapply(function(x) {
    paste(unlist(x), collapse = " OR ")
  }, search_terms), collapse = " AND ")
  formatted_fields <- paste(mapply(function(x) {
    paste(unlist(x), collapse = ":")
  }, fields), collapse = " ")

  unencoded_url <- paste(eric_base_url,
    "?search=", formatted_terms,
    "&format=", format,
    "&start=", start,
    "&rows=", rows,
    "&fields=", formatted_fields,
    sep = ""
  )

  return(utils::URLencode(unencoded_url)) # urltools::url_encode broke api compatibility, using utils::URLencode instead
}

# TODO fill in roxygen
#' get_eric_count
#' @param str
#'
#' @return an integer with the corresponding number of articles eric has for the search
get_eric_count <- function(str) {
  eric_url <- create_eric_url(str)

  # variable left out for future caching opportunity
  req <- curl::curl_fetch_memory(eric_url)
  json_resp <- rjson::fromJSON(rawToChar(req$content))
  return(json_resp$response$numFound)
}
get_eric_count <- Vectorize(get_eric_count)

# TODO implement proquest communication
#' create_proquest_url
#'
#' @param str
#'
#' @return
create_proquest_url <- function(str) {
  testthat::skip("Proquest url creation not implemented")
}

# TODO implement proquest communication
#' get_proquest_count
#'
#' @param str
#'
#' @return
get_proquest_count <- function(str) {
  testthat::skip("Proquest communication not implemented")
}
