# https://cengel.github.io/gearup2016/SULdataAccess.html

#' get_count
#' @param database a string indicating which database to query
#' @param query a query string, eg A OR B AND C
#' @return an integer for the count of papers from a database
get_count <- function(database, query) {
  return(switch(database,
    "eric" = get_eric_count(query),
    "jstor" = get_jstor_count(query),
    "proquest" = get_proquest_count(query),
    "arxiv" = get_arxiv_count(query),
    "asee_peer" = get_asee_peer_count(query),
    "google_scholar" = get_google_scholar_count(query),
    "ebsco" = get_ebsco_count(query),
    "plos" = get_plos_count(query)
  ))
}

#' get_eric_count
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles eric has for the search
get_eric_count <- function(str) {
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

    message(paste("Searching ERIC for: ", str))
    return(utils::URLencode(unencoded_url)) # urltools::url_encode broke api compatibility, using utils::URLencode instead
  }

  eric_url <- create_eric_url(str)

  # variable left out for future caching opportunity
  req <- curl::curl_fetch_memory(eric_url)
  json_resp <- rjson::fromJSON(rawToChar(req$content))
  return(json_resp$response$numFound)
}
get_eric_count <- Vectorize(get_eric_count)


# TODO implement jstor communication
#' get_jstor_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles jstor has for the search
get_jstor_count <- function(str) {
  create_jstor_url <- function(str) {  }

  testthat::skip("jstor communication not implemented")
  return(0)
}
get_jstor_count <- Vectorize(get_jstor_count)

# TODO implement proquest communication
#' get_proquest_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles proquest has for the search
get_proquest_count <- function(terms, database = "medlineprof", authorization = "Bearer 8461684a-5a79-407c-9321-93d8d719ddeb") {
  formatted_terms <- paste(mapply(function(x) {
    paste(unlist(x), collapse = " OR ")
  }, terms), collapse = " AND ")

  create_proquest_data <- function(terms, database) {
    message(paste("Searching ProQuest for: ", terms))

    return(
      paste0(
        '<searchRequest xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"/searchapi_v1.xsd\">
    	<search>
    		<query>', terms, "</query>
    		<databases>
    			<database>", database, "</database>
    		</databases>
    	</search>
      </searchRequest>"
      )
    )
  }

  proquest_base_url <- "https://api-dialog.proquest.com/v1/search"

  headers <- c(
    `Authorization` = "Bearer 8461684a-5a79-407c-9321-93d8d719ddeb",
    `Content-Type` = "text/xml"
  )

  data <- create_proquest_data(formatted_terms, database)

  response <- httr::POST(url = proquest_base_url, httr::add_headers(.headers = headers), body = data)

  return(as.double(httr::content(response)$searchResponse$result$docsFound))
}
get_proquest_count <- Vectorize(get_proquest_count)

#' get_arxiv_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles arxiv has for the search
get_arxiv_count <- function(str) {

  # create_arxiv_url <- function(str) {  }

  # testthat::skip("arXiv communication not implemented")
  return(as.double(aRxiv::arxiv_count(str)))
}
get_arxiv_count <- Vectorize(get_arxiv_count)

# TODO implement asee_peer communication
#' get_asee_peer_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles asee has for the search
get_asee_peer_count <- function(str) {
  create_asee_peer_url <- function(str) {  }

  testthat::skip("asee_peer communication not implemented")
  return(0)
}
get_asee_peer_count <- Vectorize(get_asee_peer_count)

# TODO implement google_scholar communication
#' get_google_scholar_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles google has for the search
get_google_scholar_count <- function(str) {
  create_google_scholar_url <- function(str) {  }

  testthat::skip("google_scholar communication not implemented")
  return(0)
}
get_google_scholar_count <- Vectorize(get_google_scholar_count)

# TODO implement ebsco communication
#' get_ebsco_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles ebsco has for the search
get_ebsco_count <- function(str) {
  create_ebsco_url <- function(str) {  }

  testthat::skip("ebsco communication not implemented")
  return(0)
}
get_ebsco_count <- Vectorize(get_ebsco_count)

# TODO implement plos communication
#' get_plos_count
#'
#' @param str terms to search for
#'
#' @return an integer with the corresponding number of articles plos has for the search
get_plos_count <- function(str) {

  # https://github.com/ropensci/rplos
  testthat::skip("plos communication not implemented")
  return(0)
}
get_plos_count <- Vectorize(get_plos_count)
