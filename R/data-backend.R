#  This file provides the model for combining search term sets into
#  a tibble for visualization.


#' make_set
# TODO figure out if examples are needed for non exported functions
#' @param input_terms a string in the form "A OR B OR C"
#' @return A tibble with column names from \code{input_terms} representing search terms OR'd together
#' #make_set("A OR B")
#' #make_set("E OR F OR G")
#' #make_set("M OR N OR O OR P")s
make_set <- function(input_terms) {
  if (input_terms == "") stop("input_terms must not be empty")

  term_list <- unlist(stringr::str_split(input_terms, " OR "))

  # TODO debug duplicate detection
  # if (term_list %>% table %>% tibble::enframe %>% dplyr::filter(value > 1) %>% nrow > 1) {
  #   stop("duplicate input term detected: %s",
  #        term_list %>%
  #          table %>%
  #          tibble::enframe %>%
  #          dplyr::filter(value > 1) %>%
  #          dplyr::pull(name))
  # }

  tib <- dplyr::bind_rows(rlang::set_names(
    rep(FALSE, length(term_list)), term_list
  )) %>%
    dplyr::slice(0)
  return(tib)
}

#' set_to_group
#' @param set a set as output from \code{\link{make_set}}
#'
#' @param ... additional sets
#'
#' @return a group tibble, representing search term sets which are AND'd together
#' set_to_group(make_set("A OR B"))
#' set_to_group(make_set("A OR B"), make_set("C OR D"))
set_to_group <- function(set, ...) {

  #' generate_name_from_set
  #' @param set a list of term strings
  #' @return string with the terms deliminated by an underscore
  generate_name_from_set <- function(set) {
    return(set %>% names() %>% paste0(collapse = "_"))
  }

  #' nth_bit
  #' @param i an integer
  #' @param n the index of the bit to retreive
  #' @return the nth bit of an integer
  nth_bit <- function(i, n) {
    bitwAnd(bitwShiftR(i, n - 1), 1)
  }

  tib <- tibble::tibble(set, ...)
  tib_len <- length(tib)

  for (i in 2:2^tib_len - 1) {
    tib %<>% tibble::add_row(!!!setNames(purrr::map(1:tib_len, ~ nth_bit(i, .)), names(.)))
  }

  inital_set_names <- set %>% generate_name_from_set()
  tib %<>% tidyr::nest("{inital_set_names}" := names(set))

  for (extra_set in list(...)) {
    extra_set_names <- extra_set %>% generate_name_from_set()
    tib %<>% tidyr::nest("{extra_set_names}" := names(extra_set))
  }

  return(tib)
}

#' add_queries_to_group
#' @param group a group as returned by the \code{\link{set_to_gorup}} function
#' @return a tibble with binary expansions of terms and English queries of them
#' #make_queries_for_group(set_to_group(make_set("A OR B")))
#' #make_queries_for_group(set_to_group(make_set("A OR B"), make_set("C OR D")))
add_queries_to_group <- function(group) {

  #' combine_augmented_sets
  #'
  #' An augmented set is a term set tibble with queries. In order to combine
  #' them, we need a cartesian product on the term binaries but the queries are
  #' combined with an " AND " between them.
  #'
  #' @param set_a Name of first set to combine
  #' @param set_b Name of second set to combine
  #' @return a tibble with the binary expansions of the sets present and queries combined
  combine_augmented_sets <- function(set_a, set_b) {
    combined_acc <- dplyr::bind_cols(
      set_a %>% dplyr::slice(0) %>% dplyr::select(-query),
      set_b %>% dplyr::slice(0)
    )

    for (a_slice_index in 1:nrow(set_a)) {
      a_slice <- set_a %>% dplyr::slice(a_slice_index)

      for (b_slice_index in 1:nrow(set_b)) {
        b_slice <- set_b %>% dplyr::slice(b_slice_index)

        a_query <- dplyr::select(a_slice, query)
        b_query <- dplyr::select(b_slice, query)

        combined_query <- paste(dplyr::select(a_slice, query),
          dplyr::select(b_slice, query),
          sep = " AND "
        )

        if (a_query == "" || b_query == "") {
          combined_query <- paste0(
            dplyr::select(a_slice, query),
            dplyr::select(b_slice, query)
          )
        }

        a_terms <- a_slice %>% dplyr::select(-query)
        b_terms <- b_slice %>% dplyr::select(-query)


        combined_acc %<>% tibble::add_row(dplyr::bind_cols(a_terms,
          b_terms,
          query = combined_query
        ))
      }
    }

    return(combined_acc)
  }

  #' make_queries_for_set
  #'
  #' @param set
  #'
  #' @return
  make_queries_for_set <- function(set) {

    #' make_query
    #'
    #' @param ... a number of binary variables from the columns of an explor tibble
    #' @return A string representing the terms present in the provided columns
    make_query <- function(...) {
      tmp_names <- names(list(...))
      new_names <- c()
      for (i in 1:...length()) {
        if (...elt(i) == 1) {
          new_names %<>% append(tmp_names[i])
        }
      }

      if (length(new_names) > 1) {
        new_names %<>% paste(collapse = " OR ")
      } else if (length(new_names) == 0) {
        new_names <- ""
      }
      return(new_names)
    }

    return(purrr::pmap_chr(set, .f = make_query))
  }

  set_names <- group %>% names()

  augmented_set_tibs <- c()
  for (set_name in set_names) {
    set_tib <- group %>%
      dplyr::select(set_name) %>%
      tidyr::unnest(cols = c(set_name)) %>%
      unique()

    augmented_set_tib <- set_tib %>%
      tibble::add_column("query" := make_queries_for_set(set_tib))

    augmented_set_tibs %<>% append(list(augmented_set_tib))
  }

  aug_query_tib_acc <- augmented_set_tibs[1] %>% purrr::pluck(1)
  if (length(augmented_set_tibs) >= 2) {
    for (i in 2:length(augmented_set_tibs)) {
      aug_query_tib_acc %<>% combine_augmented_sets(augmented_set_tibs[i] %>% purrr::pluck(1))
    }
  }

  return(aug_query_tib_acc)
}

#' group_to_explor
#' @param group_with_queries a group as created by the add_queries_to_group function
#' @return a fully expanded explor tibble without paper counts
#' group_to_explor(add_queries_to_group(set_to_group(make_set("A OR B"))))
#' group_to_explor(add_queries_to_group(set_to_group(make_set("A OR B"), make_set("C OR D"))))
group_to_explor <- function(group_with_queries) {
  return(group_with_queries %>% tibble::add_column(eric = NA, proquest = NA))
}

#' An example explor tibble
#'
#' A dataset containing a fully formed term search set,
#' English translations, and eric article counts
#'
#' @format A tibble with four search terms, queries, eric counts, and proquest counts. NA represents data has not been collected
#' \describe{
#'    \item{A}{search term "A"}
#'    \item{B}{search term "B"}
#'    \item{C}{search term "C"}
#'    \item{D}{search term "D"}
#'    \item{query}{English translation of search query}
#'    \item{eric}{Number of matching articles in ERIC}
#'    \item{proquest}{Number of matching articles in Proquest}
#' }
"example_explor_b"

