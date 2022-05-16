# TODO roxygen documentation
#' Title
#'
#' @param binary
#' @param sym
#'
#' @return
translate_binary_to_label <- function(binary, sym) {
  sym_holder <- sym
  present_vars <- c()
  binary_vars <- stringr::str_split(binary, pattern = " ", simplify = TRUE)

  for (i in 1:length(binary_vars)) {
    if (binary_vars[i] == "1") {
      present_vars %<>% append(sym[i])
    }
  }

  return(paste0(present_vars, collapse = " OR "))
}
translate_binary_to_label <- Vectorize(translate_binary_to_label)

# TODO roxygen documentation
#' Title
#'
#' @param explor
#' @param sa
#' @param sb
#'
#' @return
create_heatmap <- function(explor, sa, sb) {
  sa %<>% stringr::str_split(" OR ", simplify = TRUE)
  sb %<>% stringr::str_split(" OR ", simplify = TRUE)

  set_a_syms <- purrr::map(sa, rlang::sym)
  set_b_syms <- purrr::map(sb, rlang::sym)

  # find the names of terms we're not using
  other_names <- explor %>%
    dplyr::select(
      -as.vector(sa),
      -as.vector(sb),
      -eric, -query, -proquest
    ) %>%
    names()

  if (length(other_names) > 1) {
    # filter to only where the terms we're not using are 0
    explor %<>%
      dplyr::filter_at(vars(other_names), all_vars(. == 0)) %>%
      dplyr::select(-other_names)
  }

  # process as if we only have two terms
  heat_data <- explor %>%
    dplyr::summarize(set_a = paste(!!!set_a_syms), set_b = paste(!!!set_b_syms), eric) %>%
    dplyr::mutate(
      set_a = translate_binary_to_label(set_a, list(sa)),
      set_b = translate_binary_to_label(set_b, list(sb))
    )

  # make the heatmap
  heat_plot <- heat_data %>%
    ggplot2::ggplot(ggplot2::aes(set_a, set_b, fill = eric)) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 15, vjust = 0.5, angle = 45),
      axis.text.y = ggplot2::element_text(size = 15)
    )

  return(heat_plot)
}

# TODO roxygen documentation
#' Title
#'
#' @param explor
#' @param sa
#'
#' @return
create_summary <- function(explor, sa) {
  sa %<>% stringr::str_split(" OR ", simplify = TRUE)

  set_a_syms <- purrr::map(sa, rlang::sym)

  # find the names of terms we're not using
  other_names <- explor %>%
    dplyr::select(
      -as.vector(sa),
      -eric, -query, -proquest
    ) %>%
    names()

  # filter to only where the terms we're not using are 0
  explor %<>%
    dplyr::filter_at(vars(other_names), all_vars(. == 0)) %>%
    dplyr::select(-other_names)

  # process as if we only have two terms
  data <- explor %>%
    dplyr::summarize(set_a = paste(!!!set_a_syms), eric) %>%
    dplyr::mutate(set_a = translate_binary_to_label(set_a, list(sa)))

  # make the summary
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = set_a, y = eric)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_log10(
      n.breaks = 10,
      labels = scales::label_number()
    ) +
    ggplot2::labs(x = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 0))

  return(plot)
}
