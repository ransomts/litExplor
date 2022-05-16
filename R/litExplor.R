#' litExplor
#'
#' This file contains the litExplor gadget
#' @importFrom stats setNames

# TODO roxygen documentation
#' @export litExplor
litExplor <- function() {
  # Define UI
  ui <- miniUI::miniPage(
    miniUI::miniTabstripPanel(miniUI::miniTabPanel(
      "Search",

      # Sidebar layout with input and output definitions
      # Input for terms and options
      shiny::fillCol(
        shiny::wellPanel(
          shiny::textInput("terms1", "Term Set", value = "FAA, Part 147"),
          shiny::actionButton("add_term_set", "Add Term Set", class = "btn-success"),
          shiny::wellPanel(
            shiny::tags$h4("Options"),
            shiny::checkboxInput("peer", "Peer Reviewed", value = FALSE),
            shiny::radioButtons(
              "database", "Database: ",
              c(
                "ERIC" = "eric",
                "ProQuest" = "proquest"
              )
            )
          ),
          shiny::actionButton("search", "Search", class = "btn-success"),
          shiny::br(),
        )
      )
    ), miniUI::miniTabPanel(
      "Summary",
      shiny::fillCol(
        shiny::wellPanel(
          shiny::plotOutput("summary_graph"),
          shiny::uiOutput("summary_dropdown")
        )
      )
    ), miniUI::miniTabPanel(
      "Comparison",
      shiny::fillCol(
        shiny::wellPanel(
          shiny::plotOutput("comparison_graph"),
          shiny::uiOutput("compare_dropdown_a"),
          shiny::uiOutput("compare_dropdown_b")
        )
      )
    ))
  )

  server <- function(input, output, session) {
    get_input_terms <- function(input, number_input_boxes) {
      terms_list <- list()

      sapply(1:number_input_boxes, function(x) {
        terms_list[[length(terms_list) + 1]] <<- input[[paste0("terms", x)]]
      })

      return(terms_list %>% purrr::map(function(x) {
        stringr::str_replace_all(x, ",", " OR ") %>% stringr::str_squish
      }))
    }

    add_term_set <- function(number_input_boxes, value = "") {
      input_box <- shiny::textInput(paste0("terms", number_input_boxes + 1), "", value = value)

      shiny::insertUI(
        selector = paste0("#terms", number_input_boxes),
        where = "afterEnd",
        ui = input_box
      )

      return(number_input_boxes + 1)
    }

    explor <- NULL

    number_input_boxes <- 1

    number_input_boxes <- add_term_set(number_input_boxes, value = "Pandemic, Covid-19")

    shiny::observeEvent(input$add_term_set, {
      number_input_boxes <<- add_term_set(number_input_boxes)
    })


    shiny::observeEvent(input$summary_dropdown, {
      output$summary_graph <- shiny::renderPlot(create_summary(explor, input$summary_dropdown))
    })

    shiny::observeEvent(input$compare_dropdown_a, {
      terms_list <- get_input_terms(input, number_input_boxes)


      terms_trimmed <- terms_list[purrr::map(terms_list, function(x) {
        all(input$compare_dropdown_a != x)
      }) == TRUE]

      output$compare_dropdown_b <- shiny::renderUI({
        shiny::selectInput(
          "compare_dropdown_b",
          "",
          c(terms_trimmed)
        )
      })
    })

    shiny::observeEvent(input$compare_dropdown_b, {
      output$comparison_graph <- shiny::renderPlot(create_heatmap(explor,
                                                                  sa = input$compare_dropdown_a,
                                                                  sb = input$compare_dropdown_b))
    })

    shiny::observeEvent(input$search, {
      print('inside search observer')
      terms_list <- get_input_terms(input, number_input_boxes)

      print(terms_list)
      output$summary_dropdown <- shiny::renderUI({
        shiny::selectInput(
          "summary_dropdown",
          "",
          c(terms_list)
        )
      })

      print('2')

      if (length(terms_list) < 2) {
        output$compare_dropdown_a <- shiny::renderUI({
          shiny::verbatimTextOutput("compare_dropdown_message")
        })
        output$compare_dropdown_message <- shiny::renderText("Must more than one set to use comparison graph")
      } else {
        output$compare_dropdown_a <- shiny::renderUI({
          shiny::selectInput(
            "compare_dropdown_a",
            "",
            c(terms_list)
          )
        })
      }

      sets <- purrr::map(terms_list, make_set)

      if (length(sets) < 2) {
        group <- set_to_group(sets[[1]])
      } else {
        group <- set_to_group(sets[[1]], sets[[2:length(sets)]])
      }

      group %<>% make_queries_for_group

      explor <<- group_to_explor(group)

      explor <<- dplyr::mutate(explor, eric = get_eric_count(query)) %>% tidyr::unnest(eric)

      # output$comparison_graph <- renderPlot({
      #   create_compare_graph(explor)
      # })
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("explor"))
}
