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
        style = "overflow-y:scroll; max-height: 600px",
        shiny::wellPanel(
          shiny::textInput("terms1", "Term Sets", value = "math, computing, science, engineering"),
          shiny::actionButton("add_term_set", "Add Term Set", class = "btn-success"),
          shiny::actionButton("remove_term_set", "Remove Term Set", class = "btn-danger"),
          shiny::wellPanel(
            shiny::checkboxGroupInput("search_options", "Options:",
              choices = c("Peer Reviewed")
            ),
            shiny::numericInput("start_year", "Start year: ",
              value = 1970, step = 1
            ),
            shiny::numericInput("end_year", "End year: ",
              value = 2022, step = 1
            ),
            shiny::checkboxGroupInput(
              "databases", "Database:",
              choices = c(
                "ERIC" = "eric",
                "ProQuest" = "proquest",
                "arXiv" = "arxiv",
                "JSTOR" = "jstor",
                "ASEE PEER" = "asee_peer",
                "PLOS" = "plos"
              ), inline = TRUE
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
          plotly::plotlyOutput("summary_graph"),
          shiny::uiOutput("summary_dropdown")
        )
      )
    ), miniUI::miniTabPanel(
      "Comparison",
      shiny::fillCol(
        shiny::wellPanel(
          plotly::plotlyOutput("comparison_graph", height = "100%"),
          shiny::uiOutput("compare_dropdown_a"),
          shiny::uiOutput("compare_dropdown_b")
        )
      )
    ), miniUI::miniTabPanel(
      "Data",
      shiny::fillCol(
        shiny::wellPanel(
          DT::DTOutput('explor_table')
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
        stringr::str_replace_all(x, ",", " OR ") %>% stringr::str_squish()
      }))
    }

    add_term_set <- function(number_input_boxes, value = "") {
      shiny::insertUI(
        selector = paste0("#terms", number_input_boxes),
        where = "afterEnd",
        ui = shiny::textInput(paste0("terms", number_input_boxes + 1), "", value = value)
      )

      return(number_input_boxes + 1)
    }

    remove_term_set <- function(number_input_boxes) {
      if (number_input_boxes <= 1) {
        return(number_input_boxes)
      }
      shiny::removeUI(
        selector = paste0("#terms", number_input_boxes)
      )
      return(number_input_boxes - 1)
    }

    explor <- NULL

    number_input_boxes <- 1

    number_input_boxes <- add_term_set(number_input_boxes, value = "online learning, virtual education")

    shiny::observeEvent(input$add_term_set, {
      number_input_boxes <<- add_term_set(number_input_boxes)
    })

    shiny::observeEvent(input$remove_term_set, {
      number_input_boxes <<- remove_term_set(number_input_boxes)
    })

    shiny::observeEvent(input$summary_dropdown, {
      output$summary_graph <- plotly::renderPlotly(create_summary(explor, input$summary_dropdown))
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
      output$comparison_graph <- plotly::renderPlotly(create_heatmap(explor,
        sa = input$compare_dropdown_a,
        sb = input$compare_dropdown_b
      ))
    })

    shiny::observeEvent(input$search, {
      terms_list <- get_input_terms(input, number_input_boxes)

      output$summary_dropdown <- shiny::renderUI({
        shiny::selectInput(
          "summary_dropdown",
          "",
          c(terms_list)
        )
      })

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

      explor <<- group_to_explor(group)

      for (database in input$databases) {
        message(paste("Searching database: ", database))
        explor <<- dplyr::mutate(explor, "{database}" := get_count(database, query)) %>%
          tidyr::unnest(rlang::sym(database))
      }
      explor <<- dplyr::mutate(explor, total_count = rowSums(dplyr::across(input$databases)))
      # output$comparison_graph <- renderPlot({
      #   create_compare_graph(explor)
      # })
      output$explor_table <- DT::renderDataTable(explor)
    })
  }

  shiny::runGadget(ui, server)
}
