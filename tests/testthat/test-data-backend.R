test_terms_a <- "A"
test_terms_b <- "A OR B"
test_terms_c <- "A OR B OR C"
test_terms_d <- "B OR C"
test_terms_e <- "D OR E OR F"

test_that("make_set correctly sets column names", {
  expect_equal(make_set(test_terms_b) %>% names(), c("A", "B"))
})

test_that("make_set does not take empty string", {
  expect_error(make_set(""))
})

test_that("make_set tosses a warning and removes duplicates", {
  expect_warning(make_set("A OR A OR B"))
})

test_that("set_to_group works with single set", {
  expect_silent(set_to_group(make_set(test_terms_b)))
})

test_that("set_to_group works with multiple sets", {
  expect_silent(set_to_group(
    make_set(test_terms_a),
    make_set(test_terms_d),
    make_set(test_terms_e)
  ))
})

test_that("queries can be added to group tibble", {
  expect_equal(
    group_to_explor(set_to_group(
      make_set(test_terms_a),
      make_set(test_terms_d),
      make_set(test_terms_e)
    )) %>% dplyr::pull(query),
    c(
      "A", "A AND D", "A AND E",
      "A AND D OR E", "A AND F", "A AND D OR F",
      "A AND E OR F", "A AND D OR E OR F", "A AND B",
      "A AND B AND D", "A AND B AND E", "A AND B AND D OR E",
      "A AND B AND F", "A AND B AND D OR F", "A AND B AND E OR F",
      "A AND B AND D OR E OR F", "A AND C", "A AND C AND D",
      "A AND C AND E", "A AND C AND D OR E", "A AND C AND F",
      "A AND C AND D OR F", "A AND C AND E OR F", "A AND C AND D OR E OR F",
      "A AND B OR C", "A AND B OR C AND D", "A AND B OR C AND E",
      "A AND B OR C AND D OR E", "A AND B OR C AND F", "A AND B OR C AND D OR F",
      "A AND B OR C AND E OR F", "A AND B OR C AND D OR E OR F", "",
      "D", "E", "D OR E",
      "F", "D OR F", "E OR F",
      "D OR E OR F", "B", "B AND D",
      "B AND E", "B AND D OR E", "B AND F",
      "B AND D OR F", "B AND E OR F", "B AND D OR E OR F",
      "C", "C AND D", "C AND E",
      "C AND D OR E", "C AND F", "C AND D OR F",
      "C AND E OR F", "C AND D OR E OR F", "B OR C",
      "B OR C AND D", "B OR C AND E", "B OR C AND D OR E",
      "B OR C AND F", "B OR C AND D OR F", "B OR C AND E OR F",
      "B OR C AND D OR E OR F"
    )
  )
})
