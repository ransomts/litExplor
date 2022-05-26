search_query <- "computing"

test_that("get_count correctly switches between count functions", {
  expect_equal(get_count("eric", search_query), get_eric_count(search_query))
    expect_equal(get_count("jstor", search_query), get_jstor_count(query))
    expect_equal(get_count("proquest", search_query), get_proquest_count(query))
    expect_equal(get_count("arxiv", search_query), get_arxiv_count(query))
    expect_equal(get_count("asee_peer", search_query), get_asee_peer_count(query))
    expect_equal(get_count("google_scholar", search_query), get_google_scholar_count(query))
    expect_equal(get_count("ebsco", search_query), get_ebsco_count(query))
    expect_equal(get_count("plos", search_query), get_plos_count(query))
})

test_that("get_eric_count returns double", {
  expect_type(get_eric_count(search_query), "double")
})

test_that("get_eric_count returns positive double", {
  count <- get_eric_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_jstor_count returns positive double", {
  count <- get_jstor_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_proquest_count returns positive double", {
  count <- get_proquest_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_arxiv_count returns positive double", {
  count <- get_arxiv_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_asee_peer_count returns positive double", {
  count <- get_asee_peer_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_google_scholar_count returns positive double", {
  count <- get_google_scholar_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_ebsco_count returns positive double", {
  count <- get_ebsco_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})

test_that("get_plos_count returns positive double", {
  count <- get_plos_count(search_query)
  expect_type(count, "double")
  expect_gte(count, 0)
})
