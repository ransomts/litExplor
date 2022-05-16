test_that("eric url creation case 1", {
  test_url_a <- "https://api.ies.ed.gov/eric/?search=%28%22foo%22%29&format=json&start=0&rows=20000&fields=%2A%20peerreviewed%3A%22T%22"
  test_url_b <- "https://api.ies.ed.gov/eric/?search=%28%22foo%22%20OR%20%22bar%22%29&format=json&start=0&rows=20000&fields=%2A%20peerreviewed%3A%22T%22"
  test_url_c <- "https://api.ies.ed.gov/eric/?search=%28%22foo%22%20OR%20%22bar%22%29%20AND%20%28%22foobie%22%29&format=json&start=0&rows=20000&fields=%2A%20peerreviewed%3A%22T%22"
  test_url_d <- "https://api.ies.ed.gov/eric/?search=%28%22foo%22%20OR%20%22bar%22%29%20AND%20%28%22foobie%22%20OR%20%22doobie%22%29&format=json&start=0&rows=20000&fields=%2A%20peerreviewed%3A%22T%22"
  test_url_e <- "https://api.ies.ed.gov/eric/?search=%28%22foo%20bar%22%29%20AND%20%28%22foobie%22%20OR%20%22doobie%22%29&format=json&start=0&rows=20000&fields=%2A%20peerreviewed%3A%22T%22"

  # expect_equal(create_eric_url(test_terms_a), test_url_a)
  # expect_equal(create_eric_url(test_terms_b), test_url_b)
  # expect_equal(create_eric_url(test_terms_c), test_url_c)
  # expect_equal(create_eric_url(test_terms_d), test_url_d)
  # expect_equal(create_eric_url(test_terms_e), test_url_e)
})
