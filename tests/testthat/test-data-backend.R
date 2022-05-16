test_that("make_set correctly sets column names", {
  expect_equal(make_set("A OR B") %>% names(), c("A", "B"))
})

test_that("make_set does not take empty string", {
  expect_error(make_set(""))
})

# test_that("make_set does not work with duplicates", {
#   expect_error(make_set("A OR A OR B"))
# })
