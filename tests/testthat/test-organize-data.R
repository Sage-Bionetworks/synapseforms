context("organize-data.R")

test_that("make_tidier_table returns tidier table", {
  data <- data.frame(
    variables = c("s1.a", "s1.b.1", "s2.a.1", "s2.b.1.apple"),
    sub1 = c(NA, "yes", "no", "jane"),
    sub2 = c("jimothy", "pb", NA, NA),
    stringsAsFactors = FALSE
  )
  tidy_data <- data.frame(
    form_data_id = c(
      "sub1", "sub1", "sub1", "sub1",
      "sub2", "sub2", "sub2", "sub2"
    ),
    section = c(
      "s1", "s1", "s2", "s2",
      "s1", "s1", "s2", "s2"
    ),
    variable = c(
      "a", "b", "a", "b",
      "a", "b", "a", "b"
    ),
    sub_variable = c(
      NA, "1", "1", "1.apple",
      NA, "1", "1", "1.apple"
    ),
    response = c(
      NA, "yes", "no", "jane",
      "jimothy", "pb", NA, NA
    ),
    stringsAsFactors = FALSE
  )
  res <- make_tidier_table(data)
  expect_equal(res, tidy_data)
})
