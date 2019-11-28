context("get-sub-info.R")

data <- tibble::tribble(
  ~variables, ~sub1, ~sub2, ~sub3, ~sub4,
  "a.1", "Jim", "Sally", "Alice", NA,
  "a.2", "Smith", "Quackenbush", "Patton", NA,
  "b.1",  1, NA, 3, 4,
  "b.1.i", "a", NA, "c", "d",
  "c.1.i", TRUE, NA, FALSE, NA,
  "c.2", "yes", "no", "no", NA,
  "d.i", NA, 3, 5, NA
)

test_that("get_main_sections returns correct sections", {
  res1 <- get_main_sections(data, "sub1")
  res2 <- get_main_sections(data, "sub2")
  res3 <- get_main_sections(data, "sub3")
  res4 <- get_main_sections(data, "sub4")
  res5 <- get_main_sections(data, "sub5")
  expect_equal(res1, c("a", "b", "c"))
  expect_equal(res2, c("a", "c", "d"))
  expect_equal(res3, c("a", "b", "c", "d"))
  expect_equal(res4, "b")
  expect_null(res5)
})

test_that("get_submission_names returns correct names", {
  res1 <- get_submission_names(data)
  res2 <- get_submission_names(data[, 1:3])
  expect_equal(res1, c("sub1", "sub2", "sub3", "sub4"))
  expect_equal(res2, c("sub1", "sub2"))
})
