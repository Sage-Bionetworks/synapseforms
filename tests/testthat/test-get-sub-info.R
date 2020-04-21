context("get-sub-info.R")

data <- tibble::tribble(
  ~form_data_id, ~section, ~variable, ~sub_variable, ~response,
  "sub1", "a", "1", NA, "Jim",
  "sub1", "a", "2", NA, "Smith",
  "sub1", "b", "1", NA, "1",
  "sub1", "b", "1", "i", "a",
  "sub1", "c", "1", "i", "TRUE",
  "sub1", "c", "2", NA, "yes",
  "sub1", "d", "i", NA, NA,
  "sub2", "a", "1", NA, "Sally",
  "sub2", "a", "2", NA, "Quackenbush",
  "sub2", "b", "1", NA, NA,
  "sub2", "b", "1", "i", NA,
  "sub2", "c", "1", "i", NA,
  "sub2", "c", "2", NA, "no",
  "sub2", "d", "i", NA, "3",
  "sub3", "a", "1", NA, "Alice",
  "sub3", "a", "2", NA, "Patton",
  "sub3", "b", "1", NA, "3",
  "sub3", "b", "1", "i", "c",
  "sub3", "c", "1", "i", "FALSE",
  "sub3", "c", "2", NA, "no",
  "sub3", "d", "i", NA, "5",
  "sub4", "a", "1", NA, NA,
  "sub4", "a", "2", NA, NA,
  "sub4", "b", "1", NA, "4",
  "sub4", "b", "1", "i", "d",
  "sub4", "c", "1", "i", NA,
  "sub4", "c", "2", NA, NA,
  "sub4", "d", "i", NA, NA,
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

test_that("get_submission_ids returns correct names", {
  res1 <- get_submission_ids(data)
  res2 <- get_submission_ids(data[1:14, ])
  expect_equal(res1, c("sub1", "sub2", "sub3", "sub4"))
  expect_equal(res2, c("sub1", "sub2"))
})
