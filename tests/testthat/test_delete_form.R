library(testthat)
library(mockery)

context("testing the delete_form function in add-update-delete-forms.R")

test_that("delete_form deletes the correct form", {

  m <- mock(TRUE, cycle=TRUE)

  mockery::stub(where = delete_form,
                what = "rest_delete",
                how = m)

  #test the function
  result_1 <- delete_form(syn = "", form_data_id = 1)
  result_2 <- delete_form(syn = "", form_data_id = -1)
  result_3 <- delete_form(syn = "", form_data_id = Inf)
  result_4 <- delete_form(syn = "", form_data_id = "syn123")
  result_5 <- delete_form(syn = "", form_data_id = "x")
  result_6 <- delete_form(syn = "", form_data_id = " ")

  #expected outputs
  expected_uri_1 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/1"
  expected_uri_2 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/-1"
  expected_uri_3 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/Inf"
  expected_uri_4 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/syn123"
  expected_uri_5 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/x"
  expected_uri_6 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/ "

  #check if they match
  expect_args(m, 1, "", expected_uri_1)
  expect_args(m, 2, "", expected_uri_2)
  expect_args(m, 3, "", expected_uri_3)
  expect_args(m, 4, "", expected_uri_4)
  expect_args(m, 5, "", expected_uri_5)
  expect_args(m, 6, "", expected_uri_6)
  })
