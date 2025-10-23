context("accept-reject-submission.R")

test_that("strings are generated from reject_submission", {
  m <- mockery::mock(TRUE)
  mockery::stub(reject_submission, "rest_put", m)
  test_reject <- reject_submission(syn = "",
                                   form_data_id = 1,
                                   reason = "your submission looks good")
  testthat::expect_true(test_reject)
})

test_that("error when strings are more than 500 chars", {
  string <- glue::glue_collapse({seq(1:300)}, sep = ",")
  m <- mockery::mock(TRUE)
  mockery::stub(reject_submission, "rest_put", m)
  testthat::expect_error(reject_submission(syn = "",
                                           form_data_id = 1,
                                           reason = string)
  )

})
