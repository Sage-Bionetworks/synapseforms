context("accept-reject-submission.R")

test_that("accept_submission makes the correct string", {
  # Different return values does not matter; simply put in to check that
  # mock was called twice and returned the expected values. Leaving as is
  # for future reference on how mocking works.
  m <- mock(TRUE, FALSE)
  stub(accept_submission, "rest_put", m)
  res1 <- accept_submission(syn = "", form_data_id = 1)
  res2 <- accept_submission(syn = "", form_data_id = "42")
  uri1 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/1/accept"
  uri2 <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/42/accept"
  expect_args(m, 1, "", uri1)
  expect_args(m, 2, "", uri2)
  expect_true(res1)
  expect_false(res2)
})

test_that("accept_submission throws error if form_data_id is not Z+", {
  # Should not pass through to the rest_put function, but mocking to be safe
  # Return value does not matter. Have it cycle and return TRUE each time
  m <- mock(TRUE, cycle = TRUE)
  stub(accept_submission, "rest_put", m)
  expect_error(accept_submission("", 0))
  expect_error(accept_submission("", "0"))
  expect_error(accept_submission("", -42))
  expect_error(accept_submission("", "-42"))
  expect_error(accept_submission("", 32.4))
  expect_error(accept_submission("", "foo"))
  expect_error(accept_submission("", NULL))
  expect_error(accept_submission("", NA))
  expect_error(accept_submission("", TRUE))
  expect_error(accept_submission("", FALSE))
})
