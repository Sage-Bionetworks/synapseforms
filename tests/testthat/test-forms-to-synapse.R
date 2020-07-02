context("forms-to-synapse")

setup({
  FORM_DATA <- c(27776867, 26133145, 26304195)
  form_group_name <- uuid::UUIDgenerate()
  # save syn object and form group to global namespace
  syn <<- log_into_synapse()
  form_group <<- create_new_form_group(syn, form_group_name)
  forms <- purrr::map(FORM_DATA, function(data_file_handle_id) {
    add_new_form(
      syn, form_group$groupId, data_file_handle_id, uuid::UUIDgenerate())
  })
  # submit the first two forms
  submitted_forms <- purrr::map(forms[c(1,2)], function(form) {
    submit_form_for_review(syn, form$formDataId)
  })
  # review the first form
  reviewed_forms <- accept_submission(syn, submitted_forms[[1]]$formDataId)
})

teardown({
  syn$delete(form_group)
})

test_that("We see 3 recently created forms", {
  recently_created_forms <- get_recent_forms(
    syn = syn,
    form_group_id = form_group$groupId,
    time_duration = 600,
    form_event = "create",
    as_reviewer = FALSE)
  expect_equal(nrow(recently_created_forms), 3)
})

test_that("We see 2 recently submitted forms", {
  recently_submitted_forms <- get_recent_forms(
    syn = syn,
    form_group_id = form_group$groupId,
    time_duration = 600,
    form_event = "submit",
    as_reviewer = TRUE)
  expect_equal(nrow(recently_submitted_forms), 2)
})

test_that("We see 1 recently reviewed form", {
  recently_reviewed_forms <- get_recent_forms(
    syn = syn,
    form_group_id = form_group$groupId,
    time_duration = 600,
    form_event = "review",
    as_reviewer = TRUE)
  expect_equal(nrow(recently_reviewed_forms), 1)
})

test_that("We only see recently submitted forms", {
  unsubmitted_forms = get_recent_forms(
    syn = syn,
    form_group_id = form_group$groupId,
    time_duration = Inf,
    form_event="create",
    as_reviewer = FALSE,
    submission_state = "WAITING_FOR_SUBMISSION")
  expect_gt(nrow(unsubmitted_forms), 0)
  # pause 10 seconds, then submit previously unsubmitted form (see `setup`)
  Sys.sleep(10)
  submit_form_for_review(syn, unsubmitted_forms$formDataId[[1]])
  recently_submitted_forms <- get_recent_forms(
    syn = syn,
    form_group_id = form_group$groupId,
    time_duration = 10,
    form_event = "submit",
    as_reviewer = TRUE)
  expect_equal(nrow(recently_submitted_forms), 1)
})

test_that("We properly validate arguments to get_recent_forms", {
  expect_error(validate_form_event_params(
    time_duration = "1", # is a string integer, invalid
    form_event = "create"))
  expect_error(validate_form_event_params(
    time_duration = 1,
    form_event = "created" # is not an allowed value
  ))
})

test_that("We return some text for our email body in response to form event", {
  email_body <- draft_message(
    exportable_forms = mtcars,
    form_group_name = "ITS YO BABY BOY BOBBY JEAN",
    form_event = "submit")
  expect_type(email_body, "character")
})