#' Add a new form
#'
#' Add a new form to a group. User must have SUBMIT
#' permission on the ACL for the group.
#' See \href{https://docs.synapse.org/rest/POST/form/data.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams get_submissions_metadata
#' @param form_name The name for the form.
#' @param file_handle_id A Synapse fileHandleId with the form
#'   data.
add_new_form <- function(syn, group, file_handle_id, form_name) {
  body <- glue::glue('{{"name":"{form_name}","fileHandleId":"{file_handle_id}"}}') # nolint
  uri <- glue::glue("/form/data?groupId={group}") # nolint
  form <- rest_post(syn = syn, uri = uri, body = body)
  form
}

#' Update form
#'
#' Update a form. User must be the original owner
#' of the form and the state of the form must be either
#' `WAITING_FOR_SUBMISSION` or `REJECTED`. Froms with the states
#' `SUBMITTED_WAITING_FOR_REVIEW` or `ACCEPTED` are immutable.
#' See \href{https://docs.synapse.org/rest/PUT/form/data/id.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams add_new_form
#' @inheritParams submit_form_for_review
update_form <- function(syn, form_data_id, file_handle_id, form_name = NULL) {
  uri <- glue::glue("/form/data/{form_data_id}") # nolint
  body <- glue::glue('{{"fileHandleId":"{file_handle_id}"}}') # nolint
  if (!is.null(form_name)) {
    body <- glue::glue('{{"name":"{form_name}","fileHandleId":"{file_handle_id}"}}') # nolint
  }
  form <- rest_put(syn = syn, uri = uri, body = body)
  form
}

#' Submit a form for review
#'
#' Submit a form for review. Warning: cannot update
#' form itself after it has been submitted. Only the
#' creator of the formDataId, if they have SUBMIT
#' permissions in the ACL can submit a form for review.
#' See \href{https://docs.synapse.org/rest/POST/form/data/id/submit.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams get_submissions_metadata
#' @param form_data_id The formDataId for the form.
submit_form_for_review <- function(syn, form_data_id) {
  uri <- glue::glue("/form/data/{form_data_id}/submit")
  form <- rest_post(syn = syn, uri = uri)
  form
}

#' Delete a form
#'
#' Delete a form from the database, and caller must
#' have SUBMIT privileges in formGroup ACL.
#'
#' @export
#' @inheritParams get_submissions_metadata
#' @inheritParams submit_form_for_review
#' @return None
delete_form <- function(syn, form_data_id) {
  uri <- glue::glue("/form/data/{form_data_id}")
  rest_delete(syn = syn, uri = uri)
}
