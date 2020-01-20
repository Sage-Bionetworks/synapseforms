#' Add a new form
#'
#' Add a new form to a group. User must have SUBMIT
#' permission on the ACL for the group.
#' See \href{https://docs.synapse.org/rest/POST/form/data.html}{Synapse REST API}. # nolint
#'
#' @inheritParams get_submissions_metadata
#' @param form_name The name for the form.
#' @param file_handle_id A Synapse fileHandleId with the form
#'   data.
add_new_form <- function(syn, group, form_name, file_handle_id) {
  body <- glue::glue('{{"name":"{form_name}","fileHandleId":"{file_handle_id}"}}') # nolint
  form <- syn$restPOST(
    uri = glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/data?groupId={group}"), # nolint
    body = body
  )
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
  form <- syn$restPOST(
    uri = glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/data/{form_data_id}/submit"), # nolint
    body = "" # synapseclient requires body here, but API does not
  )
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
  syn$restDELETE(
    uri = glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/data/{form_data_id}")
  )
}
