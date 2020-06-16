#' Reject a submission
#'
#' Reject a submission and give a reason. This sets the
#' submission status `state` to `REJECTED`, and updates the
#' review elements, `reviewedOn`, `reviewedBy`, and
#' `rejectionMessage`.` Note: this function will
#' throw an error if the REST call to Synapse fails; this could
#' happen if the user does not have needed permissions, or
#' the `form_data_id` specified does not exist.
#'
#' WARNING: Once a submission is rejected, the state can
#' not be changed again.
#'
#' See \href{https://docs.synapse.org/rest/POST/form/data/id/reject.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams download_all_and_get_table
#' @param form_data_id The formDataId of the submission.
#' @param reason The reason for the rejection as a string;
#'   limited to 500 characters.
#' @return The response from the rejection call as a list containing
#'   the submission metadata.
reject_submission <- function(syn, form_data_id, reason) {
  if (nchar(reason) <= 500) {
    body <- glue::glue('{{"reason":"{reason}"}}')
    uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/data/{form_data_id}/reject") # nolint
    reject_response <- rest_put(syn = syn, uri = uri, body = body)
    reject_response
  } else {
    stop("The rejection reason is limited to 500 characters")
  }
}

#' Accept a submission
#'
#' Accept a submission. This sets the submission status
#' `state` to `ACCEPTED`, and updates the review elements,
#' `reviewedOn` and `reviewedBy`. Note: this function will
#' throw an error if the REST call to Synapse fails; this could
#' happen if the user does not have needed permissions, or
#' the `form_data_id` specified does not exist.
#'
#' WARNING: Once a submission is rejected, the state can
#' not be changed again.
#'
#' See \href{https://docs.synapse.org/rest/POST/form/data/accept.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams reject_submission
#' @return The response from the acceptance call as a list containing
#'   the submission metadata.
accept_submission <- function(syn, form_data_id) {
  if (!is_positive_integer(form_data_id)) {
    stop("form_data_id must be a positive integer")
  }
  uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/data/{form_data_id}/accept") # nolint
  accept_response <- rest_put(syn = syn, uri = uri)
  accept_response
}
