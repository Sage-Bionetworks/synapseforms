#' Get all submission metadata by state filter
#'
#' Get the metadata for all submissions by state filter:
#' WAITING_FOR_SUBMISSION, SUBMITTED_WAITING_FOR_REVIEW,
#' ACCEPTED, REJECTED.
#'
#' @export
#' @param syn Synapse login object
#' @param state_filter The submission state of the submissions.
#' Pass a list of submission states to match on any state in the list.
#' Submission states are: `WAITING_FOR_SUBMISSION`, `SUBMITTED_WAITING_FOR_REVIEW`,
#' `ACCEPTED`, `REJECTED`. Set to NULL (default) to ignore submission state.
#' @param group The groupID.
#' @param all_users TRUE to get all submissions in group; FALSE to get
#'   group submissions from caller only.
#' @return A dataframe of the submission metadata, or NULL if there are
#'   no submissions that meet the criteria.
#' @examples
#' \dontrun{
#'
#' # Note: Must have permissions to access group.
#' # The example group will not work for most individuals.
#'
#' syn <- log_into_synapse()
#'
#' sub_meta_to_review <- get_submissions_metadata(
#'   syn = syn,
#'   group = 13
#' )
#'
#' sub_meta_accepted <- get_submissions_metadata(
#'   syn = syn,
#'   state_filter = "ACCEPTED",
#'   group = 13
#' )
#' }
get_submissions_metadata <- function(syn, group, all_users = TRUE,
                                     state_filter = "SUBMITTED_WAITING_FOR_REVIEW") { # nolint
  uri <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/list/reviewer"
  if (!all_users) {
    uri <- "https://repo-prod.prod.sagebase.org/repo/v1/form/data/list"
  }
  if (is.null(state_filter)) {
    if (all_users) { # We cannot get forms that are WAITING_FOR_SUBMISSION as a reviewer
      state_filter <- c("SUBMITTED_WAITING_FOR_REVIEW", "ACCEPTED", "REJECTED")
    } else {
      state_filter <- c("WAITING_FOR_SUBMISSION", "SUBMITTED_WAITING_FOR_REVIEW",
                        "ACCEPTED", "REJECTED")
    }
  }
  state_filter_str <- paste(state_filter, collapse = "\",\"")
  query_string <- '{{"filterByState":["{state_filter_str}"],"groupId":"{group}"}}' # nolint
  body <- glue::glue(query_string)
  response <- syn$restPOST(
    uri = uri,
    body = body
  )
  if (length(response$page) == 0) {
    return(NULL)
  }
  metadata <- get_json_as_df(data = response$page)

  while (length(response) == 2) {
    query_string <- '{{"filterByState":["{state_filter_str}"],"groupId":"{group}","nextPageToken":"{response$nextPageToken}"}}' # nolint
    body <- glue::glue(query_string)
    response <- syn$restPOST(
      uri = uri,
      body = body
    )
    temp_metadata <- get_json_as_df(data = response$page)
    metadata <- rbind(metadata, temp_metadata)
  }
  metadata
}

#' Get submission metadata as dataframe
#'
#' Get the submission metadata as a dataframe.
#'
#' @keywords internal
#' @param data The json submission metadata.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_json_as_df <- function(data) {
  # Write to tempfile because the data is not a json string or txt,
  # which is what jsonlite wants; this solves the problem.
  temp <- tempfile()
  jsonlite::write_json(data, temp)
  data_df <- jsonlite::fromJSON(temp, simplifyDataFrame = TRUE)
  # Fix dataframe columns into list columns
  data_df <- data_df %>% dplyr::mutate(
    submissionStatus_submittedOn = .data$submissionStatus$submittedOn,
    submissionStatus_state = .data$submissionStatus$state,
    submissionStatus = NULL
  )
  # Change list columns to character columns
  data_df <- tidyr::unnest(data_df, names(data_df))
  data_df
}
