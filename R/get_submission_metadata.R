#' Get all submission metadata
#'
#' Get the metadata for all submissions.
#'
#' @param state_filter The filter that is desired to gather submissions by.
#' @param group The groupID.
#' @return A dataframe of the submission metadata.
get_submissions_metadata <- function(state_filter = "SUBMITTED_WAITING_FOR_REVIEW", group) { # nolint
  body <- glue::glue('{{"filterByState":["{state_filter}"],"groupId":"{group}"}}') # nolint
  jsonfile <- synapser::synRestPOST(
    uri = "https://repo-prod.prod.sagebase.org/repo/v1/form/data/list/reviewer",
    body = body
  )
  # Write to tempfile simply because jsonlite seems to work better
  # when reading from a file
  temp <- tempfile()
  jsonlite::write_json(jsonfile, temp)
  metadata_file <- jsonlite::fromJSON(temp, simplifyDataFrame = TRUE)
  metadata_file$page
}
