#' Get the presigned url for downloading the submission file.
#'
#' @param syn Synapse login object
#' @param file_handle_id The fileHandleId for the submission.
#' @param form_data_id The formDataId for the submission.
#' @return The presigned URL.
get_ps_url <- function(syn, file_handle_id, form_data_id) {
  body <- glue::glue('{{"requestedFiles": [{{"fileHandleId": "{file_handle_id}", "associateObjectId": "{form_data_id}", "associateObjectType": "FormData"}}], "includePreSignedURLs": true,"includeFileHandles": false}}') # nolint
  file_url <- syn$restPOST(
    uri = "https://repo-prod.prod.sagebase.org/file/v1/fileHandle/batch",
    body = body
  )
  file_url$requestedFiles[[1]]$preSignedURL
}

#' Download the submission file
#'
#' Downloads the submission file locally. Can specify
#' the directory to download to, or else the `name` parameter
#' will assume the path is included.
#'
#' @param ps_url The presigned URL for the submission.
#' @param name The name, with extension, of the submission file.
#'   If the `output_dir` is not specified, then `name` should be
#'   full path.
#' @param output_dir The directory to download the submission to.
#'   If `NULL`, will assume full path included in `name`; else will
#'   prepend `output_dir` to `name`.
download_form_file <- function(ps_url, name, output_dir = NULL) {
  if (is.null(output_dir)) {
    curl::curl_download(ps_url, destfile = name)
  } else {
    curl::curl_download(ps_url, destfile = glue::glue("{output_dir}{name}"))
  }
}

#' Download submission to temp file
#'
#' Downloads the submission to a temporary
#' file and returns the name of the file.
#'
#' @param syn Synapse login object
#' @inheritParams get_ps_url
#' @return The name of the temporary file.
get_form_temp <- function(syn, file_handle_id, form_data_id) {
  ps_url <- get_ps_url(syn, file_handle_id, form_data_id)
  filename <- tempfile(
    pattern = glue::glue("form_{file_handle_id}_data_{form_data_id}"),
    fileext = ".json"
  )
  download_form_file(ps_url, name = filename)
  filename
}
