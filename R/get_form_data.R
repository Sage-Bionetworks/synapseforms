#' Get the presigned url for downloading the submission file.
#'
#' @param file_handle_id The fileHandleId for the submission.
#' @param form_data_id The formDataId for the submission.
#' @return A list with the fileHandleId and presigned URL.
get_ps_url <- function(file_handle_id, form_data_id) {
  body <- glue::glue('{{"requestedFiles": [{{"fileHandleId": "{file_handle_id}", "associateObjectId": "{form_data_id}", "associateObjectType": "FormData"}}], "includePreSignedURLs": true,"includeFileHandles": false}}') # nolint
  file_url <- synRestPOST(
    uri = "https://repo-prod.prod.sagebase.org/file/v1/fileHandle/batch",
    body = body
  )
}

#' Download the submission file.
#'
#' @param ps_url The presigned URL for the submission.
#' @param name The name of the submission file.
#' @param output_dir The directory to download the submission to.
download_form_file <- function(ps_url, name, output_dir) {
  curl::curl_download(ps_url, destfile = glue::glue("{output_dir}{name}"))
}
