#' Get the presigned url for downloading the submission file.
#'
#' @param syn Synapse login object
#' @param file_handle_id The fileHandleId(s) for the submission(s).
#' @param form_data_id The formDataId(s) for the submission(s).
#' @return The presigned URL(s).
get_ps_url <- function(syn, file_handle_id, form_data_id) {
  # build the JSON object to feed to the REST call
  requested_files <- purrr::map2(file_handle_id, form_data_id,
    function(handle, form_id) {
       glue::glue('{{"fileHandleId":"{handle}",',
                  '"associateObjectId":"{form_id}",',
                  '"associateObjectType":"FormData"}}')
    }) %>%
    paste(collapse = ",")
  body <- glue::glue('{{"requestedFiles":[{requested_files}],',
                       '"includePreSignedURLs": true,',
                       '"includeFileHandles": false}}') # nolint
  response <- syn$restPOST(
    uri = "https://repo-prod.prod.sagebase.org/file/v1/fileHandle/batch",
    body = body
  )
  presigned_urls <- purrr::map(response[[1]], ~ .$preSignedURL) %>%
    purrr::as_vector() # retain return type of previous implementation
  return(presigned_urls)
}

#' Download the submission file
#'
#' Downloads the submission file locally.
#'
#' @param ps_url The presigned URL(s) for the submission.
#' @param name The basename of the submission file.
#' @param output_dir The directory to download the submission to.
#' By default the output directory is the working directory.
download_form_file <- function(ps_url, name, output_dir = ".") {
  output_dir <- normalizePath(output_dir)
  form_files <- purrr::map2(ps_url, name, function(url, basename) {
    curl::curl_download(url, destfile = fs::path(output_dir, basename))
  }) %>%
    purrr::as_vector() # retain return type of previous implementation
  return(form_files)
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
  ps_url <- get_ps_url(
    syn = syn,
    file_handle_id = file_handle_id,
    form_data_id = form_data_id
  )
  filename <- tempfile(
    pattern = glue::glue("form_{file_handle_id}_data_{form_data_id}"),
    fileext = ".json"
  )
  download_form_file(ps_url = ps_url, name = filename)
  filename
}
