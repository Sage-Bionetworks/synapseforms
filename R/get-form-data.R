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
  output_dir <- unlist(purrr::map(output_dir, normalizePath))
  pmap_args <- list(url = ps_url, basename = name, output_directory = output_dir)
  form_files <- purrr::pmap(pmap_args, function(url, basename, output_directory) {
    absolute_path <- fs::path(output_directory, basename)
    if (!file.exists(absolute_path)) {
      file.create(absolute_path)
    }
    curl::curl_download(url, destfile = absolute_path)
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
  filenames <- purrr::map2(file_handle_id, form_data_id, function(fhid, fdi) {
    tempfile(pattern = glue::glue("form_{fhid}_data_{fdi}"), fileext = ".json")
  })
  basenames <- purrr::map(filenames, basename)
  dirnames <- purrr::map(filenames, dirname)
  downloaded_form_file <- download_form_file(
    ps_url = ps_url,
    name = unlist(basenames),
    output_dir = unlist(dirnames)
  )
  return(downloaded_form_file)
}
