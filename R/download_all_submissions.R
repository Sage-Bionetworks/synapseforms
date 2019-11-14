#' Download all submission files to folder.
#'
#' @param state_filter The filter that is desired to gather submissions by.
#' @param group The groupID.
#' @param output_dir The directory to output the files to.
download_all_submissions <- function(state_filter = "SUBMITTED_WAITING_FOR_REVIEW", # nolint
                                     group, output_dir) {
  synapser::synLogin()
  subs_meta <- get_submissions_metadata(state_filter, group)
  ps_url_list <- purrr::map2(
    subs_meta$dataFileHandleId,
    subs_meta$formDataId,
    function(handle, id) {
      get_ps_url(handle, id)
    }
  )
  # Download all files
  purrr::map2(
    ps_url_list,
    subs_meta$name,
    function(url, name) {
      download_form_file(url, name, output_dir = output_dir)
    }
  )
}
