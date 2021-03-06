#' Download all submissions locally
#'
#' Download all submissions to a local drive.
#'
#' @param syn Synapse login object
#' @param state_filter The filter that is desired to gather submissions by.
#' @param group The groupID.
#' @param output_dir The directory to output the files to.
download_all_submissions_local <- function(syn, group, output_dir,
                                           state_filter = "SUBMITTED_WAITING_FOR_REVIEW") { # nolint
  subs_meta <- get_submissions_metadata(
    syn = syn,
    group = group,
    all_users = TRUE,
    state_filter = state_filter
  )

  ps_url_list <- purrr::map2(
    subs_meta$dataFileHandleId,
    subs_meta$formDataId,
    function(handle, id) {
      get_ps_url(syn = syn, file_handle_id = handle, form_data_id = id)
    }
  )
  # Download all files
  purrr::map2(
    ps_url_list,
    subs_meta$name,
    function(url, name) {
      download_form_file(ps_url = url, name = name, output_dir = output_dir)
    }
  )
}

#' Download all submissions
#'
#' Download all submissions to temporary files.
#'
#' @export
#' @param syn Synapse login object
#' @param state_filter The filter that is desired to gather submissions by.
#' @param group The groupID.
#' @return Named list of temporary filenames, where the name is
#'   the `formDataId`.
download_all_submissions_temp <- function(syn, group,
                                          state_filter = "SUBMITTED_WAITING_FOR_REVIEW") { # nolint
  subs_meta <- get_submissions_metadata(
    syn = syn,
    group = group,
    all_users = TRUE,
    state_filter = state_filter
  )

  file_list <- purrr::map2(
    subs_meta$dataFileHandleId,
    subs_meta$formDataId,
    function(handle, id) {
      get_ps_url(syn = syn, file_handle_id = handle, form_data_id = id)
    }
  )
  unlist(file_list)
  names(file_list) <- subs_meta$formDataId
  file_list
}
