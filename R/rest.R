#' restGET wrapper
#'
#' Wrapper for the Synapse restGET API call.
#'
#' @export
#' @param syn Synapse login object
#' @param uri The uri for the API call
rest_get <- function(syn, uri) {
  syn$restGET(uri = uri)
}

#' restPUT wrapper
#'
#' Wrapper for the Synapse restPUT API call.
#'
#' @export
#' @inheritParams rest_get
#' @param body The body for the API call
rest_put <- function(syn, uri, body) {
  syn$restPUT(uri = uri, body = body)
}

#' restPOST wrapper
#'
#' Wrapper for the Synapse restPOST API call.
#'
#' @export
#' @inheritParams rest_put
rest_post <- function(syn, uri, body) {
  syn$restPOST(uri = uri, body = body)
}

#' restDELETE wrapper
#'
#' Wrapper for the Synapse restDELETE API call.
#'
#' @export
#' @inheritParams rest_get
rest_delete <- function(syn, uri) {
  syn$restDELETE(uri = uri)
}
