#' Create new Synapse FormGroup
#'
#' Create a new Synapse FormGroup. Creator will be
#' listed in ACL as administrator. See
#' \href{https://docs.synapse.org/rest/POST/form/group.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams download_all_and_get_table
#' @param name Group name, between 3 and 256 characters,
#'   that is globally unique. If a group with the same name exists
#'   and the user has ACCESS_TYPE.READ permissions on the group,
#'   then will get the FormGroup back.
#' @return The FormGroup list, including the fields groupId,
#'   name, createdBy, and createdOn.
create_new_form_group <- function(syn, name) {
  body <- glue::glue('{{"name":"{name}"}}')
  uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/group?name={name}") # nolint
  group_response <- rest_post(syn = syn, uri = uri, body = body)
  group_response
}

#' Get FormGroup ACL
#'
#' Get the Access Control List for a FormGroup.
#' See \href{https://docs.synapse.org/rest/POST/form/group/id/acl.html}{Synapse REST API}. # nolint
#'
#' @export
#' @inheritParams create_new_form_group
#' @param group The FormGroup id number.
#' @return The Access Control List (ACL) as a list.
get_group_acl <- function(syn, group) {
  uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/group/{group}/acl") # nolint
  acl <- rest_get(syn = syn, uri = uri)
  acl
}

#' Update a FormGroup ACL
#'
#' Update the ACL for a FormGroup.
#' See \href{https://docs.synapse.org/rest/PUT/form/group/id/acl.html}{Synapse REST API}. # nolint
#' WARNING: Ensure ACL is formatted correctly.
#'
#' @export
#' @inheritParams get_group_acl
#' @param access_control_list The access control list (ACL) in JSON form.
update_group_acl <- function(syn, group, access_control_list) {
  uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/group/{group}/acl") # nolint
  acl <- rest_put(syn = syn, uri = uri, body = access_control_list)
  acl
}
