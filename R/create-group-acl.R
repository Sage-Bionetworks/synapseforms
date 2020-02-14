#' Create new Synapse FormGroup
#'
#' Create a new Synapse FormGroup. Creator will be
#' listed in ACL as administrator. See
#' \href{https://docs.synapse.org/rest/POST/form/group.html}{Synapse REST API}. # nolint
#'
#' @inheritParams download_all_and_get_table
#' @param name Group name, between 3 and 256 characters,
#'   that is globally unique. If a group with the same name exists
#'   and the user has ACCESS_TYPE.READ permissions on the group,
#'   then will get the FormGroup back.
#' @return The FormGroup list, including the fields groupId,
#'   name, createdBy, and createdOn.
create_new_form_group <- function(syn, name) {
  body <- glue::glue('{{"name":"{name}"}}')
  group_response <- syn$restPOST(
    uri = glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/group?name={name}"), # nolint
    body = body
  )
  group_response
}

#' Get FormGroup ACL
#'
#' Get the Access Control List for a FormGroup.
#' See \href{https://docs.synapse.org/rest/POST/form/group/id/acl.html}{Synapse REST API}. # nolint
#'
#' @inheritParams create_new_form_group
#' @param group The FormGroup id number.
#' @return The Access Control List (ACL) as a list.
get_group_acl <- function(syn, group) {
  acl <- syn$restGET(
    uri = glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/group/{group}/acl") # nolint
  )
  acl
}

#' Update a FormGroup ACL
#'
#' Update the ACL for a FormGroup.
#' See \href{https://docs.synapse.org/rest/PUT/form/group/id/acl.html}{Synapse REST API}. # nolint
#' WARNING: Ensure ACL is formatted correctly.
#'
#' @inheritParams get_group_acl
#' @param access_control_list The access control list (ACL) in JSON form.
update_group_acl <- function(syn, group, access_control_list) {
  acl <- syn$restPUT(
    uri = glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/form/group/{group}/acl"), # nolint
    body = access_control_list
  )
  acl
}
