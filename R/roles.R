# Wrappers for roles API

#' Get all available roles
#'
#' @inheritParams ol_token_get
#' @param ... filters for the roles. See options in the \href{https://developers.onelogin.com/api-docs/1/roles/get-roles}{API docs}.
#'
#' @return a tibble of roles
#' @export
#'
#' @examples
#' if (interactive()) ol_roles_get(onelogin())
ol_roles_get <- function(con, ...) {
  con$GET("api/1/roles", ...)
}

#' Get details of particular role
#'
#' @inheritParams ol_token_get
#' @param role_id the id of a role
#'
#' @return a tibble of details on the role
#' @export
#'
#' @examples
#' if (interactive()) ol_role_get_by_id(onelogin(), 1234)
ol_role_get_by_id <- function(con, role_id) {
 con$GET(glue::glue("api/1/roles/{role_id}"))
}