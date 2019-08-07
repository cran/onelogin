# Provide wrappers around User resources

###### GET

#' Get Users from OneLogin
#'
#' You can filter the user by various parameters in onelogin. See the
#' \href{https://developers.onelogin.com/api-docs/1/users/get-users}{page in the API docs} for filter options.
#'
#' The id column in the returned tibble is the user_id for any of the user
#' functions that are by id.
#'
#' @inheritParams ol_token_get
#' @param ... filter parameters, optional; see API documentation
#'
#' @return A tibble of users and their attributes
#' @export
#'
#' @examples
#' if (interactive()) ol_users_get(onelogin())
#' if (interactive()) ol_users_get(onelogin(), firstname = "name")
ol_users_get <- function(con,  ...) {
  con$GET("api/1/users", ...)
}

#' Get a User by their ID
#'
#' @inheritParams ol_token_get
#' @param user_id user id
#'
#' @return A tibble of user data (one row)
#' @export
#'
#' @examples
#' if (interactive()) ol_user_get_by_id(onelogin(), 54400533)
ol_user_get_by_id <- function(con, user_id) {
  con$GET(glue::glue("api/1/users/{user_id}"))
}

#' Get Apps for User
#'
#' @inheritParams ol_user_get_by_id
#'
#' @return A tibble of user data (one row)
#' @export
#'
#' @examples
#' if (interactive()) ol_user_get_apps(onelogin(), 54400533)
ol_user_get_apps <- function(con, user_id) {
  con$GET(glue::glue("api/1/users/{user_id}/apps"))
}

#' Get Roles for a User
#'
#' @inheritParams ol_user_get_by_id
#'
#' @return A data frame of the user id and role
#' @export
#'
#' @examples
#' if (interactive()) ol_user_get_roles(onelogin(), 54400533)
ol_user_get_roles <- function(con, user_id) {
  res <- con$GET(glue::glue("api/1/users/{user_id}/roles"), res_to_df = FALSE)

  tibble::tibble(user_id = user_id, roles = unlist(res$data))
}

#' Get custom fields available for users
#'
#' @inheritParams ol_user_get_by_id
#'
#' @return A tibble of custom fields available
#' @export
#'
#' @examples
#' if (interactive()) ol_user_get_custom_fields(onelogin())
ol_user_get_custom_fields <- function(con) {
  con$GET(glue::glue("api/1/users/custom_attributes"))
}

##### POST

#' Create a 'OneLogin' user.
#'
#' For a full listing of available fields, see the \href{https://developers.onelogin.com/api-docs/1/users/create-user}{API documentation}
#'
#' @param con a 'OneLogin' connection
#' @param firstname first name, character
#' @param lastname last name, character
#' @param email full email, character
#' @param username username
#' @param ... other named parameters for the person
#'
#' @return A tibble of user data
#' @export
#'
#' @examples
#' if (interactive()) ol_user_create(onelogin(), "Fake", "User",
#' "fake@user.com", "fake")
ol_user_create <- function(con, firstname, lastname, email, username, ...) {
  con$POST("api/1/users",
           body = c(list(firstname = firstname,
                         lastname = lastname,
                         email = email,
                         username = username),
                    list(...)))
}

###### PUT

#' Update user information by ID
#'
#' @inheritParams ol_user_get_by_id
#' @param ... named parameters to change in request
#'
#' @return A tibble of user data
#' @export
#'
#' @examples
#' if (interactive()) ol_user_update(onelogin(), 54963040, firstname = "Fake1")
ol_user_update <- function(con, user_id, ...) {
  con$PUT(glue::glue("api/1/users/{user_id}"), body = list(...))
}

#' Assign role to user
#'
#' @inheritParams ol_user_get_by_id
#' @param role_id_array numeric, one or more roles
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_assign_role(onelogin(), 54963040, 268986)
ol_user_assign_role <- function(con, user_id, role_id_array) {
  stopifnot(is.numeric(role_id_array))

  con$PUT(glue::glue("api/1/users/{user_id}/add_roles"),
                     body = jsonlite::toJSON(list(role_id_array = role_id_array)))
}

#' Remove role from user
#'
#' @inheritParams ol_user_assign_role
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_remove_role(onelogin(), 54963040, 268986)
ol_user_remove_role <- function(con, user_id, role_id_array) {
  stopifnot(is.numeric(role_id_array))

  con$PUT(glue::glue("api/1/users/{user_id}/remove_roles"),
                     body = jsonlite::toJSON(list(role_id_array = role_id_array)))
}

#' Set or change user's password
#'
#' @inheritParams ol_user_get_by_id
#' @param password character, new password
#' @param password_confirmation character, new password
#' @param validate_policy utilize password policy checks? defaults to FALSE
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_pwd_cleartext(onelogin(), 54963040, "pwd", "pwd")
ol_user_pwd_cleartext <- function(con, user_id,
                                  password, password_confirmation,
                                  validate_policy = FALSE) {

  con$PUT(glue::glue("api/1/users/set_password_clear_text/{user_id}"),
          body = list(password = password,
                      password_confirmation = password_confirmation,
                      validate_policy = validate_policy))

}

#' Set or changes user's password after encryption
#'
#' See the \href{https://developers.onelogin.com/api-docs/1/users/set-password-using-sha-256}{API docs} for details on how to encrypt the password.
#'
#' @inheritParams ol_user_pwd_cleartext
#' @param password_algorithm algorithm, defaults to "salt+sha256"
#' @param password_salt defaults to ""
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_pwd_sha256_salt(onelogin(), 54963040,
#' safer::encrypt_string("saltpwd"), safer::encrypt_string("saltpwd"),
#' password_salt = "salt")
ol_user_pwd_sha256_salt <- function(con, user_id,
                                  password,
                                  password_confirmation,
                                  password_algorithm = "salt+sha256",
                                  password_salt = "") {

  con$PUT(glue::glue("api/1/users/set_password_using_salt/{user_id}"),
          body = list(password = password,
                      password_confirmation = password_confirmation,
                      password_algorithm = password_algorithm,
                      password_salt = password_salt))
}

#' Set custom attribute for users
#'
#' See \code{\link{ol_user_get_custom_fields}} to get custom fields
#'
#' @inheritParams ol_user_get_by_id
#' @param ... named custom attributes to set
#' @export
#'
#' @examples
#' if (interactive()) ol_user_set_custom_attr(onelogin, 54963040, attr = "value")
ol_user_set_custom_attr <- function(con, user_id, ...) {
  con$PUT(glue::glue("/api/1/users/{user_id}/set_custom_attributes"),
          body = list(custom_attributes = list(...)))
}

#' Set user state
#'
#' @inheritParams ol_user_get_by_id
#' @param state numeric, 0-3, see \href{https://developers.onelogin.com/api-docs/1/users/set-state}{documentation} for value meanings
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_set_state(onelogin(), 54963040, 1)
ol_user_set_state <- function(con, user_id, state) {
  stopifnot(as.integer(state) %in% 0L:3L)

  con$PUT(glue::glue("api/1/users/{user_id}/set_state"),
          body = list(state = as.integer(state)))
}

#' Log user out
#'
#' @inheritParams ol_user_get_by_id
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_log_out(onelogin(), 54963040)
ol_user_log_out <- function(con, user_id) {
  con$PUT(glue::glue("api/1/users/{user_id}/logout"))
}

#' Lock user
#'
#' @inheritParams ol_user_get_by_id
#' @param locked_until numeric, number of minutes, default to 0 see \href{https://developers.onelogin.com/api-docs/1/users/lock-user-account}{API documentation} for more details
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_lock_account(onelogin(), 54963040)
ol_user_lock_account <- function(con, user_id, locked_until = 0) {
 con$PUT(glue::glue("api/1/users/{user_id}/lock_user"),
         body = list(locked_until = locked_until))
}

#' Delete user
#'
#' @inheritParams ol_user_get_by_id
#'
#' @export
#'
#' @examples
#' if (interactive()) ol_user_delete(onelogin(), 54963040)
ol_user_delete <- function(con, user_id) {
  con$DELETE(glue::glue("api/1/users/{user_id}"))
}
