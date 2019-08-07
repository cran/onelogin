# Manage API Access OAuth Tokens

#' Generate a 'OneLogin' token
#'
#' @param con a 'OneLogin' connection
#'
#' @return A 'OneLogin' connection with auth token
#' @export
#'
#' @examples
#' if(interactive()) ol_token_get(onelogin())
ol_token_get <- function(con) {
  con$generate_token()
}

#' Refresh 'OneLogin' auth token
#'
#' @inheritParams ol_token_get
#'
#' @return A 'OneLogin' connection with refreshed auth token
#' @export
#'
#' @examples
#' if(interactive()) ol_token_refresh(onelogin())
ol_token_refresh <- function(con) {
  con$get_refresh_token()
}

#' Revoke `OneLogin` access token
#'
#' @inheritParams ol_token_get
#'
#' @return A tibble of response status
#' @export
#'
#' @examples
#' #' if(interactive()) ol_token_revoke(onelogin())
ol_token_revoke <- function(con) {
  con$revoke_token()
}

#' Get 'OneLogin' API rate limit
#'
#' @inheritParams ol_token_get
#'
#' @return A tibble of rate limit data
#' @export
#'
#' @examples
#' if(interactive()) ol_token_get_rate_limit(onelogin())
ol_token_get_rate_limit <- function(con) {
  con$GET("auth/rate_limit")
}