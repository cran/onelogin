# Functions for invite endpoints

#' Generate Invite Link
#'
#' @inheritParams ol_token_get
#' @param email email of a user
#'
#' @return data frame with email and invite link
#' @export
#'
#' @examples
#' if(interactive()) ol_invite_gen_link(onelogin(), "example@example.com")
ol_invite_gen_link <- function(con, email) {
  res <- con$POST("api/1/invites/get_invite_link",
                  body = list(email = email),
                  res_to_df = FALSE)

  tibble::tibble(email = email, link = res$data[[1]])
}

#' Send Invite Links
#'
#' @inheritParams ol_invite_gen_link
#'
#' @return NULL
#' @export
#'
#' @examples
#' if (interactive()) ol_invite_send_link(onelogin(), "example@example.com")
ol_invite_send_link <- function(con, email) {
  con$POST("api/1/invites/send_invite_link", body = list(email = email))
}