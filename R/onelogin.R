#' Class representing a Onelogin API client
ONELOGIN <- R6::R6Class(
  'ONELOGIN',

  public = list(
    host = NULL,
    client_id = NULL,
    client_secret = NULL,
    access_token = NULL,
    refresh_token = NULL,
    token_expire = NULL,

    get_onelogin = function() {
      self
      },

    get_host = function(region) {
      glue::glue("https://api.{region}.onelogin.com/api")
    },

    initialize = function(region, client_id, client_secret) {
      region <- tolower(region)
      stopifnot(region %in% c("us", "eu"))

      self$host = self$get_host(region)
      glue::glue("Defining onelogin API connection {self$host}")
      self$client_id = safer::encrypt_string(client_id)
      self$client_secret = safer::encrypt_string(client_secret)
    }
  )
)

#' Define a 'OneLogin' Connection
#'
#' Define a connection to the 'OneLogin' API. Please see the \href{https://developers.onelogin.com/api-docs/1/getting-started/dev-overview}{API documentation}
#' for details on using this API and on getting credentials.
#'
#' @param region either "US" or "EU", defaults to "US"
#' @param client_id 'OneLogin' client ID, defaults to Sys.getenv("ONELOGIN_CLIENT_ID")
#' @param client_secret 'OneLogin' client secret, defaults to Sys.getenv("ONELOGIN_CLIENT_SECRET")
#'
#' @return a 'OneLogin' connection
#' @export
#'
#' @examples
#' onelogin(region = "US", "client_id", "client_secret")
onelogin <- function(region = 'US',
                     client_id = Sys.getenv("ONELOGIN_CLIENT_ID"),
                     client_secret = Sys.getenv("ONELOGIN_CLIENT_SECRET")) {

  con <- ONELOGIN$new(region = region,
                      client_id = client_id,
                      client_secret = client_secret)
  con
}
