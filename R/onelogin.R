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

    get_host = function(region) {
      glue::glue("https://api.{region}.onelogin.com/")
    },

    initialize = function(region, client_id, client_secret) {
      region <- tolower(region)
      stopifnot(region %in% c("us", "eu"))

      self$host = self$get_host(region)
      message(glue::glue("Defining onelogin API connection {self$host}"))
      self$client_id = safer::encrypt_string(client_id)
      self$client_secret = safer::encrypt_string(client_secret)
    },

    make_auth = function(type = "token") {
      switch(
        type,
        token = glue::glue("bearer:{self$access_token}"),
        generate_token = glue::glue(
          paste0("client_id:{safer::decrypt_string(self$client_id)}, ",
                 "client_secret:{safer::decrypt_string(self$client_secret)}")))
    },

    generate_token = function() {
      res <- self$POST("/auth/oauth2/v2/token",
                       auth_type = "generate_token",
                       body = list(grant_type = "client_credentials"),
                       res_to_df = FALSE)

      self$access_token = res$access_token
      self$refresh_token = res$refresh_token
      self
    },

    get_refresh_token = function() {
      res <- self$POST("/auth/oauth2/v2/token",
                       auth_type = "generate_token",
                       body = list(grant_type = "refresh_token",
                                   access_token = self$access_token,
                                   refresh_token = self$refresh_token),
                       res_to_df = FALSE)

      self$access_token = res$access_token
      self$refresh_token = res$refresh_token
      self
    },

    revoke_token = function() {
      res <- con$POST("auth/oauth2/revoke",
               body = list(access_token = self$access_token),
               auth_type = "generate_token",
               res_to_df = FALSE)
      res$status %>% tibble::as_tibble()
    },

    GET = function(path, writer = httr::write_memory(), parser = 'parsed',
                   res_to_df = TRUE,
                   ...) {
      req <- file.path(self$host, path)
      httr::GET(req,
                       httr::add_headers(Authorization = self$make_auth()),
                       writer,
                       query = list(...)) %>%
        self$parse_res(res_to_df = res_to_df)
    },

    PUT = function(path, body = NULL, encode = 'json', res_to_df = TRUE, ...) {
      req <- paste0(self$host, path)
      httr::PUT(
        req,
        httr::add_headers(Authorization = self$make_auth(),
                          `Content-Type` = "application/json"),
        body = body,
        encode = encode
      ) %>%
        self$parse_res(res_to_df = res_to_df)
    },

    POST = function(path, body, encode = 'json', res_to_df = TRUE,
                    auth_type = "token") {
      req <- paste0(self$host, path)
      httr::POST(
        req,
        httr::add_headers(Authorization = self$make_auth(auth_type)),
        body = body,
        encode = encode) %>%
        self$parse_res(res_to_df = res_to_df)
    },

    DELETE = function(path, body = NULL, encode = 'json', res_to_df = TRUE) {
      req <- paste0(self$host, path)
      httr::DELETE(req,
                          httr::add_headers(Authorization = self$make_auth()),
                          body = body,
                          encode = encode) %>%
        self$parse_res(res_to_df = res_to_df)
    },

    parse_res = function(res, res_to_df) {
      if (httr::http_error(res)) return(res)
      res <- httr::content(res, as = 'parsed')

      # Responses come down in pages of 50 -- recurse to get all pages
      dat <- NULL
      if (!is.null(res$pagination$next_link)) {
        # Need to remove host from next link or get twice is self$GET
        req <- sub(con$host, "", res$pagination$next_link)
        dat <- self$GET(req)
      }

      if (res_to_df) {
        dat <- bind_rows(dat, self$res_to_df(res))
      } else {
        dat <- res
      }

      dat
    },

    res_to_df = function(res) {
      data <- res$data

      if (all(purrr::map_int(data, length) == 1)) {
        return(tibble::as_tibble(data))
      }

      data %>%
        purrr::map_df(~ purrr::map_if(., is.null, function(x) NA) %>%
                        tibble::as_tibble())
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
#' @return A 'OneLogin' connection
#' @export
#'
#' @examples
#' if (interactive()) onelogin(region = "US")
onelogin <- function(region = 'US',
                     client_id = Sys.getenv("ONELOGIN_CLIENT_ID"),
                     client_secret = Sys.getenv("ONELOGIN_CLIENT_SECRET")) {

  con <- ONELOGIN$new(region = region,
                      client_id = client_id,
                      client_secret = client_secret)

  # Try to get access token
  tryCatch({
    con <- con$generate_token()
  },
  error = function(e){
    message(glue::glue("Trouble getting access token."))
    stop(e)
  })
  con
}
