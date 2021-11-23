#' @title
#' List all users
#'
#' @description
#' [API Reference](https://developers.notion.com/reference/get-users)
#'
#' @param n_max (int) maximum number of users to return
#'
#' @export
user_list <- function(n_max = Inf) {

  assert_number(n_max, lower = 1L)
  users    <- list()
  has_more <- TRUE
  cursor   <- NULL

  while (length(users) < n_max && has_more) {

    resp     <- get_user_list(start_cursor = cursor)
    users    <- base::append(users, resp$results)
    cursor   <- resp$next_cursor
    has_more <- resp$has_more

  }

  return(map(users, as_user))

}

# Helpers -----------------------------------------------------------------

get_user_list <- function(start_cursor = NULL, page_size = 100L) {

  notion_api() %>%
    req_url_path_append("users") %>%
    req_url_query(
      start_cursor = assert_string(start_cursor, null.ok = TRUE),
      page_size    = assert_int(page_size, lower = 1L, upper = 100L)
    ) %>%
    req_perform() %>%
    resp_body_json()

}
