user_list <- function(n_max = Inf) {

  assert_number(n_max, lower = 1L)
  users    <- list()
  has_more <- TRUE
  cursor   <- NULL

  while (length(users) < n_max && has_more) {

    resp <-
      user_list_api_call(start_cursor = cursor) %>%
      req_perform() %>%
      resp_body_json()

    users    <- append(users, resp$results)
    cursor   <- resp$next_cursor
    has_more <- resp$has_more

  }

  return(map(users, as_user))

}

user_list_api_call <- function(start_cursor = NULL, page_size = 100L) {

  notion_api() %>%
    req_url_path_append("users") %>%
    req_url_query(
      start_cursor = assert_string(start_cursor, null.ok = TRUE),
      page_size    = assert_int(page_size, lower = 1L, upper = 100L)
    )

}
