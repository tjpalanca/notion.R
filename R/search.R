#' @title
#' Notion Search API
#'
#' @description
#' [Documentation](https://developers.notion.com/reference/post-search)
#'
#' @param query `str` limits which pages are returned by comparing with title
#' @export
search_api_call <- function(query           = "",
                            sort_direction  = c("ascending", "descending"),
                            sort_timestamp  = c("last_edited_time"),
                            filter_value    = c("page", "database"),
                            filter_property = c("object"),
                            start_cursor    = NULL,
                            page_size       = 100L) {

  assert_string(query)
  assert_string(start_cursor, null.ok = TRUE)
  assert_int(page_size, lower = 1L, upper = 100L)
  sort_direction  <- match_arg(sort_direction)
  sort_timestamp  <- match_arg(sort_timestamp)
  filter_value    <- match_arg(filter_value)
  filter_property <- match_arg(filter_property)

  notion_api() %>%
    req_url_path_append("search") %>%
    req_body_json(list(
      query = query,
      sort = if (!is.null(sort_direction)) list(
        direction = sort_direction,
        timestamp = sort_timestamp
      ),
      filter = if (!is.null(filter_value)) list(
        value    = filter_value,
        property = filter_property
      ),
      start_cursor = start_cursor,
      page_size = page_size
    )) %>%
    req_perform() %>%
    resp_body_json()

}
