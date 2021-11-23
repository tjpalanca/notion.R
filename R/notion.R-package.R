#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_int
#' @importFrom checkmate assert_list
#' @importFrom checkmate assert_multi_class
#' @importFrom checkmate assert_number
#' @importFrom checkmate assert_posixct
#' @importFrom checkmate assert_string
#' @importFrom httr2 req_body_json
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_url_path_append
#' @importFrom httr2 req_url_query
#' @importFrom httr2 resp_body_json
#' @importFrom lubridate as_datetime
#' @importFrom magrittr %>%
#' @importFrom magrittr set_class
#' @importFrom purrr assign_in
#' @importFrom purrr list_modify
#' @importFrom purrr map
#' @importFrom purrr modify_in
#' @importFrom purrr update_list
#' @importFrom purrr zap
#' @importFrom rlang %||%
#' @importFrom rlang exec
## usethis namespace: end
NULL

pkg_name <- function() "notion.R"

pkg_class <- function(...) c(..., "notion")

pkg_object <- function(...) structure(list(...), class = pkg_class())
