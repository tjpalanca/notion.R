#' @title
#' Notion File Object
#'
#' @description
#' [Documentation](https://developers.notion.com/reference/file-object)
#'
#' @name file
NULL

file <- function(url, ...) {

  object(url = assert_string(url), ...) %>%
    add_class("file")

}

external_file <- function(url) {

  file(type = "external", url = url) %>%
    add_class("external_file")

}

uploaded_file <- function(url, expiry_time) {

  file(type = "file", url = url,
       expiry_time = assert_datetime_string(expiry_time)) %>%
    add_class("uploaded_file")

}

# Conversion --------------------------------------------------------------

as_file <- function(obj) UseMethod("as_file")

#' @export
as_file.list <- function(x) {

  exec(file, !!!x)

}
