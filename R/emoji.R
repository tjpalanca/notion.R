emoji <- function(emoji) {

  pkg_object(type  = "emoji", emoji = assert_string(emoji)) %>%
    add_class("emoji")

}

# Conversion --------------------------------------------------------------

as_emoji <- function(obj) UseMethod("as_emoji")

#' @export
as_emoji.list <- function(x) {

  exec(emoji, !!!list_modify(x, type = NULL))

}
