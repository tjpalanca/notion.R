richtext <- function(plain_text,
                     href = NULL,
                     type = c("text", "mention", "equation"),
                     annotations = richtext_annotations(),
                     ...) {

  type <- match_arg(type) %||% "text"
  pkg_object(
    plain_text  = assert_string(plain_text),
    href        = assert_string(href, null.ok = TRUE),
    annotations = assert_class(annotations, pkg_class("richtext_annotations")),
    type        = assert_string(type),
    ...
  ) %>%
    add_class("richtext")

}

text <- function(plain_text,
                 content,
                 link = NULL,
                 href = NULL,
                 annotations = richtext_annotations()) {

  richtext(
    plain_text  = plain_text,
    href        = href,
    type        = "text",
    annotations = annotations,
    content     = assert_string(content),
    link        = assert_string(link)
  ) %>%
    add_class("text")

}

richtext_annotations <- function(
  bold          = FALSE,
  italic        = FALSE,
  strikethrough = FALSE,
  underline     = FALSE,
  code          = FALSE,
  color         = c("default", "gray", "brown", "orange", "yellow", "green",
                    "blue", "purple", "pink", "red", "gray_background",
                    "brown_background", "orange_background",
                    "yellow_background", "green_background", "blue_background",
                    "purple_background", "pink_background", "red_background")
) {

  color <- match_arg(color) %||% "default"
  pkg_object(
    bold          = assert_flag(bold),
    italic        = assert_flag(italic),
    strikethrough = assert_flag(strikethrough),
    underline     = assert_flag(underline),
    code          = assert_flag(code),
    color         = assert_string(color)
  ) %>%
    add_class("richtext_annotations")

}

# Conversions -------------------------------------------------------------

as_richtext <- function(x) UseMethod("as_richtext")

as_richtext_annotations <- function(x) UseMethod("as_richtext_annotations")

#' @export
as_richtext.list <- function(x) {

  x %>%
    modify_in("annotations", as_richtext_annotations) %>%
    exec(richtext, !!!.)

}

#' @export
as_richtext_annotations.list <- function(x) {

  exec(richtext_annotations, !!!x)

}
