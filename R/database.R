database <- function(id, created_time, last_edited_time,
                     title, icon = NULL, cover = NULL, properties, parent, url) {

  object(
    object           = "database",
    id               = assert_string(id),
    created_time     = assert_datetime_string(created_time),
    last_edited_time = assert_datetime_string(last_edited_time),
    title            = assert_list(title, pkg_class("richtext")),
    icon             = assert_multi_class(icon, c("file", "emoji", pkg_name()),
                                          null.ok = TRUE),
    cover            = assert_class(cover, pkg_class("file"), null.ok = TRUE),
    properties       = assert_list(properties,
                                   types = pkg_class("property"),
                                   names = "named"),
    parent           = assert_multi_class(parent, c("page_parent",
                                                    "workspace_parent")),
    url              = assert_string(url)
  ) %>%
    add_class("database")

}

database_id <- function(id) {

  object(id = assert_string(id)) %>%
    add_class("database_id")

}

# Conversions -------------------------------------------------------------

as_database <- function(x) UseMethod("as_database")

#' @export
as_database.list <- function(x) {

  x %>%
    list_modify(object = zap()) %>%
    assign_in("title", map(.$title, as_richtext)) %>%
    assign_in("properties", map(.$properties, as_property)) %>%
    assign_in("icon", if (is.null(.$icon)) {
      NULL
    } else if (.$icon$type == "file") {
      as_file(.$icon)
    } else if (.$icon$type == "emoji") {
      as_emoji(.$icon)
    }) %>%
    assign_in("cover", if (is.null(.$cover)) {
      NULL
    } else {
      as_file(.$cover)
    }) %>%
    assign_in("parent", if (is.null(.$parent)) {
      NULL
    } else if (.$parent$type == "workspace") {
      as_workspace_parent(.$parent)
    } else if (.$parent$type == "page") {
      as_page_parent(.$parent)
    }) %>%
    exec(database, !!!.)

}

# Methods -----------------------------------------------------------------

