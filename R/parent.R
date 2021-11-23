parent <- function(...) {

  object(...) %>%
    add_class("parent")

}

page_parent <- function(page_id) {

  parent(type = "page_id", page_id = assert_uuid(page_id)) %>%
    add_class("page_parent")

}

workspace_parent <- function() {

  parent(type = "workspace", workspace = TRUE) %>%
    add_class("workspace_parent")

}

# Conversion --------------------------------------------------------------

as_page_parent <- function(x) UseMethod("as_page_parent")

as_workspace_parent <- function(x) UseMethod("as_workspace_parent")

#' @export
as_page_parent.list <- function(x) {

  exec(page_parent, !!!list_modify(x, type = zap()))

}

#' @export
as_workspace_parent.list <- function(x) {

  exec(workspace_parent, !!!list_modify(x, type = zap(), workspace = zap()))

}
