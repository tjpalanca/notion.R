user <- function(id, ...,
                 type = c("person", "bot"),
                 name = NULL,
                 avatar_url = NULL) {

  type <- match_arg(type)
  pkg_object(
    object = "user",
    id     = assert_uuid(id),
    type   = assert_string(type),
    name   = assert_string(name, null.ok = TRUE),
    avatar_url = assert_string(avatar_url, null.ok = TRUE)
  ) %>%
    add_class("user")

}

user_id <- function(id) {

  assert_uuid(id) %>%
    add_class(pkg_class("user_id"))

}

person_user <- function(id,
                        person,
                        name = NULL,
                        avatar_url = NULL) {

  user(
    id   = id,
    name = name,
    type = "person",
    avatar_url = avatar_url,
    person = assert_list(person)
  ) %>%
    add_class("person_user")

}

bot_user <- function(id,
                     bot,
                     name = NULL,
                     avatar_url = NULL) {

  user(
    id   = id,
    name = name,
    type = "bot",
    avatar_url = avatar_url,
    bot  = assert_list(bot)
  ) %>%
    add_class("bot_user")

}

# Conversion --------------------------------------------------------------

as_user <- function(x) UseMethod("as_user")

#' @export
as_user.list <- function(x) {

  type <- x$type
  x <- list_modify(x, type = zap(), object = zap())

  if (type == "person") {
    exec(person_user, !!!x)
  } else if (type == "bot") {
    exec(bot_user, !!!x)
  } else {
    stop("User type not known")
  }

}

# Methods -----------------------------------------------------------------

#' @export
retrieve.user_id <- function(x) get_user(x)

#' @export
retrieve.user <- function(x) retrieve(user_id(x$id))

# Helpers -----------------------------------------------------------------

get_user <- function(user_id) {

  notion_api() %>%
    req_url_path_append("users", assert_string(user_id)) %>%
    req_perform() %>%
    resp_body_json() %>%
    as_user()

}
