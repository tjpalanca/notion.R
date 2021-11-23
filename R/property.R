property <- function(id,
                     name ,
                     type = c("title", "rich_text", "number", "select",
                              "multi_select", "date", "people", "files",
                              "checkbox", "url", "email", "phone_number",
                              "formula", "relation", "rollup", "created_time",
                              "created_by", "last_edited_time",
                              "last_edited_by"),
                     ...) {

  type <- match_arg(type)
  pkg_object(
    id   = assert_string(id),
    name = assert_string(name),
    type = assert_string(type),
    ...
  ) %>%
    add_class("property")
}

title_property <- function(name) {

  property(id = "title", name = name, type = "title") %>%
    add_class("title_property")

}

rich_text_property <- function(id, name) {

  property(id = id, name = name, type = "rich_text") %>%
    add_class("rich_text_property")

}

number_property <- function(id, name,
                            format = c(
                              "number", "number_with_commas", "percent",
                              "dollar", "canadian_dollar", "euro", "pound",
                              "yen", "ruble", "rupee", "won", "yuan", "real",
                              "lira", "rupiah", "franc", "hong_kong_dollar",
                              "new_zealand_dollar", "krona", "norwegian_krone",
                              "mexican_peso", "rand", "new_taiwan_dollar",
                              "danish_krone", "zloty", "baht", "forint",
                              "koruna", "shekel", "chilean_peso",
                              "philippine_peso", "dirham", "colombian_peso",
                              "riyal", "ringgit", "leu", "argentine_peso",
                              "uruguayan_peso"
                            )) {

  format <- match_arg(format) %||% "number"
  property(id = id, name = name, type = "number",
           format = assert_string(format)) %>%
    add_class("number_property")

}

select_property <- function(id, name, options) {

  property(id = id,
           name = name,
           type = "select",
           options = assert_list(options, pkg_class("select_option"))) %>%
    add_class("select_property")

}

select_option <- function(id, name,
                          color = c(
                            "default", "gray", "brown", "orange", "yellow",
                            "green", "blue", "purple", "pink", "red"
                          )) {

  color <- match_arg(color) %||% "default"
  pkg_object(id = id, name = name, color = assert_string(color)) %>%
    add_class("select_option")

}

multi_select_property <- function(id, name, options) {

  property(id = id,
           name = name,
           type = "multi_select",
           options = assert_list(options,
                                 pkg_class("multi_select_option"))) %>%
    add_class("multi_select_property")

}

multi_select_option <- function(id, name,
                                color = c(
                                  "default", "gray", "brown", "orange",
                                  "yellow", "green", "blue", "purple", "pink",
                                  "red"
                                )) {

  color <- match_arg(color) %||% "default"
  pkg_object(id = id, name = name, color = assert_string(color)) %>%
    add_class("multi_select_option")

}

date_property <- function(id, name) {

  property(id = id, name = name, type = "date") %>%
    add_class("date_property")

}

people_property <- function(id, name) {

  property(id = id, name = name, type = "date") %>%
    add_class("people_property")

}

files_property <- function(id, name) {

  property(id = id, name = name, type = "files") %>%
    add_class("files_property")

}

checkbox_property <- function(id, name) {

  property(id = id, name = name, type = "checkbox") %>%
    add_class("checkbox_property")

}

url_property <- function(id, name) {

  property(id = id, name = name, type = "url") %>%
    add_class("url_property")

}

email_property <- function(id, name) {

  property(id = id, name = name, type = "email") %>%
    add_class("email_property")

}

phone_number_property <- function(id, name) {

  property(id = id, name = name, type = "phone_number") %>%
    add_class("phone_number_property")

}

formula_property <- function(id, name, expression) {

  property(id = id, name = name, type = "formula",
           expression = assert_string(expression)) %>%
    add_class("formula_property")

}

relation_property <- function(id, name,
                              database_id,
                              synced_property_id,
                              synced_property_name) {

  property(id = id,
           name = name,
           type = "relation",
           database_id = assert_uuid(database_id),
           synced_property_id = assert_string(synced_property_id),
           synced_property_name = assert_string(synced_property_name)) %>%
    add_class("relation_property")

}

rollup_property <- function(id, name,
                            relation_property_name,
                            relation_property_id,
                            rollup_property_name,
                            rollup_property_id,
                            `function` = c(
                              "count_all", "count_values",
                              "count_unique_values", "count_empty",
                              "count_not_empty", "percent_empty",
                              "percent_not_empty", "sum", "average", "median",
                              "min", "max", "range", "show_original"
                            )) {

  `function` <- match_arg(`function`)
  property(id = id,
           name = name,
           type = "rollup",
           relation_property_name = assert_string(relation_property_name),
           relation_property_id   = assert_string(relation_property_id),
           rollup_property_name   = assert_string(rollup_property_name),
           rollup_property_id     = assert_string(rollup_property_id),
           `function`             = assert_string(`function`)) %>%
    add_class("rollup_property")

}

created_time_property <- function(id, name) {

  property(id = id, name = name, type = "created_time") %>%
    add_class("created_time_property")

}

created_by_property <- function(id, name) {

  property(id = id, name = name, type = "created_by") %>%
    add_class("created_by_property")

}

last_edited_time_property <- function(id, name) {

  property(id = id, name = name, type = "last_edited_time") %>%
    add_class("last_edited_time_property")

}

last_edited_by_property <- function(id, name) {

  property(id = id, name = name, type = "last_edited_by") %>%
    add_class("last_edited_by_property")

}

# Conversion --------------------------------------------------------------

as_property <- function(x) UseMethod("as_property")

#' @export
as_property.list <- function(x) {

  exec(property, !!!x)

}

