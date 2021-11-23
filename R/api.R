notion_api_url <- function() "https://api.notion.com"

notion_api_key <- function() Sys.getenv("NOTIONR_API_KEY")

notion_version <- function() "2021-08-16"

notion_api <- function(api_key = notion_api_key(),
                       version = notion_version()) {

  httr2::request("https://api.notion.com") %>%
    httr2::req_auth_bearer_token(api_key) %>%
    httr2::req_headers(`Notion-Version` = version) %>%
    httr2::req_url_path("v1")

}
