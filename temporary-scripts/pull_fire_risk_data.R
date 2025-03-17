library(tidyverse)
library(rvest)
library(here)
library(janitor)
library(httr2)

html = rvest::read_html(file.path("C:", "Users", "wcurrangroome", "Downloads", "RDS-2020-0016_html.html"))

# extract the links

UA <- paste('Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:98.0)',
            'Gecko/20100101 Firefox/98.0')

get_system_username = function() {
  here::here() %>%
    stringr::str_match("Users/.*?/") %>%
    stringr::str_remove_all("Users|/")
}

get_box_path = function() {
  username = get_system_username()
  file.path(
    "C:", "Users", username, "Box", "METRO Climate and Communities Practice Area",
    "github-repository", "hazards", "fire-service")
}

html_processed = html %>%
  rvest::html_element(".product") %>%
  html_elements("a")

html_filtered = tibble(
  link = html_processed %>%
    rvest::html_attr("href"),
  text = html_processed %>%
    rvest::html_text()) %>%
  filter(str_detect(link, "zip"))

walk(
  html_filtered$link %>% .[c(3:length(.))],
  function(link) {

    filename = html_filtered$text[html_filtered$link == link]

    if (str_detect(link, "rds")) {
      link = str_c("https://www.fs.usda.gov", link)
    }

    link %>%
      httr2::request() %>%
      httr2::req_timeout(3000) %>%
      httr2::req_headers("User-Agent" = UA, "Connection" = 'keep-alive') %>%
      httr2::req_perform(path = file.path(get_box_path(), filename))
  })

rvest::html_attr("href") %>%
  keep(~ str_detect(.x, "rds.*zip"))# %>%
.[c(5:length(.))] %>%
  walk(
    .,
    function(filename) {
      str_c("https://www.fs.usda.gov", filename) %>%
        request() %>%
        req_timeout(300) %>%
        req_headers("User-Agent" = UA, "Connection" = 'keep-alive') %>%
        req_perform(path = file.path(get_box_path(), filename %>% str_split("\\/") %>% .[[1]] %>% .[[length(.)]])) })


unzip(file.path("C:", "Users", "wcurrangroome", "Downloads", "forest_service_data.zip"), exdir = file.path("C:", "Users", "wcurrangroome", "Downloads", "forest_service_data"))


get_box_path() %>%
  list.files(full.names = TRUE) %>%
  keep(~ str_detect(.x, ".*zip$")) %>%
  discard(~ str_detect(.x, "CONUS")) %>%
  walk(
    function(full_path) {
      code = str_remove(full_path, "RDS-2020") %>%
        str_extract("[0-9]{4}")
      unzip(full_path, exdir = file.path(get_box_path(), "unzipped-data", code)) })

