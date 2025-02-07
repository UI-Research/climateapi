#' @title Get the user's username
#'
#' @return The username of the user running the script
#' @export
#'
#' @examples
#' \dontrun{
#' get_system_username()
#' }
get_system_username = function() {
  here::here() %>%
    stringr::str_match("Users/.*?/") %>%
    stringr::str_remove_all("Users|/")
}

#' @title Get the path to the C&C Box folder
#'
#' @return The filepath to the C&C Box folder
#' @export
#'
#' @examples
#' \dontrun{
#' get_box_path()
#' }
get_box_path = function() {
  username = get_system_username()
  file.path(
    "C:", "Users", username, "Box", "METRO Climate and Communities Practice Area",
    "github-repository")
}
