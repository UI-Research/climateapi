# library(tidyverse)
# library(sf)
# library(tigris)
#
# ## Note that this function is an adaptation of original work by Kyle Walker
# ## as reflected in the function `tigris::shift_geometry()`; significant portions
# ## of this code are taken verbatim, or with minimal changes, from that function.
#
# shift_geometries = function(sf) {
#
# }
#
# default_projection = 4326
#
# input_sf = st_read(here::here("data", "US States and Territories Shapefile_20250502")) %>%
#   ## ensure geometries are workable
#   st_cast("MULTIPOLYGON") %>%
#   st_make_valid() %>%
#   st_buffer(0) %>%
#   ## reduce object size enormously
#   st_simplify() %>%
#   select(
#     state_name = name,
#     state_code = statefp,
#     state_abbreviation = stusps) %>%
#   st_transform(default_projection)
#
# ## our benchmark geographies
# valid_sf = input_sf
#
# state_codes_shifted = c(
#   "02", # AK
#   "15", # HI
#   "60", # AS
#   "66", # GU
#   "69", # MP
#   "72", # PR
#   "78")  # VI
#
# to_bbox_sf = function(sf) {
#   sf %>%
#     st_bbox() %>%
#     st_as_sfc()
# }
#
# bbox_states = valid_sf %>%
#   dplyr::filter(state_code %in% state_codes_shifted)
#
# bboxes = map_dfr(
#   bbox_states$state_code,
#   function(state_code) {
#     ## trim the HI bbox to exclude uninhabited archipelagos
#     if (state_code == "15") {
#       state_bbox = st_bbox(c(
#         xmin = -161,
#         ymin = 18.5,
#         xmax = -154.5,
#         ymax = 22.5),
#         crs = st_crs(4326)) %>%
#         st_as_sfc() %>%
#         st_as_sf() %>%
#         dplyr::mutate(
#           state_code = {{state_code}},
#           intersection_flag = any(sf::st_intersects(input_sf, ., sparse = FALSE)[,1]))
#     } else {
#       state_bbox = bbox_states %>%
#         dplyr::filter(state_code == {{state_code}}) %>%
#         to_bbox_sf %>%
#         sf::st_as_sf() %>%
#         dplyr::mutate(
#           state_code = {{state_code}},
#           intersection_flag = any(sf::st_intersects(input_sf, ., sparse = FALSE)[,1]))
#     }}) %>%
#   dplyr::arrange(state_code)
#
# if (bboxes %>% dplyr::filter(isTRUE(intersection_flag)) %>% nrow() == 0) {
#   warning("None of your features require shifting.\nTransforming your object's CRS to 'EPSG:4326'",
#           call. = FALSE)
#
#   return(input_sf %>% st_transform(default_projection))
# }
#
# # Check to see if there is a GEOID column to identify state information
# # If it is a GEOID that works (e.g. counties, tracts), then use it and avoid spatial inferences
# if (!is.null(geoid_column)) {
#   input_sf = input_sf %>%
#     dplyr::mutate(state_code = stringr::str_sub(.data[[geoid_column]], 1, 2)) } else {
#   # This is where we need to infer the location of the features
#   # We can do this by checking to see where the input features intersect
#   # the bounding boxes
#   input_sf = input_sf %>%
#     dplyr::mutate(
#       state_code = dplyr::case_when(
#         suppressMessages(sf::st_intersects(input_sf, bboxes[1,], sparse = FALSE)[,1]) ~ "02",
#         suppressMessages(sf::st_intersects(input_sf, bboxes[2,], sparse = FALSE)[,1]) ~ "15",
#         suppressMessages(sf::st_intersects(input_sf, bboxes[3,], sparse = FALSE)[,1]) ~ "60",
#         suppressMessages(sf::st_intersects(input_sf, bboxes[4,], sparse = FALSE)[,1]) ~ "66",
#         suppressMessages(sf::st_intersects(input_sf, bboxes[5,], sparse = FALSE)[,1]) ~ "69",
#         suppressMessages(sf::st_intersects(input_sf, bboxes[6,], sparse = FALSE)[,1]) ~ "72",
#         suppressMessages(sf::st_intersects(input_sf, bboxes[7,], sparse = FALSE)[,1]) ~ "78",
#         TRUE ~ "00")) }
#
# state_projections = tibble::tribble(
#   ~state_code, ~projection,
#   "02", 4326,# 3338,
#   "15", 4326,# 6633,
#   "60", 4326,# 2195,
#   "66", 4326,# 6637,
#   "69", 4326,# 2195,
#   "72", 4326,# 32161,
#   "78", 4326)# 32161)
#
# states_to_project = valid_sf %>%
#   dplyr::left_join(state_projections, by = "state_code") %>%
#   dplyr::filter(!is.na(projection))
#
# state_centroids = map(
#   states_to_project$state_code,
#   function(state_code) {
#     state = states_to_project %>%
#       dplyr::filter(state_code == {{state_code}})
#
#     projection = state$projection
#
#     state %>%
#       sf::st_geometry() %>%
#       sf::st_transform(projection) %>%
#       sf::st_centroid()
#     })
#
# # Parse the geometries (thanks to Claus Wilke for code samples & inspiration)
# place_geometry_wilke <- function(geometry, position, scale = 1, centroid = sf::st_centroid(geometry)) {
#   (geometry - centroid) * scale + sf::st_sfc(st_point(position))
# }
#
# bb = valid_sf %>%
#   dplyr::filter(!state_code %in% state_codes_shifted) %>%
#   sf::st_transform(default_projection) %>%
#   sf::st_bbox()
#
# input_lower48 = input_sf %>%
#   dplyr::filter(!state_code %in% state_codes_shifted) %>%
#   sf::st_transform(default_projection)
#
# # Initialize the list in which shapes will be stored
# shapes_list = list(us_lower48)
#
# position = "below"
# rescale = TRUE
#
# # "02", # AK
# # "15", # HI
# # "60", # AS
# # "66", # GU
# # "69", # MP
# # "72", # PR
# # "78")  # VI
#
# shift_metadata = tibble::tribble(
#   ~state_name, ~state_abbreviation, ~state_code, ~position, ~x_shift, ~y_shift, ~scale, ~rescale,
#   ## rescale == TRUE
#   "Alaska", "AK", "02", "below", .08, .07, .35, TRUE,
#   "Alaska", "AK", "02", "outside", -.08, 1.2, .35, TRUE,
#   "Hawaii", "HI", "15", "below", .35, .1, 1, TRUE,
#   "Hawaii", "HI", "15", "outside", 0, .2, 1, TRUE,
#   "Puerto Rico", "PR", "72", "below", .65, .05, 2.5, TRUE,
#   "Puerto Rico", "PR", "72", "outside", .95, -.05, 2.5, TRUE,
#
#   "United States Virgin Islands", "VI", "78", "below", .65, -.05, 1.25, TRUE,
#   "United States Virgin Islands", "VI", "78", "outside", .95, 0.05, 1.25, TRUE,
#   "American Samoa", "AS", "60", "below", .33, 0, 1, TRUE,
#   "American Samoa", "AS", "60", "outside", 0, .2, 1, TRUE,
#   "Guam", "GU", "66", "below", .2, -.05, 2, TRUE,
#   "Guam", "GU", "66", "outside", 0, .2, 2, TRUE,
#   "Commonwealth of the Northern Mariana Islands", "MP", "69", "below", .2, .1, .75, TRUE,
#   "Commonwealth of the Northern Mariana Islands", "MP", "69", "outside", 0, .2, .75, TRUE,
#
#   ## rescale == FALSE
#   "Alaska", "AK", "02", "below", .2, .13, .35, FALSE,
#   "Alaska", "AK", "02", "outside", .25, 1.35, .35, FALSE,
#   "Hawaii", "HI", "15", "below", .6, .1, 1.5, FALSE,
#   "Hawaii", "HI", "15", "outside", 0, .2, 1.5, FALSE,
#   "Puerto Rico", "PR", "72", "below", .75, -.1, 2.5, FALSE,
#   "Puerto Rico", "PR", "72", "outside", .95, -.05, 2.5, FALSE,
#
#   "United States Virgin Islands", "VI", "78", "below", .65, 0, 2.5, FALSE,
#   "United States Virgin Islands", "VI", "78", "outside", .95, 0.05, 2.5, FALSE,
#   "American Samoa", "AS", "60", "below", .6, .1, 2, FALSE,
#   "American Samoa", "AS", "60", "outside", 0, .2, 2, FALSE,
#   "Guam", "GU", "66", "below", .6, .1, 1.5, FALSE,
#   "Guam", "GU", "66", "outside", 0, .2, 1.5, FALSE,
#   "Commonwealth of the Northern Mariana Islands", "MP", "69", "below", .6, .1, 1.5, FALSE,
#   "Commonwealth of the Northern Mariana Islands", "MP", "69", "outside", 0, .2, 1.5, FALSE) %>%
#   dplyr::filter(
#     position == {{position}},
#     rescale == {{rescale}}) %>%
#   dplyr::mutate(
#     scale = dplyr::if_else(rescale == FALSE, 1, scale))
#
# states_to_shift = states_to_project %>%
#   dplyr::select(state_code, projection) %>%
#   dplyr::left_join(
#     shift_metadata,
#     by = "state_code") %>%
#   dplyr::mutate(id = dplyr::row_number()) %>%
#   dplyr::select(
#     id,
#     state_code,
#     projection,
#     x_shift,
#     y_shift,
#     scale)
#
# scale_and_shift = function(
#     id,
#     state_code,
#     geometry,
#     projection,
#     x_shift,
#     y_shift,
#     scale) {
#
#   # state = slice(states_to_shift, 1)
#   # x_shift = state$x_shift
#   # y_shift = state$y_shift
#   # scale = state$scale
#
#   #browser()
#
#   state = geometry %>%
#     st_sfc() %>%
#     st_set_crs(default_projection) %>%
#     sf::st_transform(projection) %>%
#     st_as_sf() %>%
#     mutate(state_code = {{state_code}})
#
#   if (state_code == "15") {
#     state = state %>%
#       st_intersection(bboxes %>% dplyr::filter(state_code == "15"))
#   }
#
#   position = c(
#     bb$xmin + x_shift*(bb$xmax - bb$xmin),
#     bb$ymin + y_shift*(bb$ymax - bb$ymin))
#
#   adjusted_geometry = place_geometry_wilke(
#     geometry = sf::st_geometry(state),
#     position = position,
#     scale = scale,
#     centroid = state_centroids[[id]]) %>%
#     sf::st_geometry()
#
#   st_geometry(state) = adjusted_geometry
#
#   sf::st_crs(state) = default_projection
#
#   return(state)
# }
#
#
# temp = purrr::pmap(
#   states_to_shift,
#   scale_and_shift) %>%
#   bind_rows()# %>%
#   #filter(state_code != "02")
#
# ggplot() +
#   geom_sf(data = input_lower48 %>% st_transform(5070), fill = "blue") +
#   geom_sf(data = temp, aes(fill = state_code), color = NA) +
#   urbnthemes::theme_urbn_map()
#
# output_data = shapes_list %>%
#   dplyr::bind_rows() %>%
#   dplyr::select(-state_fips)
#
# return(output_data)
#
#
# ggplot() +
#   geom_sf(data = bboxes, fill = "blue") +
#   geom_sf(data = input_sf, fill = "green")
#
# all_shifted = test %>%
#   shift_geometry(position = "outside")
#
# ## 52 -- 50 states, DC, PR
# tigris_shifted_geoemtries = all_shifted %>%
#   filter(state_code == "72" | state_code < 60)
#
# unshifted_geometries = all_shifted %>%
#   filter(!state_code %in% tigris_shifted_geoemtries$state_code)
#
# tigris_shifted_bbox = st_bbox(tigris_shifted_geoemtries) %>% st_as_sfc() %>% st_buffer(100000)
#
# ## virgin islands
# ## just south and east of PR
#
# ## american samoa
# ## significantly south and west of HI
#
# ## guam
# ## even further west of HI, but similar north-south alignment
#
# ## northern mariana islands
# ## directly north of guam
#
# unshifted_geometries
#
# shifted_usvi = unshifted_geometries %>%
#   filter(state_code == "78")
#
#
#
#
# position = "outside"
# rescale = TRUE
#
#
#
# ggplot() +
#   geom_sf(data = tigris_shifted_geoemtries, fill = "blue", color = NA) +
#   geom_sf(data = unshifted_geometries, fill = "green", color = NA) +
#   geom_sf(data = tigris_shifted_bbox, fill = NA, color = "red") +
#   urbnthemes::theme_urbn_map()
#
#
# tigris_shifted_geoemtries %>% nrow()
#
# all_shifted %>%
#   arrange(desc(state_code))
#
#

