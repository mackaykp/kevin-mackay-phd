library(leaflet)
library(htmltools)
library(glue)
library(dplyr)

create_custom_icon <- function(icon_url, icon_width = 40, icon_height = 40) {
  makeIcon(
    iconUrl = icon_url,
    iconWidth = icon_width,
    iconHeight = icon_height,
    iconAnchorX = icon_width / 2,
    iconAnchorY = 0
  )
}

add_markers_conditionally <- function(map, data, group_name, icon_url, icon_width = 40, icon_height = 40) {
  if (!is.null(data) && nrow(data) > 0) {
    data <- data %>%
      mutate(
        popup_info = glue(
          "<b>Type:</b> {if ('amenity' %in% colnames(data)) amenity else if ('leisure' %in% colnames(data)) leisure else 'Green space'}<br>",
          "<b>Name:</b> {if ('name' %in% colnames(data)) ifelse(!is.na(name), name, 'No name available') else 'No name available'}"
        )
      )
    custom_icon <- create_custom_icon(icon_url, icon_width = icon_width, icon_height = icon_height)
    map <- map %>%
      addMarkers(data = data, icon = custom_icon, popup = ~popup_info, group = group_name)
  }
  return(map)
}

add_polylines_conditionally <- function(map, data, group_name, color) {
  if (!is.null(data) && nrow(data) > 0) {
    data <- data %>%
      mutate(
        popup_info = ifelse(
          "name" %in% colnames(data),
          glue("<b>Type:</b> {highway}<br><b>Name:</b> {name}"),
          glue("<b>Type:</b> {highway}<br><b>Name:</b> No name available")
        )
      )
    map <- map %>%
      addPolylines(data = data, color = color, weight = 3, opacity = 0.8, popup = ~popup_info, group = group_name)
  }
  return(map)
}

add_polygons_conditionally <- function(map, data, group_name, fill_color) {
  if (!is.null(data) && nrow(data) > 0) {
    data <- data %>%
      mutate(
        popup_info = ifelse(
          "name" %in% colnames(data),
          glue("<b>Type:</b> {amenity}<br><b>Name:</b> {name}"),
          glue("<b>Type:</b> {amenity}<br><b>Name:</b> No name available")
        )
      )
    map <- map %>%
      addPolygons(data = data, fillColor = fill_color, color = "black", weight = 1, fillOpacity = 0.6, popup = ~popup_info, group = group_name)
  }
  return(map)
}
