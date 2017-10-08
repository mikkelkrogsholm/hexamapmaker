#' Fixes bad coordinates i z
#'
#' @name fix_shape
#'
#' @param z A data frame containing x, y and id.
#'
#' @description
#' fix_shape takes a set of points (x and y coordinates) and fixes them so they are ready for hexamap.
#'
#' @return
#' The function returns a data frame with coordinates
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(hexamapmaker)
#' library(ggplot2)
#' library(tibble)
#' library(ggthemes)
#'
#' # Points on a "normal" grid.
#' my_points <- tibble::tibble(
#'   x = c(1, 2, 1, 2, 1, 2, 4, 4),
#'   y = c(1, 1, 2, 2, 3, 3, 1, 2),
#'   id = c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8")
#' )
#'
#' # Plot the points
#' ggplot(my_points, aes(x = x, y = y, group = id)) +
#'   geom_point() +
#'   coord_fixed(ratio = 1) +
#'   theme_map()
#'
#' # Turn points into hexagons
#' hexa_points <- make_polygons(my_points)
#'
#' # Plot the new hexagons
#' ggplot(hexa_points, aes(x, y, group = id)) +
#'   geom_polygon(colour = "black", fill = NA) +
#'   coord_fixed(ratio = 1) +
#'   theme_map()
#'
#' # Oh no! It is way off - lets fix it.
#' my_points <- fix_shape(my_points)
#'
#' # Plot points
#' ggplot(my_points, aes(x = x, y = y, group = id)) +
#'   geom_point() +
#'   coord_fixed(ratio = 1) +
#'   theme_map()
#'
#' # Turn points into hexagons
#' hexa_points <- make_polygons(my_points)
#'
#' ggplot(hexa_points, aes(x, y, group = id)) +
#'   geom_polygon(colour = "black", fill = NA) +
#'   coord_fixed(ratio = 1) +
#'   theme_map()
#'
#' # Add color by using the fill argument in ggplot.
#' # Remember to remove it from the geom_polygon then
#' (p <- ggplot(hexa_points, aes(x, y, group = id, fill = id)) +
#'     geom_polygon(colour = "black", show.legend = FALSE) +
#'     coord_fixed(ratio = 1) +
#'     theme_map())
#'
#' # Label hexagons
#' add_hexalabel(hexa_points, p)

fix_shape <- function(z){

  y <- z$y

  z$x[y %% 2 == 0] <- z$x[y %% 2 == 0] * 2
  z$x[y %% 2 == 1] <- z$x[y %% 2 == 1] * 2 - 1
  z$y[y %% 2 == 0] <- z$y[y %% 2 == 0] * 2 - 1
  z$y[y %% 2 == 1] <- z$y[y %% 2 == 1] * 2 - 1

  return(z)
}
