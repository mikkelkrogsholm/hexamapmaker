#' Labels hexagons made with hexamap
#'
#' @name hexalabel
#'
#' @param zz a data frame containing the output of the hexamap function
#' @param p a ggplot2 object with mapped hexagons
#'
#' @description
#' Hexalabel adds labels to your hexagons. Use it after you have first drawn the hexagons from the output from hexamap.
#'
#' @return
#' The function returns labeled hexagons using ggplot2.
#'
#' @importFrom rlang .data
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

add_hexalabel <- function(zz, p){

  # Setting to NULL to avoid CRAN issues
  # See: https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  id <- x <- y <- NULL

  centroid <- dplyr::group_by(zz, id)

  centroid <- dplyr::summarise(centroid,
                               x = mean(x, na.rm = TRUE),
                               y = mean(y, na.rm = TRUE))

  p <- p + ggplot2::geom_text(data = centroid,
                            ggplot2::aes(x = x, y = y, label = id))

  return(p)
}

