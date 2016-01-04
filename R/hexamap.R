#' Turns a set of points into hexagons
#'
#' @name hexamap
#'
#' @param
#' z: A data frame containing x, y and id.
#'
#' @description
#' Hexamap takes a set of points (x and y coordinates) and turns them into hexagons.
#' The idea is, that you can quickly design the layout of a hexagon map by just adding points in a coordinate system. Then the hexamap function turns them into hexagon-shaped polygons that can be plotted with ggplot2.
#' Note: the x's and y's must be spaced with 2 apart. See example.
#'
#' @return
#' The function returns a set of hexagons.
#'
#' @export
#'
#' @examples
#' # Create data frame
#' # Notice the spacing of the points
#'
#' x <- c(1,3,2,4,1,3,7,8)
#' y <- c(1,1,3,3,5,5,1,3)
#' id <- c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8")
#' z <- data.frame(id,x,y)
#'
#' # Plot points
#'
#' library(ggplot2)
#' ggplot(z, aes(x, y, group = id)) +
#'   geom_point() +
#'   coord_fixed(ratio = 1) +
#'   ylim(0,max(y)) + xlim(0,max(x))
#'
#' # Turn points into hexagons
#'
#' library(hexamapmaker)
#'
#' zz <- hexamap(z)
#'
#' ggplot(zz, aes(x, y, group = id)) +
#'   geom_polygon(colour="black", fill = NA) +
#'   coord_fixed(ratio = 1)

hexamap <- function(z){

  require(praise)

  hexadata <- data.frame()

  for(i in z$id){

    mydata <- z[z$id == i,]
    x <- mydata$x + c(-1,-1,0,1,1,0)
    y <- mydata$y + c(0,1,1.577,1,0,-0.577)

    multiplier <- (mydata$y-1)/2

    y <- y - 0.423 * multiplier

    mydata <- data.frame(id = i, x, y)

    hexadata <- rbind(hexadata, mydata)
  }

  print(praise("${EXCLAMATION}! Your new hexagon map is ${adjective}!"))

  return(hexadata)
}
