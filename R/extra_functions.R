#' Fixes bad coordinates i z
#'
#' @name makeGoodShape
#'
#' @param
#' z: A data frame containing x, y and id.
#'
#' @description
#' makeGoodShape takes a set of points (x and y coordinates) and fixes them so they are ready for hexamap.
#'
#' @return
#' The function returns a data frame with coordinates
#'
#' @export
#'
#' @examples
#'
#' library(hexamapmaker)
#' library(ggplot2)
#'
#' ## Example using a "normal" grid
#' x <- c(1,2,1,2,1,2,4,4)
#' y <- c(1,1,2,2,3,3,1,2)
#'
#' id <- c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8")
#'
#' z <- data.frame(id,x,y)
#'
#' ggplot(z, aes(x, y, group = id)) +
#'   geom_point() +  coord_fixed(ratio = 1)
#'
#' zz <- hexamap(z)
#'
#' ggplot(zz, aes(x, y, group = id)) +
#'   geom_polygon(colour="black", fill = NA) +
#'   coord_fixed(ratio = 1) ## Messed up
#'
#' ## Now use new function to fix structure:
#' newZ <- makeGoodShape(z)
#'
#' ggplot(newZ, aes(x, y, group = id)) +
#'   geom_point() +  coord_fixed(ratio = 1)
#'
#' zzz <- hexamap(newZ)
#'
#' ggplot(zzz, aes(x, y, group = id)) +
#'   geom_polygon(colour="black", fill = NA) +
#'   coord_fixed(ratio = 1)

makeGoodShape <- function(z){

  z$x[y%%2==0] <- z$x[y%%2==0]* 2
  z$x[y%%2==1] <- z$x[y%%2==1]* 2 - 1
  z$y[y%%2==0] <- z$y[y%%2==0]* 2 - 1
  z$y[y%%2==1] <- z$y[y%%2==1]* 2 - 1

  return(z)
}
