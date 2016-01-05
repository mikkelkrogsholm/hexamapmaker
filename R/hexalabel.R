#' Labels hexagons made with hexamap
#'
#' @name hexalabel
#'
#' @param
#' zz: A data frame containing the output of the hexamap function
#' p: a ggplot2 object with mapped hexagons
#'
#' @description
#' Hexalabel adds labels to your hexagons. Use it after you have first drawn the hexagons from the output from hexamap.
#'
#' @return
#' The function returns labeled hexagons using ggplot2.
#'
#' @export
#'
#' @examples
#' # Create data frame
#' Notice the spacing of the points
#'
#' x <- c(1,3,2,4,1,3,7,8)
#' # y <- c(1,1,3,3,5,5,1,3)
#' id <- c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8")
#' z <- data.frame(id,x,y)
#'
#' Plot points
#'
#' library(ggplot2)
#' ggplot(z, aes(x, y, group = id)) +
#'   geom_point() +
#'   coord_fixed(ratio = 1) +
#'   ylim(0,max(y)) + xlim(0,max(x))
#'
#'  Turn points into hexagons
#'
#' library(hexamapmaker)
#'
#' zz <- hexamap(z)
#'
#' # Create plot
#'
#' p <- ggplot(zz, aes(x, y, group = id)) +
#'   geom_polygon(colour="black", fill = NA) +
#'   coord_fixed(ratio = 1)
#'
#' # Label hexagons
#'
#' hexalabel(zz,p)
#'
#' p

hexalabel <- function(zz, p){
  require(dplyr)
  require(ggplot2)
  centroid <- zz %>% group_by(id) %>% summarise(x = mean(x), y = mean(y))
  p <- p + geom_text(data=centroid, aes(x=x, y=y, label=id))
  return(p)
}
