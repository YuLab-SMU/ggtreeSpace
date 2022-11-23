
#' Phylomorphospace theme No.1, with arrow-end axis and grey panel grid
#'
#' @param ... additional parameters for theme()
#'
#' @return a theme object with arrow-end axis
#' @export
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 %+replace%
#' @importFrom grid arrow
#' @importFrom grid unit
#'
#' @examples
#' library(ggtree)
#' library(phytools)
#' 
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreeSpace(tr, td) +
#'  geom_tippoint() + 
#'  theme_treeSpace()
theme_treeSpace <- function(...){
  theme_minimal() %+replace%
    theme(axis.line.x = element_line(arrow = arrow(length = unit(0.3, 'cm'))),
          axis.line.y = element_line(arrow = arrow(length = unit(0.3, 'cm'))),
          ...) 
}




#' Phylomorphospace theme No.1,with blank background and panel border
#'
#' @param ... additional parameters for theme()
#'
#' @return a theme object with blank background and panel border
#' @export
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 theme_bw
#'
#' @examples
#' library(ggtree)
#' library(phytools)
#' 
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreeSpace(tr, td) +
#'  geom_tippoint() + 
#'  theme_treeSpace2()
theme_treeSpace2 <- function(...){
  theme_bw() +
    theme(panel.grid = element_blank(), 
          ...)
}

