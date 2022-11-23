
#' Title
#'
#' @param ... additional parameters for theme()
#'
#' @return updated ggplot object with new theme
#' @export
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
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
  theme_bw() +
    theme(panel.grid = element_blank(), 
          ...)


