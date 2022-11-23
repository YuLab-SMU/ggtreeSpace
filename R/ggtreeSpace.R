#' @title Plot phylomorphospace
#'
#' @description This function plot phylomorphospace
#' @param tr tree
#' @param tipdata tipdata containig tip coordinates
#' @param mapping aesthetic mapping
#' @param ... additional parameters
#' @return ggtreeSpace object
#' @importFrom ggtree ggtree
#' @importFrom ggplot2 labs
#' @examples
#' library(ggtree)
#' library(ggplot2)
#' library(phytools)
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreeSpace(tr, td) +
#'  geom_tippoint()
#'
#' @export
ggtreeSpace <- function(tr, tipdata, mapping = NULL, ...){
  
  c <- colnames(tipdata)
  trd <- make_ts_data(tr, tipdata)

  p <- ggtree(trd, 
              mapping = mapping, 
              layout = 'equal_angle', 
              ...) +
        theme_treeSpace() +
        labs(x = c[1],
             y = c[2])
        
  class(p) <- c("ggtreeSpace", class(p))

  p
}

