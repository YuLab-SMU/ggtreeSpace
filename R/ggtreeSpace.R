#' @title Plot phylomorphospace
#'
#' @description This function plots a phylomorphospace by mapping a tree
#' object onto a vector space like morphospace.
#' @param tr a tree object. This should be an object of class that is 
#'           compatible with `ggtree`, typically an object of class 
#'           `phylo` or `treedata`.
#' @param data Trait data as a data frame or matrix, where each row 
#' represents a tree tip or node. 
#' 
#'   For data matching the number of tips, ancestral traits are reconstructed 
#'   for internal nodes. 
#'   
#'   For data equal to the total number of nodes, values are directly used as 
#'   node coordinates.
#' @param mapping aesthetic mapping
#' @param ... additional parameters for customization with `ggtree`. Please 
#' use `?ggtree::ggtree` fot more information.
#' @return ggtreeSpace object
#' @importFrom ggtree ggtree
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 coord_cartesian
#' @examples
#' library(ggtree)
#' library(phytools)
#' library(ggtreeSpace)
#' 
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreespace(tr, td) +
#'  geom_tippoint()
#'
#' @export
ggtreespace <- function(tr, data, mapping = NULL, ...){
  
  if(is.null(data))
    stop("Traits data is required.")
  
  if(is.null(colnames(data)) || length(colnames(data)) == 0) {
    c <- c("x", "y")
  } else {
    c <- colnames(data)
  }
  
  trd <- make_ts_data(tr, data)
  
  p <- ggtree(trd, 
              mapping = mapping, 
              layout = 'equal_angle', 
              ...) +
    theme_treespace() +
    labs(x = c[1],
         y = c[2])

  suppressMessages(p <- p + coord_cartesian())
        
  class(p) <- c("ggtreeSpace", class(p))

  p
}

