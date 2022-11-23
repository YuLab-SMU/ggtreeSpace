
#' A layer of phylomorphospace
#'
#' @param tr a phylogenetic tree
#' @param tipdata tipdata
#' @param mapping mapping
#' @param ... additional parameters
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggtree)
#' library(phytools)
#' 
#' tr <- rtree(10)
#' a <- fastBM(tr, nsim = 2)
#' 
#' p <- ggplot() + geom_treeSpace(tr, a)
geom_treeSpace <- function(tr, tipdata, mapping = NULL, ...)
{
  structure(list(tr = tr, tipdata = tipdata, mapping = mapping, ...), 
            class = "treespace")
}


#' @method ggplot_add treespace
#' @export

ggplot_add.treespace <- function(object, plot, object_name)
{
  st_layer <- do.call(make_ts_layer, object)
  plot$layers <- append(plot$layers, st_layer)
  plot
}


#' @importFrom ggtree geom_tree
make_ts_layer <- function(tr, tipdata, mapping, ...)
{
  trd <- make_ts_data(tr, tipdata)
  layer <- geom_tree(data = trd,
                     mapping = mapping,
                     layout = "equal_angle",
                     ...
                     )
  layer
}


#' @importFrom ggtree fortify
#' @importFrom phytools fastAnc
#' @importFrom ape as.phylo
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
make_ts_data <- function(tr, tipdata)
{
  dat <- cbind(tipdata[, 1], tipdata[, 2])
  trd <- fortify(tr)
  anc <- apply(dat, 2, fastAnc, tree = as.phylo(tr))
  nodecoords <- rbind(dat, anc)
  
  trd %<>%
    select(-c(x, y)) %>%
    mutate(x = nodecoords[,1], 
           y = nodecoords[,2])

  trd
}


