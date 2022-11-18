
geom_treeSpace <- function(tr, tipdata, mapping = NULL, ...)
{
  structure(list(tr = tr, tipdata = tipdata, mapping = mapping, ...), 
            class = "treespace")
}

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

#' @importFrom dplyr mutate
#' @importFrom magrittr %<>% %>%
make_ts_data <- function(tr, tipdata)
{
  dat <- cbind(tipdata[, 1], tipdata[, 2])
  trd <- ggtree::fortify(tr)
  anc <- apply(dat, 2, fastAnc, tree = as.phylo(tr))
  nodecoords <- rbind(dat, anc)
  
  trd %<>%
    dplyr::select(-c(x, y)) %>%
    dplyr::mutate(x = nodecoords[,1], 
           y = nodecoords[,2])

  trd
}


