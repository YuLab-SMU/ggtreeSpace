#' A layer of phylomorphospace
#'
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
#' @param ... additional parameters for customization with `geom_tree`. Please
#' use `?ggtree::geom_tree` for more information.
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
#' p <- ggplot() +
#'   geom_treespace(tr, a)
geom_treespace <- function(tr, data, mapping = NULL, ...) {
  structure(list(tr = tr, data = data, mapping = mapping, ...),
    class = "treespace"
  )
}


#' @importFrom ggtree geom_tree
make_ts_layer <- function(tr, data, mapping, ...) {
  trd <- make_ts_data(tr, data)
  layer <- geom_tree(
    data = trd,
    mapping = mapping,
    layout = "equal_angle",
    ...
  )
  layer
}



#' @title Make plot data for ggtreespace.
#' This function processes a phylogenetic tree and associated trait data to
#' create a data frame suitable for plotting with `ggtreespace`.
#'
#' @param tr a tree object. This should be an object of class that is
#'           compatible with `ggtree`, typically an object of
#'           class `phylo` or `treedata`.
#' @param data Trait data as a data frame or matrix, where each row
#' represents  a tree tip or node.
#'
#'   For data matching the number of tips, ancestral traits are reconstructed
#'   for internal nodes.
#'
#'   For data equal to the total number of nodes, values are directly used as
#'   node coordinates.
#'
#' @return ggplot object
#' @importFrom ggtree fortify
#' @importFrom phytools fastAnc
#' @importFrom ape as.phylo
#' @importFrom dplyr mutate
#' @importFrom ape Ntip
#' @importFrom ape Nnode
#'
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
#' trd <- make_ts_data(tr, a)
make_ts_data <- function(tr, data) {
  if (is.null(data)) {
    stop("Trait data is required.")
  }

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("The input trait data must be a data frame or matrix.")
  }

  nr <- nrow(data)
  if (nr == 0) {
    stop("The input trait data must be a non-empty data frame.")
  }

  if (ncol(data) > 2) {
    warning("Only the first 2 column of the trait data will be used.")
  }

  trd <- fortify(tr)
  # dat <- cbind(data[, 1], data[, 2])
  dat <- data[, c(1, 2)]

  nt <- Ntip(tr)
  nn <- Nnode(tr, internal.only = FALSE)

  if (nr != nt && nr != nn) {
    stop("The input trait data must be as long as the number of
         tips or nodes.")
  }

  if (nr == nt) {
    anc <- apply(dat, 2, fastAnc, tree = as.phylo(tr))
    dat <- rbind(dat, anc)
  }

  tsd <- make_tsd(trd, dat)
  return(tsd)
}

make_tsd <- function(trd, coorddata) {
  trd |>
    select( - all_of(c('x', 'y'))) |>
    mutate(
      x = coorddata[, 1],
      y = coorddata[, 2]
    ) |>
    recal_bl()
}
