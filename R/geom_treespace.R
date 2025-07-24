#' A layer of phylomorphospace
#'
#' @param tr a tree object. This should be an object of class that is
#'           compatible with `ggtree`, typically an object of class
#'           `phylo` or `treedata`.
#' @param trait Trait data as a data frame or matrix, where each row
#' represents a tree tip or node.
#'
#'     For data matching the number of tips, ancestral traits are reconstructed
#'     for internal nodes.
#'
#'     For data equal to the total number of nodes, values are directly used as
#'     node coordinates.
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
#'     geom_treespace(tr, a)
geom_treespace <- function(tr, trait, mapping = NULL, ...) {
    structure(list(tr = tr, trait = trait, mapping = mapping, ...),
        class = "treespace"
    )
}


#' @importFrom ggtree geom_tree
make_ts_layer <- function(tr, trait, mapping, ...) {
  
    trd <- make_ts_data(tr, trait)
    layer <- geom_tree(
      data = trd,
      mapping = mapping,
      layout = "equal_angle",
      ...
    )
    return(layer)
  }
  


#' @title Make plot data for ggtreespace.
#' This function processes a phylogenetic tree and associated trait data to
#' create a data frame suitable for plotting with `ggtreespace`.
#'
#' @param tr a tree object. This should be an object of class that is
#'              compatible with `ggtree`, typically an object of
#'              class `phylo` or `treedata`.
#' @param trait Trait data as a data frame or matrix, where each row
#' represents  a tree tip or node.
#'
#'     For data matching the number of tips, ancestral traits are reconstructed
#'     for internal nodes.
#'
#'     For data equal to the total number of nodes, values are directly used as
#'     node coordinates.
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
make_ts_data <- function(tr, trait) {
    if (is.null(trait)) {
        stop("Trait data is required.")
    }
  
    trd <- fortify(tr)
    
    if (is.vector(trait)){
    
      if (!inherits(tr, "treedata")){
        stop("Only treedata object supports internal calling.")
      }
    
      if (length(trait) == 1){
        stop("More than one traits is needed.")
      }
    
      if (length(trait) > 2) {
        warning("Only the first 2 trait data names will be used.")
      }
    
    
      if (!all(trait %in% colnames(trd))){
        stop("Traits must be in the tree.data object.")
      }
    
      trait <- select(trd, trait)
    }
  

      nr <- nrow(trait)
      
    if (nr == 0) {
        stop("The input trait data must be a non-empty data frame.")
    }
    
    if (nr == 1) {
      stop("More than one trait is needed.")
    }

    if (ncol(trait) > 2) {
        warning("Only the first 2 column of the trait data will be used.")
    }


    # dat <- cbind(data[, 1], data[, 2])
    dat <- trait[, c(1, 2)]

    nt <- Ntip(as.phylo(tr))
    nn <- Nnode(as.phylo(tr), internal.only = FALSE)

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
    trdm <- trd |>
      select(-all_of(c("x", "y"))) |>
      mutate(
        x = if (is.matrix(coorddata)) coorddata[, 1] else coorddata[[1]],
        y = if (is.matrix(coorddata)) coorddata[, 2] else coorddata[[2]]
      ) |>
      recal_bl()
    return(trdm)
}




#' @title Make plot data for ggtreespace.
#' This function processes a phylogenetic tree and associated trait data to
#' create a data frame suitable for plotting with `ggtreespace`.
#'
#' @param data Input data of check.graph.layout
#' @param t Input traits

#'
#' @return trait data.frame
#' @importFrom ggtree fortify
#' @importFrom phytools fastAnc
#' @importFrom ape as.phylo
#' @importFrom ape Ntip
#' @importFrom ape Nnode
#'
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
intern_call <- function(data, t){
  n1 <- t[1]
  n2 <- t[2]
  
  trait1 <- data[[n1]]
  trait2 <- data[[n2]]
  
  if (length(trait1) != length(trait2)) {
    stop("The selected traits must have the same length.")
  }
  
  nt <- Ntip(data)
  nn <- Nnode(data, internal.only = FALSE)
  
  if (length(trait1) != nt && length(trait1) != nn) {
    stop("The selected trait must have a length equal to the number of tips or 
         nodes.")
  }
  
  traits <- data.frame(x = trait1,
                       y = trait2)
  rownames(traits) <- NULL
  
  if (length(trait1) == nt) {
    anc <- apply(traits, 2, fastAnc, tree = as.phylo(data))
    traits <- rbind(traits, anc)
    
    return(traits)
  }
  
  if (length(trait1) == nn) {
    return(traits)
  }
}
