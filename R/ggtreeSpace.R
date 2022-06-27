#' @title Plot phylomorphospace
#'
#' @description This function plot phylomorphospace
#' @param tr tree
#' @param tipdata tipdata containig tip coordinates
#' @param mapping aesthetic mapping
#' @return phylomorphospace plot
#' @examples
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreeSpace(tr, td) +
#'  geom_tippoint()
#'
#' @export
ggtreeSpace <- function(tr, tipdata, mapping = NULL, ...){

  dat <- cbind(tipdata[, 1], tipdata[, 2])

  trd <- ggtree::fortify(tr)

  xanc <- phytools::fastAnc(as.phylo(tr),dat[,1])
  yanc <- phytools::fastAnc(as.phylo(tr),dat[,2])

  nodecoords <- tibble(x = c(dat[,1], xanc),
                       y = c(dat[,2], yanc),
                       node = 1:length(trd$node)
  )

  trd %<>%
    select(-c(x, y)) %>%
    left_join(nodecoords, c("node" = "node"))

  p <- ggtree(trd, mapping = mapping, layout = 'equal_angle', ...)

  class(p) <- c("spacetree", class(p))

  p
}
