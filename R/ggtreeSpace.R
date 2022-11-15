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
  
  trd <- make_ts_data(tr, tipdata)

  p <- ggtree(trd, mapping = mapping, layout = 'equal_angle', ...) +
        theme_bw()

  class(p) <- c("ggtreeSpace", class(p))

  p
}

