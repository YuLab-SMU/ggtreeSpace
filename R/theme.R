#' Phylomorphospace theme No.1, with arrow-end axis and grey panel grid.
#'
#' @param ... additional parameters for `ggplot2::theme`. Please use
#' `?ggplot2::theme()` to learn more information.
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
#' library(ggtreeSpace)
#'
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreespace(tr, td) +
#'   geom_tippoint() +
#'   theme_treespace()
theme_treespace <- function(...) {
  theme_minimal() %+replace%
    theme(
      axis.line.x = element_line(arrow = arrow(length = unit(0.3, "cm"))),
      axis.line.y = element_line(arrow = arrow(length = unit(0.3, "cm"))),
      ...
    )
}




#' Phylomorphospace theme No.2, with blank background and panel border.
#'
#' @param ... additional parameters for `ggplot2::theme`. Please use
#' `?ggplot2::theme()` to learn more information.
#'
#' @return a theme object with blank background and panel border
#' @export
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 theme_bw
#'
#' @examples
#' library(ggtree)
#' library(phytools)
#' library(ggtreeSpace)
#'
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2)
#' ggtreespace(tr, td) +
#'   geom_tippoint() +
#'   theme_treespace2()
theme_treespace2 <- function(...) {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      ...
    )
}
