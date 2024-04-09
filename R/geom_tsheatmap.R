#' Add a layer of heat map with trait data
#'
#' @param trait trait data. It can be either a data frame with node numbers
#' and  trait, or a trait name present in the plot data.
#' @param resolution resolution of the heat map
#' @param bins number of contour bins
#' @param ... additional parameters for `geom_contour_filled`. Please
#' use `?ggplot2::geom_contour_filled` for more information.
#'
#' @return ggplot object
#' @examples
#'
#' library(ggtree)
#' library(phytools)
#' library(ggplot2)
#' library(ggtreeSpace)
#'
#' tr <- rtree(15)
#' td <- fastBM(tr, nsim = 2, bounds = c(0, Inf))
#' col <- colorRampPalette(c(
#'   "#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C",
#'   "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"
#' ))(24)
#' tdex <- data.frame(
#'   z = fastBM(tr, nsim = 1, bounds = c(0, Inf)),
#'   node = 1:15
#' )
#' p <- ggtreespace(tr, td)
#' p %<+% tdex +
#'   geom_tippoint() +
#'   geom_tsheatmap(trait = "z", alpha = 0.7, resolution = 0.01, bin = 24) +
#'   scale_fill_manual(
#'     values = col,
#'     guide = guide_colorsteps(show.limits = TRUE)
#'   ) +
#'   theme_treespace2() +
#'   theme(
#'     legend.key.height = unit(1, "null"),
#'     legend.justification.top = "right"
#'   )
#' @export
geom_tsheatmap <- function(trait, resolution = 0.001, bins = 24, ...) {
  structure(
    list(
      trait = trait,
      resolution = resolution,
      bins,
      ...
    ),
    class = "tsheatmap"
  )
}



#' @importFrom ggplot2 geom_contour_filled
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_stat
make_hm_layer <- function(data, trait, resolution, bins, ...) {
  hmdata <- make_hm_data(data, trait, resolution)
  layer <- geom_contour_filled(
    data = hmdata,
    mapping = aes(
      x = x, y = y, z = z,
      fill = after_stat(level)
    ),
    bins = bins,
    ...
  )
  layer
}




#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom tidyselect all_of
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom interp interp
#' @importFrom stats na.omit
make_hm_data <- function(data, trait, resolution) {
  if (is.null(trait)) {
    stop("Traits data is required.")
  }

  if (is.data.frame(trait)) {
    if (!"node" %in% names(trait)) {
      stop("Trait data should contain a column of corresponding node number")
    }
    if (ncol(trait) > 2) {
      warning("only the first trait provided will be used")
    }

    traitn <- setdiff(names(trait), "node")[1]

    traitd <- trait |>
      # select(node, traitn)
      select(all_of(c('node', traitn)))

    dt <- left_join(data, traitd, by = "node")

    s <- dt |>
      na.omit() |>
      select(x = "x", y = "y", z = !!rlang::sym(traitn))
  } else if (is.character(trait)) {
    if (!trait %in% colnames(data)) {
      stop("Plot data do not contain this trait")
    }
    s <- data |>
      na.omit() |>
      select(x = "x", y = "y", z = !!rlang::sym(trait))
  } else {
    stop("Trait input should be either a data frame with node numbers and
         trait, or a trait name present in the plot data.")
  }

  coords <- interp(
    x = s$x,
    y = s$y,
    z = s$z,
    xo = seq(min(s$x), max(s$x), by = resolution),
    yo = seq(min(s$y), max(s$y), by = resolution)
  )

  dz <- as.data.frame(coords$z)
  rownames(dz) <- coords$x
  colnames(dz) <- coords$y
  dz <- rownames_to_column(dz, var = "x")
  df <- dz |>
    pivot_longer(-1, names_to = "y", values_to = "z")
  df$x <- as.double(df$x)
  df$y <- as.double(df$y)

  df
}
