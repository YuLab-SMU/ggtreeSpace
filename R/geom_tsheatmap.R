#' Add a layer of heatmap with a tip data using akima interpolation
#'
#' @param trait column name of the trait
#' @param resolution resolution of the heatmap
#' @param bins number of contour bins.
#' @param ... additional parameters
#'
#' @return ggplot object
#' @examples
#'library(ggtree)
#'library(phytools)

#'tr <- rtree(15)
#'td <- fastBM(tr, nsim = 2, bounds = c(0, Inf))
#'tdex <- data.frame(z = fastBM(tr, nsim = 1),
#'                   node = 1:15)
#'p <- ggtreeSpace(tr, td)
#'p %<+% tdex +
#'  geom_tippoint() +
#'  geom_tsheatmap(trait = "z", alpha = 0.7 ,resolution = 0.01, bin = 12)
#' @export
geom_tsheatmap <- function(trait, resolution = 0.001, bins = 24, ...){
  structure(list(trait = trait, 
                 resolution = resolution, 
                 bins, 
                 ...), 
            class = "tsheatmap")
}



#' @importFrom ggplot2 geom_contour_filled
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_stat
make_hm_layer <- function(data, trait, resolution, bins, ...){
  hmdata <- make_hm_data(data, trait, resolution)
  layer <- geom_contour_filled(data = hmdata,
                     mapping = aes(x = x, y = y, z = z, 
                                   fill = after_stat(level)),
                     bins = bins,
                     ...
                     )
  layer
}




#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
make_hm_data <- function(data, trait, resolution){
   s <- data |>
         select(x = "x",  y = "y", z=!!rlang::sym(trait)) |>
         filter(!is.na(z))
   
   coords <- akima::interp(x = s$x, 
                           y = s$y, 
                           z = s$z, 
                           xo=seq(min(s$x),max(s$x),by=resolution),
                           yo=seq(min(s$y),max(s$y),by=resolution))
   
   dz <- as.data.frame(coords$z)
   rownames(dz) <- coords$x
   colnames(dz) <- coords$y
   dz <- rownames_to_column(dz, var = "x")
   df <- dz |> 
         pivot_longer(-1, names_to = "y",values_to = "z")
   df$x <- as.double(df$x)
   df$y <- as.double(df$y)
   
   df
}





