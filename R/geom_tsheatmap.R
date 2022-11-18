#' Add a layer of heatmap with a tip data using akima interpolation
#'
#' @param trait column name of the trait
#' @param resolution resolution of the heatmap
#' @param bins Number of contour bins. Overridden by breaks.
#' @param show.legend 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_tsheatmap <- function(trait, resolution = 0.001, bins = 24, show.legend = FALSE, ...){
  structure(list(trait = trait, 
                 resolution = resolution, 
                 bins, 
                 ...), 
            class = "tsheatmap")
}

ggplot_add.tsheatmap <- function(object, plot, object_name){
  l <- list(data = plot$data)
  object <- c(l, object)
  hm_layer <- do.call(make_hm_layer, object)
  plot$layers <- append(plot$layers, hm_layer)
  plot
}


make_hm_layer <- function(data, trait, resolution, bins, ...){
  hmdata <- make_hm_data(data, trait, resolution)
  layer <- geom_contour_filled(data = hmdata,
                     mapping = aes(x = x, y = y, z = z, fill = after_stat(level)),
                     bins = bins,
                     ...
                     )
  layer
}

make_hm_data <- function(data, trait, resolution){
   s <- data %>%
         select(x = "x",  y = "y", z=!!rlang::sym(trait)) %>%
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
   df <- dz %>% 
         pivot_longer(-1, names_to = "y",values_to = "z")
   df$x <- as.double(df$x)
   df$y <- as.double(df$y)
   
   df
}





