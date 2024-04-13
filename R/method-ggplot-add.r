##' @importFrom ggplot2 ggplot_add
##' @export
##' @return ggplot object
ggplot2::ggplot_add


#' @method ggplot_add treespace
#' @export
ggplot_add.treespace <- function(object, plot, object_name) {
    st_layer <- do.call(make_ts_layer, object)
    plot$layers <- append(plot$layers, st_layer)
    return(plot)
}


#' @method ggplot_add tsheatmap
#' @export
ggplot_add.tsheatmap <- function(object, plot, object_name) {
    l <- list(data = plot$data)
    object <- c(l, object)
    hm_layer <- do.call(make_hm_layer, object)
    plot$layers <- append(plot$layers, hm_layer)
    return(plot)
}
