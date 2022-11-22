### theme_treeSpace

#' @importFrom ggplot2 element_blank
theme_treeSpace <- function(...){
    ggplot2::theme_bw(...) +
    ggplot2::theme(panel.grid = element_blank())
}

