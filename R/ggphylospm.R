#' Plot phylogenetic scatterplot matrix
#'
#' @param tr 
#' @param traits 
#' @param title 
#' @param xAxisLabels 
#' @param yAxisLabels 
#' @param tr.params 
#' @param sptr.params 
#'
#' @return
#' @export
#'
#' @examples
ggphylopsm <- function(tr, traits = NULL, title = "Phylogenetic Scatterplot Matrix", 
                       xAxisLabels = NULL, yAxisLabels = NULL,
                       tr.params = list(size = 1, colors = NULL),
                       sptr.params = list(tippoint = TRUE, tiplab = FALSE, labdir = "horizon",
                                          panel.grid = FALSE))
{
  cat("Preparing phylogenetic scatterplot matrix, please wait....\n")
  
  if(is.null(traits))
    stop("Traits data is required.")
  
  if (is.null(tr.params$colors))
    tr.params$colors <- c("red", 'orange', 'green', 'cyan', 'blue')
  
  ## default.tr.params <- 
  
  
  ## default.sptr.params <- 

  
  nc <- ncol(traits)
  colnames(traits) <- c(paste("V", 1:nc, sep = ""))
  trd <- ggtree::fortify(tr)
  anc <- apply(traits, 2, fastAnc, tree = tr)
  
  ftrd <- trd %>%
          cbind(rbind(traits, anc))
  
  plst <- list()
  n = 0
  for (i in 1:nc) for (j in 1:nc){
    n = n + 1
    if (i == j) {
      plst[[n]] <- ggtree(ftrd, mapping = aes_string(color = paste("V", i, sep = "")), 
                          continuous = "color", size = tr.params$size) + 
                    theme(legend.position = "none",
                          axis.line = element_line(color = "black")) +
                    scale_color_gradientn(colors=tr.params$colors)
      }
    else {
      suppressMessages(p <- ggtreeSpace(tr, traits[,c(i, j)]) + coord_cartesian(default = T))
      
      if (sptr.params$tippoint){
        p + geom_tippoint()
      }
        
      
      if (sptr.params$tiplab){
        if(sptr.params$labdir == "horizonal")
          p + geom_tiplab(angle = 0)
        if(sptr.params$labdir == "radial")
          p + geom_tiplab()
      }
      
      if (sptr.params$panel.grid){
        p + theme_bw()
      }
      
      plst[[n]] <- p
    }
  }
    GGally::ggmatrix(plst,
                     nc, nc,
                     title = title, 
                     xAxisLabels = xAxisLabels, 
                     yAxisLabels = yAxisLabels,
                     ) 
}
