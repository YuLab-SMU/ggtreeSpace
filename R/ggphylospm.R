#' @title Plot phylogenetic scatterplot matrix
#'
#' @param tr A phylogenetic tree
#' @param traits A data frame containing 
#' @param title Set the title for the phylogenetic scatterplot matrix
#' @param xAxisLabels Set the lable of the x axis
#' @param yAxisLabels Set the lable of the y axis
#' @param tr.params List of parameters to customize the phylogenetic tree with continuous trait mapping as continuous colors on the branch
#' @param sptr.params List of parameters to customize the phylomorphospaces
#'
#' @return
#'
#' @examples
#' library(ggtree)
#' library(phytools)
#' 
#' tr <- rtree(10)
#' a <- fastBM(tr, nsim = 4)
#' 
#' phylospm(tr, a)
#' @export
phylospm <- function(tr, traits = NULL, title = "Phylogenetic Scatterplot Matrix", 
                     xAxisLabels = NULL, yAxisLabels = NULL,
                     tr.params = list(size = 1, colors = NULL, panel.grid = TRUE),
                     sptr.params = list(tippoint = TRUE, tiplab = FALSE, labdir = "horizonal",
                                          panel.grid = FALSE))
{
  options(warn = -1)
  
  cat("Preparing phylogenetic scatterplot matrix, please wait...\n")
  
  if(is.null(traits))
    stop("Traits data is required.")
  
  if (is.null(tr.params$colors))
    tr.params$colors <- c("red", 'orange', 'green', 'cyan', 'blue')
  
  default.tr.params <- list(size = 1, 
                            colors = c("red", 'orange', 'green', 'cyan', 'blue'),
                            panel.grid = TRUE)
  new.tr.params <- set.params(tr.params, default.tr.params)
  

  default.sptr.params <- list(tippoint = TRUE, 
                              tiplab = FALSE, 
                              labdir = "horizonal",
                              panel.grid = FALSE)
  new.sptr.params <- set.params(sptr.params, default.sptr.params)
  
  
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
      suppressMessages(p <- ggtree(ftrd, mapping = aes_string(color = paste("V", i, sep = "")), 
                                   continuous = "color", size = new.tr.params$size) + 
                              scale_color_gradientn(colors = new.tr.params$colors) +
                              coord_cartesian(default = T) +
                              theme_treeSpace())
      
      suppressMessages(p <- p + coordtrans(p, traits[,i]))
      
      if (new.tr.params$panel.grid){
        p <- p + theme_bw()
      }
      plst[[n]] <- p
      
      }
    else {
      suppressMessages(p <- ggtreeSpace(tr, traits[,c(j, i)]) + 
                              coord_cartesian(default = T))
      
      p <- lim_set(p, traits[,c(j, i)])
      
      if (new.sptr.params$tippoint){
        p <- p + geom_tippoint()
      }
        
      if (new.sptr.params$panel.grid){
        p <- p + theme_bw()
      }
      
      if (new.sptr.params$tiplab){
        if(new.sptr.params$labdir == "horizonal")
            p <- p + geom_text_repel(mapping = aes(label = label), 
                                     data = td_filter(is.tip = TRUE), angle = 0)
        if(new.sptr.params$labdir == "radial")
            p <- p + geom_text_repel(mapping = aes(label = label), 
                                     data = td_filter(is.tip = TRUE))
      }
      

      plst[[n]] <- p
    }
  }
    ### change spm method
    spm <- GGally::ggmatrix(plst,
                            nc, nc,
                            title = title, 
                            xAxisLabels = xAxisLabels, 
                            yAxisLabels = yAxisLabels,
                            ) 
    
    class(spm) <- c("phylospm", class(spm))
    
    spm
}
