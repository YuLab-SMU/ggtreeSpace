#' @title Plot phylogenetic scatterplot matrix
#'
#' @description This function plot phylogenetic scatterplot matrix
#' @param tr tree
#' @param traits tipdata containig tip coordinates
#' @param mapping aesthetic mapping
#' @return phylomorphospace plot
#' @examples
#' tr <- rtree(15)
#' traits <- fastBM(tr, nsim = 5)
#' ggphylopsm(tr, triats)
#'
#' @export

### 参数列表
ggphylopsm <- function(tr, traits = NULL, ...)
{

  cat("Preparing phylogenetic scatterplot matrix, please wait....\n")
    
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
                          continuous = "color", size = 2) + ## size参数  
                    theme(legend.position = "none",
                          axis.line = element_line(color = "black")) +
                    scale_color_gradientn(colors=c("red", 'orange', 'green', 'cyan', 'blue')) ##颜色参数
      }
    else {
      suppressMessages(plst[[n]] <- ggtreeSpace(tr, traits[,c(i, j)]) + geom_tippoint() + coord_cartesian())
    }
  }
    GGally::ggmatrix(plst,
                     nc, nc) ###参数
}
