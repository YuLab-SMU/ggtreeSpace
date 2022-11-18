### gp_trans
#' Plot a phylomorphospace with a gm.prcomp object
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
# gp_trans <- function(object, ...){
#   
# }



#### coordination transition
# coordtrans <- function(p, t){
#   pd <- p$data
#   pd$x <- pd$x - ((max(pd$x)-min(pd$x))/2)
#   pd$y <- pd$y - ((max(pd$y)-min(pd$y))/2)
#   pd$x <- pd$x*(((max(t) - min(t))/((max(pd$x)-min(pd$x)))))
#   pd$y <- pd$y*(((max(t) - min(t))/((max(pd$y)-min(pd$y)))))
#   pd$x <- pd$x - (min(pd$x)- min(t))
#   pd$y <- pd$y - (min(pd$y) - min(t))
#   p$data <- pd
#   p <- p + 
#     theme(aspect.ratio = 1) +
#     xlim(min(pd$x) - 0.05*(max(pd$x)-min(pd$x)), max(pd$x) + 0.05*(max(pd$x)-min(pd$x))) +
#     ylim(min(pd$y) - 0.05*(max(pd$y)-min(pd$y)), max(pd$y) + 0.05*(max(pd$y)-min(pd$y)))
#   return(p)
# }
