#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
coordtrans <- function(p, t){
  pd <- p$data
  pd$x <- pd$x - ((max(pd$x)-min(pd$x))/2)
  pd$y <- pd$y - ((max(pd$y)-min(pd$y))/2)
  pd$x <- pd$x*(((max(t) - min(t))/((max(pd$x)-min(pd$x)))))
  pd$y <- pd$y*(((max(t) - min(t))/((max(pd$y)-min(pd$y)))))
  pd$x <- pd$x - (min(pd$x)- min(t))
  pd$y <- pd$y - (min(pd$y) - min(t))
  p$data <- pd
  p <- p + 
    xlim(min(pd$x) - 0.05*(max(pd$x)-min(pd$x)), max(pd$x) + 0.05*(max(pd$x)-min(pd$x))) +
    ylim(min(pd$y) - 0.05*(max(pd$y)-min(pd$y)), max(pd$y) + 0.05*(max(pd$y)-min(pd$y)))
  return(p)
}





#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
lim_set <- function(p, df){
  p <- p + xlim(min(df[,1]) - 0.05*(max(df[,1])-min(df[,1])), max(df[,1]) + 0.05*(max(df[,1])-min(df[,1]))) +
           ylim(min(df[,2]) - 0.05*(max(df[,2])-min(df[,2])), max(df[,2]) + 0.05*(max(df[,2])-min(df[,2])))
  
  p
}




#' @importFrom rlang get_expr
set.params <- function(input, default){
  if (is.null(get_expr(input))) {
    return(NULL)
  }
  intdi <- intersect(names(input), names(default))
  setd <- setdiff(names(default), names(input))
  seti <- setdiff(names(input), names(default))
  intdi <- input[match(intdi, names(input))]
  setd <- default[match(setd, names(default))]
  seti <- input[match(seti, names(input))]
  newp <- c(intdi, setd, seti)
}


