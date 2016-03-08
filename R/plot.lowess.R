plot.lowess <- function (formula, data = parent.frame(), ..., subset=parent.frame(), col.lowess="red", lty.lowess=2  )
{
  m <- match.call(expand.dots=TRUE)
  m[[1]] <- as.name("plot")
  eval(m)
  m[[1]] <- as.name("lowess")
  lw <- eval(m)
  lines(lw, col=col.lowess, lty=lty.lowess)  
  grid()
}
