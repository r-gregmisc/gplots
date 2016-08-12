# $Id$

# When there are two or more points with the same (x,y) value (or
# within x+-s[1] and x+-s[2]), space these out in the y direction so
# that the points are separated by at least distance s.


space <-  function(x,y,s=1/50, na.rm=TRUE, direction="x")
  {
    # to avoid duplicating code for direction="y", swap x and y temporarily...
    if(direction!='x')
      {
        tmp <- y
        y <- x
        x <- tmp
      }

    if(na.rm)
      {
        ind <- is.na(x) | is.na(y)
        x <- x[!ind]
        y <- y[!ind]
      }

    if (length(s)==1) s <- c(s,s)

    spacing.x <- (max(x) - min(x))*s[1]
    spacing.y <- (max(y) - min(y))*s[2]

    within <- function(x,y,delta) { abs(x-y) < delta }

    # sort x,y so we can do the work
    ord <- order(x,y)
    undo <- order(ord)

    x <- x[ord]
    y <- y[ord]

    # split into groups within the same interval
    sames.x <- c(FALSE, within(x[1:(length(x)-1)],
                               x[-1],
                               delta=spacing.x)
                 )
    sames.y <- c(FALSE, within(y[1:(length(y)-1)],
                               y[-1],
                               delta=spacing.y)
    )
    sames <- sames.x & sames.y
    groups <- cumsum(!sames)
    xList <- split(x, groups)
    yList <- split(y, groups)


    for( i in 1:max(groups) )
      {
      len <- length(xList[[i]])
      if(len==1)
        next

      m <- mean(xList[[i]])
      s <- len/2 * spacing.x
      deltas <- seq(from=-s, to=+s, length.out=len)
      order <- "permute"
      if (order=="permute")
        deltas <- sample(deltas)
      else if (order=="A")
        deltas <- deltas[order(abs(deltas), decreasing=TRUE)]
      else if (order=="V")
        deltas <- deltas[order(abs(deltas), decreasing=FALSE)]

      xList[[i]] <- xList[[i]] + deltas
    }

    x <- unlist(xList)[undo]
    y <- unlist(yList)[undo]

    # undo swap of x and y.
    if(direction!='x')
      {
        tmp <- y
        y <- x
        x <- tmp
      }

    return( list(x=x, y=y) )
}

