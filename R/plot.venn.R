# Authors: Steffen Moeller

plot.venn <- function(x, y, ...,
                      small=0.7,
                      showSetLogicLabel=FALSE,
                      simplify=FALSE,
                      type="regular",
                      col=NA,
                      density=NULL,
                      border=NULL,
                      angle=NA,
                      lty=par("lty"),
                      debug=F
                     )
{
  if (!is.matrix(x)) stop("Please have a true matrix passed to the 'plot.venn' method.")
  if (is.logical(type)) {
      if (!isTRUE(type)) return;
  }

  if (is.list(type)) {
      for (type.iterator in type) {
          plot.venn(x=x,y=y,small=small,showSetLogicLabel=showSetLogicLabel,simplify=simplify,type=type.iterator)
      }
  }

  if (sum(baseOf(nrow(x),2))!=1) stop(paste(c("Parameter x is expected to describe every possible",
                 "intersections, including the empty set, the number of rows is hence a power of 2.",
                 "Please retrieve the counts from the venn function which can also print directly."),collapse=" ",sep=""))

  if (ncol(x)-1<6 && "regular"==type)
     r <- drawVennDiagram(
            data=x,
            small=small,
            showSetLogicLabel=showSetLogicLabel,
            simplify=simplify,
            col=col, density=density, border=border, angle=angle, lty=lty,
            debug=debug)
  else if ("circular"== type)
     r <- drawVennDiagram(
            data=x,
            small=small,
            showSetLogicLabel=showSetLogicLabel,
            simplify=TRUE,
            col=col, density=density, border=border, angle=angle, lty=lty,
            debug=debug)
  else if ("polyominoes"==type || ncol(x)-1<8 && ncol(x)-1>5 ) {
     r <- drawVennPolyominoes(
            data=x, debug=debug)
  }
  else if ("graph"==type) {
     if (simplify) {
       # simplification should possibly go here
     }
     r <- plot.venn.graph(data=x, col=col, debug=debug)
  }
  else if (!is.logical(type)) {
    stop(paste("Do not know how to plot Venn diagrams of type '",type,"'.",sep=""))
  }

  invisible(r)

}


create.new.plot.window <- function (h) {
  plot.new()
  plot.window(c(0,h), c(0,h), ylab="", xlab="")
}


get.ith.value <- function(l,i) {
  if (is.null(l)) return(NULL)
  rep(l,length.out=i)[i]
}


#
# drawVennCircles - helper function for vennDiagram
#

drawVennCircles <- function(data,
                            small=0.7,
                            showSetLogicLabel=FALSE,
                            col=NA,
                            density=NULL,
                            border=NULL,
                            angle=NA,
                            lty=par("lty"),
                            h=400, debug=FALSE)
{

  if (debug) cat("drawVennCircles: enter with ",
                 "col=(",    paste(col,collapse=",",sep=""),   "), ",
                 "angle=(",  paste(angle,collapse=",",sep=""), "), ",
                 "border=(", paste(border,collapse=",",sep=""),")\n",
                 sep="")

  numCircles<-ncol(data)-1
  data.rownames<-rownames(data)
  data.colnames<-colnames(data)[2:(ncol(data))]

  create.new.plot.window(h)

  circle <- function(x,y=NULL,r=1,
                     col=NA, density=NULL, border=NULL, angle=NA, lty=par("lty")) {
    elps=cbind(r*cos(seq(0,2*pi,len=1000)), r*sin(seq(0,2*pi,len=1000)));
    if (!is.null(y)) {
      if (length(x) != length(y))
        stop("circle: both x and y need to be of same length")
      if (is.matrix(x) && ncol(x)>1)
        stop("circle: if y is not NULL, then x must not be a matrix")
      x<-cbind(x,y)
    }
    for(i in 1:nrow(x)) {
      ax<-elps[,1]+rep(x[i,1],1000)
      ay<-elps[,2]+rep(x[i,2],1000)
      d <- density
      if (!is.null(density)) d <- rep(density,length.out=nrow(x))[i]
      polygon(ax, ay,
                  col=get.ith.value(col,i),
              density=get.ith.value(density,i),
               border=get.ith.value(border,i),
                angle=get.ith.value(angle,i),
                  lty=get.ith.value(lty,i)
             )
    }
  }

  ##cat("drawing circles\n")
  # draw circles with radius 1.7 equally distributed
  # with centers on a circle of radius 1

  degrees<-2*pi/numCircles*(1:numCircles)

  # scaling factor
  s<-1/8*h
  # radius for circles
  r<-3/12*h

  x<-sapply(degrees,FUN=sin)*s + 0.5*h
  y<-sapply(degrees,FUN=cos)*s + 0.5*h

  ##cat("filling data\n")
  circle(x=x, y=y, r=r,
        col=col,
        density=density, border=border, angle=angle, lty=lty)

  distFromZero<-rep(NA,2^numCircles)
  degrees<-rep(NA,2^numCircles)

  degrees[(2^numCircles)]<-0
  distFromZero[(2^numCircles)]<-0

  for (i in 0:(numCircles-1)) {
    distFromZero[2^i+1] <- r
    degrees[2^i+1] <- 2*pi/numCircles*i
    d<-degrees[2^i+1]

    text(
        # starting from the lowest bit, hence reading
        # lables from the right
        label=data.colnames[numCircles - i],
        x=sin(d)*5/12*h+0.5*h,
        y=cos(d)*5/12*h+0.5*h
    )
  }

  if (4==numCircles) {
    for (i in 0:(numCircles-1)) {
        # Current set bit plus the bit left of it and the bit right of it
        distFromZero[2^i +2^((i+numCircles-1)%%numCircles) +2^((i+1)%%numCircles)+1] <- 2/12*h
        distFromZero[2^i+1] <- 3.5/12*h
        degrees[2^i +2^((i+numCircles-1)%%numCircles) +2^((i+1)%%numCircles)+1] <- degrees[2^i+1]
    }
  }

  if (3 <=numCircles) {
    for (i in 0:(numCircles-1)) {
      distFromZero[(2^i+2^((i+1)%%numCircles))+1]<- 2.2/12*h
      distFromZero[2^i+1] <- 3/12*h
      if (i == (numCircles-1)) {
        degrees[(2^i+2^((i+1)%%numCircles))+1] <- (degrees[2^i+1] + 2*pi+ degrees[1+1])/2
      }
      else {
        degrees[(2^i+2^((i+1)%%numCircles))+1] <- (degrees[2^i+1] + degrees[2^((i+1)%%numCircles)+1])/2
      }
    }
  }

  for(i in 1:2^numCircles) {
    n<-paste(baseOf((i-1),2,numCircles),collapse="")
    v<-data[n,1]
    d<-degrees[i]
    if (1 == length(d) && is.na(d)) {
      if (v>0) warning("Not shown: ",n," contains ",v,"\n")
    }
    else {
      l<-distFromZero[i]
      x<-sin(d)*l+0.5*h
      y<-cos(d)*l+0.5*h
      #cat("i=",i," x=",x," y=",y," label=",n,"\n")
      l<-v
      if (showSetLogicLabel) l<-paste(n,"\n",v,sep="")
      text(label=l,x=x,y=y)
    }
  }
}


#  drawVennEllipses - helper function for vennDiagram
#
drawVennEllipses <- function(data,
                             small=0.7,
                             showSetLogicLabel=FALSE,
                             col=NA,
                             density=NULL,
                             border=NULL,
                             angle=NA,
                             lty=par("lty"),
                             h=400, debug=F)
{

  if (debug) cat("drawVennCircles: enter with ",
                 "col=(",    paste(col,collapse=",",sep=""),   "), ",
                 "angle=(",  paste(angle,collapse=",",sep=""), "), ",
                 "border=(", paste(border,collapse=",",sep=""),")\n",
                 sep="")

  numCircles <- ncol(data)-1
  if (4  > numCircles || 5 < numCircles)
    stop("drawVennEllipses only knows to draw for 4 or 5 dimensions")

  data.rownames<-rownames(data)
  data.colnames<-colnames(data)[2:(ncol(data))]

  create.new.plot.window(h)

  # Function to turn and move ellipses/circles
  relocate_elp <- function(e, alpha, x, y){
    phi=(alpha/180)*pi;
    xr=e[,1]*cos(phi)+e[,2]*sin(phi)
    yr=-e[,1]*sin(phi)+e[,2]*cos(phi)
    xr=x+xr;
    yr=y+yr;
    cbind(xr, yr)
  }

  lab<-function (identifier, data, showLabel=showSetLogicLabel) {
    r<-data[identifier,1]
    if (showLabel) {
      return(paste(identifier,r,sep="\n"))
    }
    r
  }


  if (4 == numCircles) {
    elps=cbind(162*cos(seq(0,2*pi,len=1000)), 108*sin(seq(0,2*pi,len=1000)));

    polygon(relocate_elp(elps, 45,130,170),   col=get.ith.value(col,1),
            density=get.ith.value(density,1), border=get.ith.value(border,1),
            angle=get.ith.value(angle,1),     lty=get.ith.value(lty,1))
    polygon(relocate_elp(elps, 45,200,200),   col=get.ith.value(col,2),
            density=get.ith.value(density,2), border=get.ith.value(border,2),
            angle=get.ith.value(angle,2),     lty=get.ith.value(lty,2))
    polygon(relocate_elp(elps,135,200,200),   col=get.ith.value(col,3),
            density=get.ith.value(density,3), border=get.ith.value(border,3),
            angle=get.ith.value(angle,3),     lty=get.ith.value(lty,3))
    polygon(relocate_elp(elps,135,270,170),   col=get.ith.value(col,4),
            density=get.ith.value(density,4), border=get.ith.value(border,4),
            angle=get.ith.value(angle,4),     lty=get.ith.value(lty,4))

    text( 35, 315, data.colnames[1],cex=1.5)
    text(138, 347, data.colnames[2],cex=1.5)
    text(262, 347, data.colnames[3],cex=1.5)
    text(365, 315, data.colnames[4],cex=1.5)

    elps <- cbind(130*cos(seq(0,2*pi,len=1000)),
                  80*sin(seq(0,2*pi,len=1000)))

    text( 35, 250, lab("1000",data));
    text(140, 315, lab("0100",data));
    text(260, 315, lab("0010",data));
    text(365, 250, lab("0001",data));

    text( 90, 280, lab("1100",data), cex=small)
    text( 95, 110, lab("1010",data) )
    text(200,  50, lab("1001",data), cex=small)
    text(200, 290, lab("0110",data))
    text(300, 110, lab("0101",data))
    text(310, 280, lab("0011",data), cex=small)

    text(130, 230, lab("1110",data))
    text(245,  75, lab("1101",data),cex=small)
    text(155,  75, lab("1011",data),cex=small)
    text(270, 230, lab("0111",data))

    text(200,150,lab("1111",data))
  }
  else if (5 == numCircles) {

    elps <- cbind(150*cos(seq(0,2*pi,len=1000)),
                  60*sin(seq(0,2*pi,len=1000)))

    polygon(relocate_elp(elps, 378, 145, 200), col=get.ith.value(col,1),
            density=get.ith.value(density,5),  border=get.ith.value(border,1),
            angle=get.ith.value(angle,5),      lty=get.ith.value(lty,1))

    polygon(relocate_elp(elps, 306, 180, 125), col=get.ith.value(col,5),
            density=get.ith.value(density,4),  border=get.ith.value(border,5),
            angle=get.ith.value(angle,4),      lty=get.ith.value(lty,5))

    polygon(relocate_elp(elps, 234, 250, 150), col=get.ith.value(col,4),
            density=get.ith.value(density,3),  border=get.ith.value(border,4),
            angle=get.ith.value(angle,3),      lty=get.ith.value(lty,4))

    polygon(relocate_elp(elps, 162, 250, 220), col=get.ith.value(col,3),
            density=get.ith.value(density,2),  border=get.ith.value(border,3),
            angle=get.ith.value(angle,2),      lty=get.ith.value(lty,3))

    polygon(relocate_elp(elps,  90, 200, 250), col=get.ith.value(col,2),
            density=get.ith.value(density,1),  border=get.ith.value(border,2),
            angle=get.ith.value(angle,1),      lty=get.ith.value(lty,2))

    text( 20, 295, data.colnames[1],cex=1.5)
    text(140, 380, data.colnames[2],cex=1.5)
    text(350, 318, data.colnames[3],cex=1.5)
    text(350,   2, data.colnames[4],cex=1.5)
    text( 50,  10, data.colnames[5],cex=1.5)

    text( 61, 228, lab("10000",data));
    text(194, 329, lab("01000",data));
    text(321, 245, lab("00100",data));
    text(290,  81, lab("00010",data));
    text(132,  69, lab("00001",data));

    text(146, 250, lab("11000",data), cex=small)
    text(123, 188, lab("10100",data), cex=small)
    text(275, 152, lab("10010",data), cex=small)
    text(137, 146, lab("10001",data), cex=small)
    text(243, 268, lab("01100",data), cex=small)
    text(175, 267, lab("01010",data), cex=small)
    text(187, 117, lab("01001",data), cex=small)
    text(286, 188, lab("00110",data), cex=small)
    text(267, 235, lab("00101",data), cex=small)
    text(228, 105, lab("00011",data), cex=small)

    text(148, 210, lab("11100",data),cex=small)
    text(159, 253, lab("11010",data),cex=small)
    text(171, 141, lab("11001",data),cex=small)
    text(281, 175, lab("10110",data),cex=small)
    text(143, 163, lab("10101",data),cex=small)
    text(252, 145, lab("10011",data),cex=small)
    text(205, 255, lab("01110",data),cex=small)
    text(254, 243, lab("01101",data),cex=small)
    text(211, 118, lab("01011",data),cex=small)
    text(267, 211, lab("00111",data),cex=small)

    text(170, 231,lab("11110",data),cex=small)
    text(158, 169,lab("11101",data),cex=small)
    text(212, 139,lab("11011",data),cex=small)
    text(263, 180,lab("10111",data),cex=small)
    text(239, 232,lab("01111",data),cex=small)

    text(204,190,lab("11111",data))
  }

  invisible(data)
}


## data should be a matrix.
##   - The first column of the matrix is the
##     count of the number of objects with the specified pattern.
##   - The second and subsequent columns contain 0-1 indicators
##     giving the pattern of group membership


drawVennDiagram <-function(data,
                           small=0.7,
                           showSetLogicLabel=FALSE,
                           simplify=FALSE,
                           col=NA,
                           density=NULL,
                           border=NULL,
                           angle=NA,
                           lty=par("lty"),
                           h=400,debug=F)
{

  if(! is.matrix(data) || ! "num" %in% colnames(data)) {
    stop("gplots.drawVennDiagram: This internal function is used wrongly. ",
         "Please call the function 'venn' with the same arguments, instead.")
                #FIXME: Need to revisit this comment
                # Order is reverted since later indexing starts with
                # the "lowest bit" and that is expected at the left
  }

  numCircles <- ncol(data)-1

  r <- NULL

  if ((1 <= numCircles && numCircles <= 3) || (4 <= numCircles && simplify)) {
    r <- drawVennCircles(data, small=small,
                         showSetLogicLabel=showSetLogicLabel,
                         col=col, density=density, border=border, angle=angle, lty=lty, h=h, debug=debug)
  }
  else if ( (4 == numCircles && !simplify) || numCircles <= 5 ) {
    r <- drawVennEllipses(data, small=small,
                          showSetLogicLabel=showSetLogicLabel,
                          col=col, density=density, border=border, angle=angle, lty=lty, h=h, debug=debug)
  }
  else if (    ("polyominoes"==type && numCircles <= 7)
            || ( "regular"==type    && 6 <= numCircles && numCircles <= 7)) {
    # no new plot window required
    r <- drawVennPolyominoes(data, add=F, h=h, debug=debug)
  }
  else if ( "graph"==type && numCircles <= 7) {
    r <- drawVennGraph(data, add=F, simplify=simplify, debug=debug)
  }
  else {
    warning("For more than 7 dimensions get the graph attribute and manually simplify it.")
    r <- drawVennGraph(data, add=F, simplify=simplify, debug=debug)
  }
  r
}
