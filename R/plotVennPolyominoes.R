
# Authors:  Steffen Möller steffen.moeller@uni-rostock.de> 
#           Sarah Fischer <sarah.fischer2@uni-rostock.de>
#
# This file defines a table of positions in a grid-like positioning
# of the Venn diagram's intersections. Those are referred to as
# polyominoes and are the default way to plot Venn diagrams for 
# 1 to 7 dimensions as presented on
#  http://www.combinatorics.org/files/Surveys/ds5/VennGeometricEJC.html.
#

# The following list features position tables for up to 7 dimensions.
# Positions for set sizes below are filled to allow the list to 
# have positions in the list match the dimension.
#
# The venn.polyominoes.positions.input matrix is manually edited.
# Following the defintion of this list, a small routine converts
# this input into another list with proper matrices that have
# rownames and that are numerical.
#
# This file can be sourced from a regular R session without additional
# requirements.

venn.polyominoes.positions.input <- list (

  A = matrix(c(#A
               "0",NA,NA,
               "1",1,1
	),byrow=T,ncol=3),

  AB = matrix(c(#AB
                "00",NA,NA,
                "10", 1, 1,
                "01", 3, 1,
                "11", 2, 1
	),byrow=T,ncol=3),

  ABC = matrix(c(#ABC
                 "000",NA,NA,
                 "001", 2, 5,
                 "010", 1, 4,
                 "011", 2, 4,
                 "100", 1, 1,
                 "101", 2, 3,
                 "110", 1, 2,
                 "111", 1, 3
        ),byrow=T,ncol=3),

  ABCD = matrix(c(#ABCD
                  "0000",NA,NA,	# remaining Universe
                  "0001", 5, 2,	# D
                  "0010", 5, 4,	# C
                  "0011", 1, 3,
                  "0100", 4, 5,	# B
                  "0101", 4, 2,
                  "0110", 4, 4,
                  "0111", 4, 3,
                  "1000", 2, 5,	# A
                  "1001", 2, 2,
                  "1010", 2, 4,
                  "1011", 2, 3,
                  "1100", 3, 1,
                  "1101", 3, 2,
                  "1110", 3, 4,
                  "1111", 3, 3
  	),byrow=T,ncol=3),

  ABCDE = matrix(c(#ABCDE
                   "00000",NA,NA,
                   "00001", 1, 6, # E
                   "00010", 5, 7, # D
                   "00011", 5, 6,
                   "00100", 6, 2, # C
                   "00101", 6, 6,
                   "00110", 6, 7,
                   "00111", 5, 5,
                   "01000", 2, 1, # B
                   "01001", 2, 2,
                   "01010", 2, 3,
                   "01011", 2, 4,
                   "01100", 2, 5,
                   "01101", 2, 6,
                   "01110", 5, 4,
                   "01111", 6, 5,
                   "10000", 7, 2, # A
                   "10001", 3, 6,
                   "10010", 7, 3,
                   "10011", 4, 3,
                   "10100", 5, 2,
                   "10101", 3, 5,
                   "10110", 6, 3,
                   "10111", 4, 6,
                   "11000", 3, 1,
                   "11001", 3, 2,
                   "11010", 4, 4,
                   "11011", 3, 3,
                   "11100", 6, 4,
                   "11101", 4, 5,
                   "11110", 5, 3,
                   "11111", 3, 4
  	),byrow=T,ncol=3),

  ### Defining position matrix for 6 sets 
  ABCDEF  = matrix(c(#ABCDEF
                     "000000",NA,NA,
                     "100000", 6,11, # A
                     "000100", 1, 6, # D
                     "100100", 2, 6,
                     "001001", 2, 7,
                     "101001", 3, 7,
                     "101101", 4, 7,
                     "011011", 5, 7,
                     "111110", 6, 7,
                     "111011", 7, 7,
                     "111010", 8, 7,
                     "110010", 9, 7,
                     "100010",10, 7,
                     "000010",11, 7, # E
                     "110100", 3, 6,
                     "110110", 4, 6,
                     "110111", 5, 6,
                     "111111", 6, 6,
                     "011111", 7, 6,
                     "011110", 8, 6,
                     "011100", 9, 6,
                     "011000",10, 6,
                     "010000",11, 6, # B
                     "010100", 3, 5,
                     "010110", 4, 5,
                     "010111", 5, 5,
                     "101110", 6, 5,
                     "111101", 7, 5,
                     "001111", 8, 5,
                     "010011", 9, 5,
                     "001010", 3, 4,
                     "101010", 4, 4,
                     "101011", 5, 4,
                     "101111", 6, 4,
                     "111001", 7, 4,
                     "001110", 8, 4,
                     "000110", 3, 3,
                     "100110", 4, 3,
                     "100111", 5, 3,
                     "101100", 6, 3,
                     "110001", 7, 3,
                     "001100", 8, 3,
                     "000111", 4, 2,
                     "001101", 5, 2,
                     "101000", 6, 2,
                     "100001", 7, 2,
                     "001000", 6, 1, # C
                     "000001", 7, 1, # F
                     "001011", 3, 8,
                     "110011", 4, 8,
                     "011010", 5, 8,
                     "111100", 6, 8,
                     "011101", 7, 8,
                     "011001", 8, 8,
                     "010001", 9, 8,
                     "100011", 4, 9,
                     "010010", 5, 9,
                     "111000", 6, 9,
                     "110101", 7, 9,
                     "100101", 8, 9,
                     "000101", 9, 9,
                     "000011", 4,10,
                     "110000", 6,10,
                     "010101", 7,10
                    ),byrow=T,ncol=3),

  ### Defining position matrix for 7 sets 
  ABCDEFG = matrix(c(#ABCDEFG
                     "0000000",NA,NA,
                     "0000001", 8, 1,
                     "0000010", 9,13,
                     "0000011",14, 4,
                     "0000100", 8,13,
                     "0000101",13, 6,
                     "0000110",14,12,
                     "0000111",14, 6,
                     "0001000", 1, 7,
                     "0001001",11,14,
                     "0001010",10,14,
                     "0001011",12,14,
                     "0001100",10, 2,
                     "0001101",13,14,
                     "0001110",12,10,
                     "0001111", 2, 5,
                     "0010000", 7, 1,
                     "0010001", 5, 2,
                     "0010010", 9, 2,
                     "0010011",12, 2,
                     "0010100", 6, 2,
                     "0010101", 6,12,
                     "0010110", 3, 2,
                     "0010111", 3, 3,
                     "0011000", 4, 1,
                     "0011001", 2, 4,
                     "0011010", 4, 3,
                     "0011011", 4, 4,
                     "0011100", 4, 2,
                     "0011101", 3, 4,
                     "0011110",11,10,
                     "0011111",10,10,
                     "0100000",13, 7,
                     "0100001",13, 9,
                     "0100010", 3, 6,
                     "0100011",11, 2,
                     "0100100",12, 6,
                     "0100101", 5,10,
                     "0100110", 3, 5,
                     "0100111", 4, 5,
                     "0101000",13, 8,
                     "0101001", 5, 9,
                     "0101010", 5, 8,
                     "0101011", 6, 8,
                     "0101100",12, 8,
                     "0101101", 6, 9,
                     "0101110",11, 8,
                     "0101111",10, 8,
                     "0110000",12, 7,
                     "0110001",12, 9,
                     "0110010", 4, 6,
                     "0110011",11, 3,
                     "0110100",11, 6,
                     "0110101", 6,10,
                     "0110110",10, 6,
                     "0110111", 9, 6,
                     "0111000",11, 7,
                     "0111001",11, 9,
                     "0111010", 5, 6,
                     "0111011", 6, 6,
                     "0111100",10, 7,
                     "0111101",10, 9,
                     "0111110", 9, 7,
                     "0111111", 8, 7,
                     "1000000", 7,13,
                     "1000001", 8, 2,
                     "1000010", 9,12,
                     "1000011",13, 4,
                     "1000100", 8,12,
                     "1000101",13, 5,
                     "1000110",13,12,
                     "1000111",14, 5,
                     "1001000", 2, 7,
                     "1001001",11,13,
                     "1001010",10,13,
                     "1001011",12,13,
                     "1001100",10, 3,
                     "1001101",13,13,
                     "1001110",10, 4,
                     "1001111",10, 5,
                     "1010000", 7, 2,
                     "1010001", 5, 3,
                     "1010010", 9, 3,
                     "1010011",12, 3,
                     "1010100", 6, 3,
                     "1010101", 6,11,
                     "1010110", 6, 4,
                     "1010111", 6, 5,
                     "1011000", 7, 3,
                     "1011001", 5, 4,
                     "1011010", 9, 4,
                     "1011011", 9, 5,
                     "1011100", 7, 4,
                     "1011101", 5, 5,
                     "1011110", 7, 5,
                     "1011111", 7, 6,
                     "1100000", 7,12,
                     "1100001", 8, 3,
                     "1100010", 9,11,
                     "1100011",12, 4,
                     "1100100", 8,11,
                     "1100101",12, 5,
                     "1100110",12,12,
                     "1100111",12,11,
                     "1101000", 3, 7,
                     "1101001",11,12,
                     "1101010",10,12,
                     "1101011",10,11,
                     "1101100", 4, 7,
                     "1101101",11,11,
                     "1101110", 5, 7,
                     "1101111", 6, 7,
                     "1110000", 7,11,
                     "1110001", 8, 4,
                     "1110010", 9,10,
                     "1110011",11, 4,
                     "1110100", 8,10,
                     "1110101",11, 5,
                     "1110110", 8, 9,
                     "1110111", 8, 8,
                     "1111000", 7,10,
                     "1111001", 8, 5, 
                     "1111010", 9, 9,
                     "1111011", 9, 8,
                     "1111100", 7, 9,
                     "1111101", 8, 6,
                     "1111110", 7, 8,
                     "1111111", 7, 7
                    ),byrow=T,ncol=3)
)


# Basic check for semantic integrity for single position matrix
venn.polyominoe.matrix.check <- function(m) {
  if (is.null(m)) return(NULL)
  if(any(duplicated(m[,1]))) stop("matrix describing polyominoes has redundant rownames") 
  if(any(duplicated(apply(m[,c(2,3),drop=F],1,function(X) {paste(X,collapse=",")})))) {
    stop("matrix describing polyominoes has duplicated positions")
  }
}

# Testing all position matrices - self-check
lapply(venn.polyominoes.positions.input,venn.polyominoe.matrix.check)

# Transforming single position matrix
venn.polyominoe.matrix.transform <- function(m) {
  if (is.null(m)) return(NULL)
  r <- matrix(as.numeric(m[,2:3],drop=F),ncol=2)
  dimnames(r)=list("Intersections"=m[,1],"Positions"=c("X","Y"))
  r
}
# Transforming all position matrices to define the venn.polyominoes.positions list.
venn.polyominoes.positions <- lapply(venn.polyominoes.positions.input,venn.polyominoe.matrix.transform)

plotVennPolyominoes <- function(x,y,...,
                                  col.bg="gray",
                                  col.scheme=rainbow,
                                  col.function=pV_colouringFeatureCount,
                                  fields.label.function=pV_fieldsLabelsBinary,
                                  fields.value.function=pV_fieldsValuesDirect,
                                  cex.label=0.5,cex.value=0.9,
                                  debug=F) {
  invisible(
            pV_draw(data=x,col.bg=col.bg,
                                col.scheme=col.scheme, col.function=col.function,
                                fields.label.function=fields.label.function,
                                fields.value.function=fields.value.function,
                                cex.label=cex.label, cex.value=cex.value, add=F, debug=debug)
  )
}

# assigns the number of features present in this set
pV_colouringFeatureCount <- function(current.rowname,data) {
  if (is.numeric(current.rowname)) current.rowname<-rownames(data)[current.rowname]
  nchar(gsub(x=current.rowname,pattern="0",replacement=""))
}

# present P value as percent integer value
pV_colouringPValue <- function(current.rowname,data,factor=100) {
  p.values <- attr(data,"p.values")
  if (is.null(p.values)) stop("pV_colouringPValue: is null")
  if (is.numeric(current.rowname)) current.rowname<-rownames(data)[current.rowname]
  round(p.values[current.rowname]*factor)
}


pV_fieldsLabelsLetter <- function(current.rowname,data,x,y,x.max,y.max,
                                                     cex=c(2,1,0.6,0.4,rep(0.3,length.out=(ncol(data)-1)))[ncol(data)],
                                                     debug=F)
{
  if (debug) cat("cex label=",cex,", ncol(data)-1=",ncol(data)-1,"\n",sep="")
  # if the index is passed, transform it to the rowname, which will be a string of "0" and "1"s
  if (is.numeric(current.rowname)) current.rowname<-rownames(data)[current.rowname]
  current.rowname.split<-strsplit(x=current.rowname,split="",fixed=TRUE)[[1]]
  new.rowname <- LETTERS[1:length(current.rowname.split)]
  new.rowname["0"==current.rowname.split]=" ";
  x_var=(x-1)/max((x.max-1),1)
  y_var=(y-1+0.25)/max((y.max-1),1)
  if (debug) cat("pV_fieldsLabelsLetter Label:",
  paste(new.rowname,collapse="",sep="")," in x variable",x_var,"with the y variable ",y_var," \n")
  text(x_var,y_var,paste(new.rowname,collapse="",sep=""),cex=cex,adj=c(0.5,0.5))
}

pV_fieldsLabelsBinary <- function(current.rowname,data,x,y,x.max,y.max,cex=c(1,1,0.3),
                                                     debug=F)
{
  if (is.numeric(current.rowname)) current.rowname<-rownames(data)[current.rowname]
  x_var=(x-1)/max((x.max-1),1)
  y_var=(y-1+0.27)/max((y.max-1),1)
  if (debug) cat("pV_fieldsLabelsBinary Label:",
                 current.rowname," in x variable ",x_var," with the y variable ",y_var,"\n",sep="")
  text(x_var,y_var,current.rowname,cex=cex,adj=c(0.5,0.5),offset=0)
}

pV_fieldsValuesDirect <- function(current.rowname,data,x,y,x.max,y.max,
                                                     #     1 2 3 4   5   6   7   8   9
                                                     cex=c(4,3,2,1,0.6,0.6,0.4,0.4,0.3)[ncol(data)-1],
                                                     debug=F)
{
  if (debug) cat("cex fields=",cex,", ncol(data)-1=",ncol(data)-1,"\n",sep="")
  if (is.numeric(current.rowname)) current.rowname<-rownames(data)[current.rowname]
  x_var=(x-1)/max((x.max-1),1)
  y_var=(y-1-0.1)/max((y.max-1),1)
  if (debug) cat("Value:",current.rowname," in x variable",x_var," with the y variable ",y_var," \n")
  text(x_var,y_var,data[current.rowname,1],cex=cex,adj=c(0.5,0.5),offset=0)
}

pV_draw <- function(data,col.bg="lightgray",
                                col.scheme=NULL,
                                col.function=NULL,
                                fields.label.function=pV_fieldsLabelsBinary,
                                fields.value.function=pV_fieldsValuesDirect,
                                cex.label=0.5,cex.value=0.9, add=F, h=400,
                                debug=F)
{

  if (is.null(col.scheme)) {
    if(is.null(attr(data,"p.values"))){
      col.function=rainbow
    } else {
      col.function=colorRampPalette(c("green","white","red"))
    }
  }

  if (! "num" %in% colnames(data)) {
    stop("pV_draw: data needs to contain 'num' column as obtained from 'getVennCounts()'")
  }

  ndim <- ncol(data)-1
  if (debug) {
  	cat("head(data)\n")
  	print(head(data,5))
  	cat("ndim=",ndim,"\n")
  }
	
  if (is.null(data)) stop("pV_draw: data is null")
	
  if (is.null(col.function)) {
    if(is.null(attr(data,"p.values"))) {
      col.function <- pV_colouringFeatureCount
    } else {
      col.function <- pV_colouringPValue
    }
  }

  venn.positions <- venn.polyominoes.positions[[ndim]]

  if (is.null(venn.positions)) stop(paste("Cannot plot Venn Polyominoes with dimension ",ndim,".",sep=""))

  if (debug) print(head(venn.positions))

  x.max<-max(venn.positions[,"X"],na.rm=T)
  y.max<-max(venn.positions[,"Y"],na.rm=T)

  if (debug) cat("x.max=",x.max,", y.max=",y.max,"\n")
	
  m.plotme <- matrix(NA,ncol=y.max,nrow=x.max)
  for(i in rownames(venn.positions)) {
    if (1==regexpr(i,pattern="^0+$")) next()
    pos<-venn.positions[i,c("X","Y")]
    if (any(is.na(pos))) {
      warning("Retrieved NA for venn.position at ",i,".")
      next()
    }
    #cat("dim(pos):      "); print(pos)
    #cat("dim(m.plotme): "); print(dim(m.plotme))
    if(any(c(pos[1],pos[2])>dim(m.plotme))) {
      stop("E: Asked to put values at pos (",paste(pos,collapse=","),") with dim(m.plotme) = (",paste(dim(m.plotme),collapse=","),").\n")
    }
    m.plotme[pos[1],pos[2]] <- col.function(current.rowname=i,data=data)
    if (debug) cat("m.plotme[",pos[1],",",pos[2],"]=",m.plotme[pos[1],pos[2]],"\n")
  }

  if (is.null(attr(data,"p.values"))) {
    image(z=m.plotme,col=col.scheme(ndim),axes=FALSE,add=add,bg=col.bg)
  } else {
    image(z=m.plotme,col=col.scheme(100),axes=FALSE,add=add,bg=col.bg)
  }

  for(k in (2:nrow(venn.positions)) ){
    # skipping first entry for 'all 0s' with NA positions
    if (debug) cat("k=",k," Text :",rownames(venn.positions)[k],
	                 " in x variable",venn.positions[k,1] ,
	                 "with the y variable ",venn.positions[k,2]," \n")
    fields.label.function(current.rowname=rownames(venn.positions)[k],
                          data=data,
                          x=venn.positions[k,1],
                          y=venn.positions[k,2],
                          x.max=x.max, y.max=y.max, cex=cex.label)
    fields.value.function(current.rowname=rownames(venn.positions)[k],
                          data=data,
                          x=venn.positions[k,1],
                          y=venn.positions[k,2],
                          x.max=x.max, y.max=y.max, cex=cex.value)
  }

  invisible(data)
}


