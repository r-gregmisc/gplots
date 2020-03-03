# This code plots Venn Diagrams for up to 5 sets,
# Polyominoes for up to 7 sets, and graphs for any
# number of dimensions.
# The function getVennCounts is passed a list of vectors.
# This is transformed into a table indicating the
# number of intersections for each intersection. This table
# is generated for any number of sets.

# The function drawVennDiagram plots circles (up to three
# sets) or ellipses (4 and 5 sets) to depict the sets.
# The sum of values placed is the number of entries of
# each set.

# Function to determine values of a venn diagram
# It works for an arbitrary large set of input sets.
#
# Authors: Steffen Moeller, Sarah Fischer, Greg Warnes

getVennCounts <- function(l, universe, verbose=FALSE, permute=FALSE, intersections=TRUE, ...)
{
  UseMethod("getVennCounts")
}

getVennCounts.data.frame <- function(l, universe=NA, verbose=FALSE, permute=FALSE, intersections=TRUE, ...) {
    if (verbose) cat("Interpreting data as data.frame.\n")
    if (permute && intersections) stop("Do not ask both for intersections and permutations - mostly pointless")
    if( !all(unique(unlist(l)) %in% c(0,1))  )
      stop("Only indicator columns permitted")

    if (is.na(universe)) {
       if (is.null(rownames)) {
           universe=1:nrow(l)
       } else {
           universe=rownames(l)
       }
    }


    if (permute) {
        # all columns are arranged in a new order
        #if (verbose) cat("dim(l) before permutation: (",paste(dim(l),collapse=",",sep=""),")\n",sep="")
        l <- apply(l,2,sample)
        l <- as.data.frame(l)
        #if (verbose) cat("dim(l) after  permutation: (",paste(dim(l),collapse=",",sep=""),")\n",sep="")
    }

    l <- lapply( l, function(x) which(as.logical(x)))

    getVennCounts.list(l, universe=universe, verbose=verbose, intersections=intersections, permute=F)
}

getVennCounts.matrix <- function(l, universe=NA, verbose=FALSE, permute=FALSE, intersections=TRUE, ...) {
    if (verbose) cat("Interpreting data as matrix.\n")
    getVennCounts.data.frame(as.data.frame(l), universe=universe, verbose=verbose, intersections=intersections, permute=permute, ...)
}

# l offers a list of arrays, their values are to
# be tested for the size of their intersects.
getVennCounts.list<-function(l, universe=NA, verbose=FALSE, intersections=TRUE, permute=FALSE) {

    if (permute) stop("getVennCounts.list: Permutations are yet only implemented for incidence matrices")

    if (verbose) cat("Interpreting data as list.\n")
    numSets<-length(l)
    result.table<-NULL
    result.table.names<-NULL

    memberList <- list()

    if (verbose) cat("numSets: ",numSets,"\n",sep="")

    # Iteration over all possible intersections involving all sets
    # or the complement (negation) of those sets.

    for (i in 0:(-1 + 2^numSets)) {
        # i2 is a binary representation of that number
        i2<-baseOf(i,2,numSets)

        sel<-universe

        # positive selection first
        for (p.pos in which(1 == i2) ) {
            current.set<-l[[p.pos]]
            if (!is.null(dim(current.set))) {
                # circumventing strange experiences with data.frames
                warning(paste("List element [[",p.pos,"]] has dimensions, but all elements are considered.\n",sep=""))
                current.set<-as.character(as.matrix(current.set))
                dim(current.set)<-NULL
            }
            #print(paste("set ",p.pos,", val=1: ",paste(current.set,collapse=",")))
            if (is.null(sel)) {
                #print("Sel is null")
            } else if (1 == length(sel) && is.na(sel)) {
                sel<-current.set
            }
            else {
                w<-which(sel %in% current.set)
                if (length(w)>0) {
                    sel<-sel[w]
                }
                else {
                    sel<-NULL
                }
            }
        }

        # something should be in sel now, otherwise
        # the number will be 0

        # negative selection
        for (p.pos in which(0 == i2) ) {
            if (is.null(sel) || ( 1 == length(sel) && is.na(sel))) {
              # The complement is not known, hence no checks done
            }
            else {
                current.set<-l[[p.pos]]
                if (!is.null(dim(current.set))) {
                    warning(paste("List element [[",p.pos,"]] has dimensions, but all elements are considered.\n",sep=""))
                    current.set<-as.character(as.matrix(current.set))
                    dim(current.set)<-NULL
                }
                w<-which( ! sel %in% current.set)
                #print(paste("set ",p.pos,", val=1: ",paste(current.set,collapse=",")))
                if (length(w)>0) {
                    sel<-sel[w]
                }
                else {
                    sel<-NULL
                }
            }
        }
        #print(paste("sel:",paste(sel,collapse=",")))

        if(is.null(sel) || (1 == length(sel) && is.na(sel))) {
            sel<-NULL
        }

        r.name<-paste(i2,collapse="")
        if (intersections) {
            memberList[[r.name]] <- sel
        }

        r<-length(sel)
        result.row<-c(r,i2)
        dim(result.row)<-c(1,length(result.row))
        rownames(result.row)<-c(r.name)
        #print(paste("Adding ",r.name))
        if (is.null(result.table)) {
          result.table<-result.row
        }
        else {
          result.table<-rbind(result.table,result.row)
        }
    }

    #names(result.table)<-result.table.names
    if (is.null(names(l))) {
        colnames(result.table)<-c("num",LETTERS[1:numSets])
    }
    else {
        colnames(result.table)<-c("num",names(l))
    }
    if (intersections) {
        attr(result.table,"intersections") <- memberList
    }
    class(result.table) <- "venn"
    result.table
}

venn <- function(data,
                 universe=NA,
                 small=0.7,
                 showSetLogicLabel=FALSE,
                 simplify=FALSE,
                 show.plot=TRUE,
                 intersections=TRUE,
                 graph=FALSE,
                 names=NULL,
                 statistics=NULL,
                 n.perm=100,
                 ...
                 )
{
    counts <- getVennCounts(data,
                            universe=universe,
                            intersections=intersections,
                            permute=FALSE)

    if (!is.null(statistics) && "permutations"==statistics) {
      perm <- rep(NA,length(counts)*n.perm)
      dim(perm)<-c(dim(counts),n.perm)
      dimnames(perm) <- dimnames(counts)
      dimnames(perm)[[3]] <- NULL
      names(dimnames(perm)) <- c("set","value","permutation")
      cat("Creating ",n.perm," permutations ",sep="")
      for(i in 1:n.perm) {
        if (0 == (i%%100)) {
          cat(i)
        } else if (0== i%%10) {
          cat(":")
        } else if (0== i%%2) {
          cat(".")
        }
        perm[,,i] <- getVennCounts(l=data,intersections=FALSE,permute=TRUE,verbose=FALSE)
      }
      p.values <- sapply(names(counts[,"num"]),function(X) {
        v<-perm[X,"num",]
            # example: counts?[X,"num"] ==
            #          v: 123123123123132132 -> p=0
            #          v: 567567567567567567 -> p=1
        return(sum( counts[X,"num"]<=v )/length(v))
      })
      attr(counts, "p.values") <- p.values
      attr(counts, "permutations") <- perm
      rm(perm,p.values)
      cat("\n")
    }

    venn.graph <- NA
    if (graph) {
      venn.graph <- getVennGraph(counts,...)
      attr(counts, "graph") <- venn.graph
    }

    if (!is.null(show.plot)) {
        for (sp in show.plot) {
            if (is.logical(sp)) {
                if (isTRUE(sp)) sp <- "regular"
                else next();
            }
            plot.venn(x=counts,
                      small=small,
                      showSetLogicLabel=showSetLogicLabel,
                      simplify=simplify,
                      type=sp,
                      ...)
        }
    }

    # use VennMemberNames to properly label and order the 'intersection' table
    if (intersections) {
        attr(counts, "intersections") <- vennMembers(l=data,
                                                     universe=universe,
                                                     set.names=names)
    }
    invisible(counts)
}


# Takes observed counts and creates a counts as they would be expected for
# all sets being independent from each other. The 'new.total' attribute
# allows for a projection to a different number of samples taken.
venn.observed2expected <- function(counts,new.total=NA)
{
  total      <- sum(counts[,"num"])
  total.sets <- apply(counts[,-1],2,function(X) sum(counts[X==1,"num"]))
  total.sets.fraction <- cbind(
                               negative=(total-total.sets)/total,  # at column 1
                               positive=total.sets / total         # at column 2
                              )
  if (is.na(new.total)) new.total <- total

  expected <- apply(counts[,-1],
                    1,
                    function(X) {
                      factors <- diag(total.sets.fraction[,X+1])  # selecting the right column
                      return(new.total * prod(factors))
                    })

  cbind(num=expected,counts[,-1])
}

