# Extract intersections

# The intersections of the Venn diagrams are given technically
# easily accessible names out of 0s and 1s, but this requires to know
# the exact order in which the sets were passed to the routine. This
# may not be retrievable when looking only at the results object
# a couple of years later, and, generally, it is preferable to have
# results explain themselves.
#
# Thisroutine is called internally by the venn() function to further
# annotate the intersections with the names that are either passed
# to this routine or that are retrieved from the column names.

vennMembers <- function(l, universe=NA, set.names=NULL, ..., debug=F)
{
  venn_object <- getVennCounts(l, universe, intersections=TRUE, ...)
  map <- attr(venn_object, "intersections")
  if (debug) print(head(map))

  if(is.null(set.names) || any(is.na(set.names))) {
    if (is.null(colnames(venn_object))) {
       stop("Sorry, but we need names of the sets that are passed")
    }
    else {
       set.names <- colnames(venn_object)[-1]
    }
  }

  if(is.matrix(l) || is.data.frame(l)) {
    ids <- rownames(l)
    retval <- list()
    if (is.null(ids)) {
        # The samples have no name, intersections will refer to line numbers
        retval <- map
    } else {
        # The samples have identifiers
        for(i in names(map)) retval[[i]] <- ids[map[[i]]]
    }
  } else {
    if(is.list(l)) {
      retval <- map
    } else {
      stop("vennMembers: Do not know how to treat l, which is not a list, data frame or matrix")
    }
  }


  flags <- do.call(rbind, strsplit(names(map), character(0), fixed=TRUE))
  rownames(flags) <- names(map)
  colnames(flags) <- set.names
  nameList <- list()
  for(i in 1:nrow(flags))     nameList[[i]] <- ifelse(flags[i,]=="1", colnames(flags), "")
  nameList <- do.call(data.frame,nameList)
  nameList <- apply(nameList, 2, paste, collapse=":")
  nameList <- gsub('::+', ':', nameList)
  nameList <- gsub('^:+', '',  nameList)
  nameList <- gsub(':+$', '',  nameList)

  if (debug) print(nameList)

  names(retval) <- nameList

  sortTab <- cbind(sapply(nameList, nchar), nameList)
  ord     <- order(sortTab[,1], sortTab[,2])

  retval <- retval[ord]

  lapply(retval, as.character)
}
