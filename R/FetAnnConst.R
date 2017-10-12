#' @include FetAnnClass.R
#' @title construct an \code{degree} object
#' @name degreeConst
#' @rdname degreeConst-mehtods
#' @description construct an \code{degree} object from degreeF, degreeconfig, degreesubset
#' @param degreeF an Degree_LOJ bed file
#' @param degreeconfig an *_order.config file for the 4th column of Degree_LOJ bed file
#' @param degreesubset a *_subset.config file for subset the 5th column of Degree_LOJ bed file
#' @return A \code{degree} object
#' @export degreeConst
degreeConst <- function(degreeF, degreeconfig, degreesubset = NULL) {
  dat <- read.table(degreeF, header = TRUE, as.is = TRUE, sep = "\t")
  dat <- dat[, c(1:4, ncol(dat))]
  ord <- read.table(degreeconfig, header = FALSE, as.is = TRUE, sep = "\t")[, 1]
  if (!is.null(degreesubset)) {
    subord <- read.table(degreesubset, header = FALSE, as.is = TRUE, sep = "\t")[, 1]
    dat <- dat[which(dat[, ncol(dat)] %in% subord), ]
  }
  dat[,4] <- factor(dat[,4], levels = ord)
  dat[, 4] <- droplevels(dat[, 4])
  colnames(dat) <- c("chr", "start", "end", "clus", "dec")
  return(new("degree", bed = dat))
}
