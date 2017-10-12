#####rlang .data prevents R CMD check from giving a NOTE about undefined global variables

### to be verified
#' @import ModClusterR
#' @import RColorBrewer
#' @import ggplot2
#' @import dplyr
#' @import methods
#' @importFrom data.table data.table foverlaps melt
#' @importFrom rlang .data
#' @importFrom grDevices dev.off pdf png colorRampPalette
#' @importFrom stats na.omit quantile median
#' @importFrom graphics plot axis abline text par mtext
#' @importFrom ComplexHeatmap Heatmap draw
#' @importFrom EnrichedHeatmap EnrichedHeatmap '+.AdditiveUnit'
#' @importFrom circlize colorRamp2
#' @importFrom utils read.table write.table setTxtProgressBar

#' @title An S4 class to primary feature data.
#' @name degree-class
#' @rdname degree-class
#' @description Store cluster sets in a bed object.
#' @slot bed data.frame object
#' @exportClass degree
degree <- setClass(
  "degree",
  slots = c(bed = "data.frame"),
  validity = function(object) {
    if(nrow(object@bed) < 1) {
      return("Empty data.table was given.")
    }
    if(!is.factor(object@bed[,4])) {
      return("The 4th col of bed cannot be anything else but factor.")
    }
    if(!identical(colnames(object@bed), c("chr","start","end","clus","dec"))) {
      return("Colnames for bed must be \"chr\",\"start\",\"end\", \"clus\", and \"dec\"")
    }
    return(TRUE)
  }
)

#' @title An S4 class to secondary feature data to query overlap with primary feature data.
#' @name compeak-class
#' @rdname compeak-class
#' @description Store cluster sets in a bed object.
#' @slot bed data.frame object
#' @exportClass compeak
compeak <- setClass(
  "compeak",
  slots = c(bed = "data.frame"),
  validity = function(object) {
    if(nrow(object@bed) < 1) {
      return("Empty data.table was given.")
    }
    if(!is.factor(object@bed[,4])) {
      return("The 4th col of bed cannot be anything else but factor.")
    }
    if(!identical(colnames(object@bed)[1:4], c("chr","start","end","clus"))) {
      return("Colnames for bed must be \"chr\",\"start\",\"end\", and \"clus\"")
    }
    if(!identical(colnames(object@bed)[ncol(object@bed)], c("dec"))) {
      return("Colnames for bed must be \"decesion\"")
    }
    return(TRUE)
  }
)
