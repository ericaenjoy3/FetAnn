#' @include FetAnnClass.R
#' @title degree2compeak
#' @name degree2compeak
#' @rdname degree2compeak-methods
#' @export degree2compeak
setGeneric(name = 'degree2compeak',
  def = function(degree.obj, compeakF, inputconfig) {
    standardGeneric("degree2compeak")
  }
)

setMethod(f = "degree2compeak",
  signature = c("degree", "character", "character"),
  def = function(degree.obj, compeakF, inputconfig) {
    dat <- read.table(compeakF, header = TRUE, as.is = TRUE, sep = "\t")
    ord <- read.table(inputconfig, header = FALSE, as.is = TRUE, sep = "\t")[, 2]
    colnames(dat)[-c(1:4)] <- colnames(dat)[-c(1:4)][order(match(colnames(dat)[-c(1:4)], ord))]
    d1 <- data.table(degree.obj@bed, key = c("chr", "start", "end", "clus"))
    d2 <- data.table(dat, key = c("chr", "start", "end", "clus"))
    dat <- data.frame(d2[d1], row.names = NULL)
    dat[, 4] <- factor(dat[, 4], levels = levels(degree.obj@bed[, 4]))
    return(new("compeak", bed = dat))
  }
)

#' @title stageBar
#' @name stageBar
#' @rdname stageBar-methods
#' @export stageBar
setGeneric(name = 'stageBar',
  def = function(obj, pdffout) {
    standardGeneric("stageBar")
  }
)

setMethod(f = "stageBar",
  signature = c("degree", "character"),
  def = function(obj, pdffout) {
    ldat <- obj@bed[,c("clus", "dec")] %>% group_by(clus, dec) %>% summarise(n = n()) %>% as.data.frame()
    theme_set(theme_grey(base_size=15))
    p1 <- ggplot(ldat, aes(x = clus, y = n, fill = dec)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
      labs(x = "", y = "count") +
      theme(legend.title = element_blank(), panel.spacing = unit(2, "lines"), legend.position = "top")
    ggsave(filename = pdffout, plot = p1)
  }
)

setMethod(f = "stageBar",
  signature = c("compeak", "character"),
  def = function(obj, pdffout) {
    ldat <- melt(obj@bed[, -c(1:3)], id.vars = c("clus", "dec")) %>% group_by(clus, dec, variable) %>% summarise(n = sum(value)) %>% as.data.frame()
    theme_set(theme_grey(base_size=15))
    p1 <- ggplot(ldat, aes(x = clus, y = n, fill = variable)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
      labs(x = "", y = "count") +
      theme(legend.title = element_blank(), panel.spacing = unit(2, "lines"), legend.position = "top") +
      facet_grid(.~dec)
    ggsave(filename = pdffout, plot = p1)
  }
)

#' @title stagePie
#' @name stagePie
#' @rdname stagePie-methods
#' @export stagePie
setGeneric(name = 'stagePie',
  def = function(compeak.obj, pdffout) {
    standardGeneric("stagePie")
  }
)

setMethod(f = "stagePie",
  signature = c("compeak", "character"),
  def = function(degree.obj, compeak.obj, pdffout) {
    sec_dat <- compeak.obj@bed[, -c(1:4, ncol(compeak.obj@bed))]
    col_names <- colnames(sec_dat)
    sec_labvec <- apply(sec_dat, 1, function(vec, col_names){
      paste(col_names[which(vec>0)], collapse = ";")},
      col_names = col_names)
    sec_labvec <- sapply(sec_labvec, function(ele) ifelse(nchar(ele) == 0, "Rest", ele))
    names(sec_labvec) <- NULL
    dat <- cbind(compeak.obj@bed[, c("clus", "dec")], sec = sec_labvec)
    ldat <- dat %>% group_by(clus, dec, sec) %>% summarise(n = n()) %>% data.table(key = c("clus", "dec"))
    stats <- ldat %>% group_by(clus, dec) %>% summarise(n = sum(n)) %>% data.table(key = c("clus", "dec"))
    ldat <- ldat[stats] %>% mutate(pct = 100*n/i.n)
    # here
    cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=100))

    p1 <- ggplot(subset(ldat, dec == "ME"), aes(x = factor(1), y = pct, fill = sec))+
    geom_bar(width = 1, stat = "identity")+
    facet_grid(. ~ clus)+
    coord_polar(theta = 'y')+
    theme(panel.background = element_blank(),
      text = element_text(size = 15), axis.ticks = element_blank(), axis.title = element_blank(),
      panel.spacing = unit(2, "lines"), legend.position = "top",
      axis.text = element_blank(), panel.grid = element_blank(), legend.title = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      scale_colour_gradientn(colors = "red")
    ggsave(filename = pdffout, plot = p1)
  }
)
