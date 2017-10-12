#!/usr/bin/env Rscript

###
# Barplots and pie plots of intersection of primary feature degree data with secondary feature compeak data
#
# EL
# First Created on October 11, 2017
# Last Modified on October 11, 2017
###

suppressPackageStartupMessages(library("argparse"))
library(FetAnn)

parser <- ArgumentParser()
parser$add_argument("--degreeF", type = "character", nargs = 1, required = TRUE,
  help = "files named like '*Degree_LOJ.bed'")
parser$add_argument("--degreeconfig", type = "character", nargs = 1, required = TRUE,
  help = "the configuration file for the order of stages named like '*_order.config'")
parser$add_argument("--degreesubset", type = "character", nargs = "+", required = FALSE,
    help = "the input configuration file")
parser$add_argument("--compeakF", type = "character", nargs = 1, required = TRUE,
  help = "files named like '*ComPeak_LOJ.bed'")
parser$add_argument("--inputconfig", type = "character", nargs = 1, required = TRUE,
  help = "the input configuration file")
parser$add_argument("--nms", type = "character", nargs = 1, required = TRUE,
  help = "the output directory with prefix")

args <- parser$parse_args()
attach(args)

degree.obj <- degreeConst(degreeF, degreeconfig, ifelse(exists("degreesubset"), degreesubset, NULL))
compeak.obj <- degree2compeak(degree.obj, compeakF, inputconfig)

# plot distribution of decesion across stages [4th column of bed] of degree.obj
stageBar(degree.obj, pdffout = paste0(nms, "_DegreeDist.pdf"))

# plot distribution of decesion across stage [4th column of bed] of compeak.obj
stageBar(compeak.obj, pdffout = paste0(nms, "_ComPeakDist.pdf"))

# pie plot
