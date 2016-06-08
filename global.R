


library(idr)
library(GenomicRanges)
#source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/rangeoverlapper.R")
source("rangeoverlapper.R")
library(dplyr)
library(ggplot2)
library(ggvis)

karenina_macs <- read.delim("data/karenina_SSY_Siamang_CTCF_VS_inputctrl_peaks.narrowPeak", header= FALSE, sep="\t", stringsAsFactors = FALSE)

monty_macs <- read.delim("data/monty_SSY_Siamang_CTCF_VS_inputctrl_peaks.narrowPeak", header= FALSE, sep="\t", stringsAsFactors = FALSE)

cols <-c("chr","start","end","peaknum","visnum", ".","foldchange","neglog10pval","neglog10qval","relsummitpos_tostart")
colnames(karenina_macs) <- cols
colnames(monty_macs)<-cols
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
karenina_bayp <- read.delim("data/karenina_thresh0.txt", header= TRUE, sep=" ", stringsAsFactors = FALSE)

monty_bayp <- read.delim("data/monty_thresh0.txt", header= TRUE, sep=" ", stringsAsFactors = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~all signal truncated

ov_macs <-olRanges(query=makeGRangesFromDataFrame(karenina_macs), subject=makeGRangesFromDataFrame(monty_macs), output="df") 

ov_bayp <-olRanges(query=makeGRangesFromDataFrame(karenina_bayp,seqnames.field ="space"), 
                 subject=makeGRangesFromDataFrame(monty_bayp,seqnames.field ="space"), output="df")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# uv_macs<- readRDS("C:/Users/Owner/Box Sync/chip_seq/idr/uv_macs_reprod.rds")
# uv_bayp<- readRDS("C:/Users/Owner/Box Sync/chip_seq/idr/uv_bayp_reprod.rds")
# idr.out_macs<- readRDS("C:/Users/Owner/Box Sync/chip_seq/idr/idr.out_macs.rds")
# idr.out_bayp<- readRDS("C:/Users/Owner/Box Sync/chip_seq/idr/idr.out_bayp.rds")
# sigpeaks_bayp<- readRDS("C:/Users/Owner/Box Sync/chip_seq/idr/sigpeaks_bayp.rds")
# sigpeaks_macs<- readRDS("C:/Users/Owner/Box Sync/chip_seq/idr/sigpeaks_macs.rds")
uv_macs<- readRDS("data/uv_macs_reprod.rds")
uv_bayp<- readRDS("data/uv_bayp_reprod.rds")
idr.out_macs<- readRDS("data/idr.out_macs.rds")
idr.out_bayp<- readRDS("data/idr.out_bayp.rds")
sigpeaks_bayp<- readRDS("data/sigpeaks_bayp.rds")
sigpeaks_macs<- readRDS("data/sigpeaks_macs.rds")

df<- rbind(sigpeaks_bayp, sigpeaks_macs)

