% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggepitracks2.R
\name{ggepitracks2}
\alias{ggepitracks2}
\title{Plot Epigenetics Tracks for NGS Data using ggplot2 (version 2)}
\usage{
ggepitracks2(
  locus_file,
  track_conf,
  bsseq_conf = NULL,
  gene_model,
  ann_region = NULL,
  ann_color = "goldenrod1",
  out_dir = "out",
  height = NULL,
  width = 80
)
}
\arguments{
\item{locus_file}{BED file containing locus to plot. For each line, TAB-delimited information should contain: <chr>, <start>, <end>, <locus_name>, without header.}

\item{track_conf}{Configuration file containing track info. For each line, TAB-delimited information should contain: <track_name>, <track_type>, <track_bw/bg>, <y_min>, <y_max>, <color>, <group>, without header. NOTE: If you write "BS-seq" in "<track_type>", provide mC type (one of CG, CHG, CHH, All) to plot in "<track_bw/bg>" and provide detailed info in \code{bsseq_conf}, "<color>" is ignored in multi-mC mode of BS-seq. "<group>" is currently unused.}

\item{bsseq_conf}{Configuration file of BS-seq track. If there is no BS-seq track to plot, keep \code{NULL}. For each line, TAB-delimited information should contain: <track_name>, <CG_bw/bg>, <CHG_bw/bg>, <CHH_bw/bg>, <C_context_file>, without header. "<track_name>" here should be the same as in \code{track_conf}. If you choose only one mC type to plot, you can keep the column of other files as "NA", but provide all the bigWig files may be safe. "<C_context_file>" can be "NA" to ignore drawing C context track (currently unused).}

\item{gene_model}{BED file containing all elements of gene and/or TE model, generated by \verb{[tinyfuncr::extractPos]}}

\item{ann_region}{BED file of annotation regions. If not provided, no annotation will be added. For each line, TAB-delimited information should contain: <chr>, <start>, <end>, <annotation_name>, without header. <annotation_name> could be empty but the last TAB should be kept.}

\item{ann_color}{Color of annotated regions, defalut "goldenrod1" (alpha 30)}

\item{out_dir}{Path to output directory, will be created if not exist, default "out/". Output plots in PDF format will be saved as <out_dir>/<locus_name>.pdf}

\item{height}{Plot height, in "mm", defalut 6*ntracks+10}

\item{width}{Plot width, in "mm", defalut 80}
}
\description{
Plot genome tracks for NGS data using ggplot2, resembling GBrowse/JBrowse/IGV style (ver.2 for enhanced performance). Supporting coverage data in bigWig or bedGraph format from RNA-seq, sRNA-seq, BS-seq, ChIP-seq, RIP-seq, GRO-seq, NET-seq, DNase-seq, MNase-seq, ATAC-seq, SHAPE-seq etc. Especially optimized for BS-seq data in single-base resolution. Requires \code{bigWigToBedGraph} and \code{bedtools} on Linux.
}
\author{
Yujie Liu
}
