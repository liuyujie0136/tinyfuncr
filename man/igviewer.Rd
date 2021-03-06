% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/igviewer.R
\name{igviewer}
\alias{igviewer}
\title{Web-based IGViewer}
\usage{
igviewer(
  ref,
  refmake = FALSE,
  refid = NULL,
  anno = NULL,
  align = NULL,
  var = NULL,
  wig = NULL,
  seg = NULL,
  gwas = NULL,
  out = "IGViewer"
)
}
\arguments{
\item{ref}{a one-row data frame containing Reference Genome's information: NAME in the first column, fastaURL in the second, indexURL in the third. Note: URL can be an address to local files, relative files location is acceptable, but absolute files address should add \verb{file:///} before \verb{C:/} etc in Windows system (See Example part for more information).}

\item{refmake}{whether to make reference information from function \code{makeigvref}, default \code{FALSE}}

\item{refid}{if you set \code{refmake} as \code{TRUE}, provide reference genome id}

\item{anno}{a one-row data frame containing Annotation Track's information: NAME in the first column, URL in the second. Note: if you set \code{refmake} as \code{TRUE}, you don't need to provide annotation information.}

\item{align}{a multi-row data frame containing Alignment Tracks' information: NAME in the first column, URL in the second, indexURL in the third. Note: set \code{stringsAsFactors = FALSE} in creating data frames is preferred.}

\item{var}{a multi-row data frame containing Variant Tracks' information: NAME in the first column, URL in the second, indexURL in the third}

\item{wig}{a multi-row data frame containing WIG Tracks' information: NAME in the first column, URL in the second}

\item{seg}{a multi-row data frame containing Segmented Copy Number Tracks' information: NAME in the first column, URL in the second}

\item{gwas}{a multi-row data frame containing GWAS Tracks' information: NAME in the first column, URL in the second}

\item{out}{output file name, without suffix}
}
\description{
Use igv.js to show tracks in a html file. For more information, please visit: https://github.com/igvteam/igv.js
}
\examples{
align <-
  data.frame(
    name = c("THA1", "THA2"),
    url = c(
      paste0(
        "file:///",
        system.file(
          "extdata",
          "THA1_sorted.bam",
          package = "tinyfuncr",
          mustWork = TRUE
        )
      ),
      paste0(
        "file:///",
        system.file(
          "extdata",
          "THA2_sorted.bam",
          package = "tinyfuncr",
          mustWork = TRUE
        )
      )
    ),
    index = c(
      paste0(
        "file:///",
        system.file(
          "extdata",
          "THA1_sorted.bam.bai",
          package = "tinyfuncr",
          mustWork = TRUE
        )
      ),
      paste0(
        "file:///",
        system.file(
          "extdata",
          "THA2_sorted.bam.bai",
          package = "tinyfuncr",
          mustWork = TRUE
        )
      )
    ),
    stringsAsFactors = FALSE
  )

igviewer(refmake = TRUE,
         refid = "sacCer3",
         align = align)
}
\author{
Yujie Liu
}
