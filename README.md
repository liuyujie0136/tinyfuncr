# tinyfuncr <img src="logo-tinyfuncr.png" width=200 align="right" />

An R package containing some tiny functions for self-practice and handy use.

## Installation

```r
devtools::install_github("liuyujie0136/tinyfuncr")
```

## Functions

* `plotsave`: Save plot to device with various formats
* `bm2md`: Convert html format bookmarks to markdown
* `igviewer`: Web-based IGViewer
  * `getigvtable`: Get IGV online reference genomes table
  * `makeigvref`: Make IGV reference genome information
* `sam2bed`: Convert SAM/BAM file to BED
* `toUTF8`: Convert file(s) with different encoding(s) to UTF-8
* `LeapCal`: Leap-year calendar
* `fastIO`: Read/Write fasta/fastq files
  * `fastIN`: Read fasta/fastq files
  * `fastOUT`: Write fasta/fastq files
* `fastQCer`: Quality Check for fastq files
  * `fastQual`: Visualize quality information in fastq files
  * `fastGC`: Compute GC content across all reads in fastq files
  * `fastSC`: Compute sequence content across all bases in fastq files
  * `fastSQ`: Mean sequence quality scores across all reads in fastq files
* `mergeDups`: Merge duplicated lines into one entry
* `expandDups`: Expand one entry into multiple lines
* `fopen`: Open file with system default application
* `geom_signif_wrapper`: Wrapper for significance layer in `ggsignif`
* `RWrapper`: Wrapper for reading and writing files
  * `read_tcsv`: Read table/csv files
  * `write_tcsv`: Write table/csv files

## Dependencies

* In `tidyverse`: `readr`, `stringr`, `magrittr`, `purrr`, `ggplot2`
* Data: `reshape2`
* Plot: `export`, `eoffice`
* Bio: `Rsamtools`, `IRanges`, `GenomicRanges`
* Others: `Rcpp`

## Bugs

If you find bugs, please open an [issue](https://github.com/liuyujie0136/tinyfuncr/issues).
