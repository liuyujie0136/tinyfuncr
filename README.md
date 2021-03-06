# tinyfuncr <img src="logo-tinyfuncr.png" width=200 align="right" />

A collection of tiny R functions for handy use.

## Installation

```r
devtools::install_github("liuyujie0136/tinyfuncr")
```

## Functions

* `plotsave`: Save plot to device with various formats
* `bm2md`: Convert html format bookmarks to markdown
* `igviewer`: Web-based IGViewer
  * `getigvtable`: Get IGV online reference genomes table
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
  * `read_tcsv`: Read table/csv files into a data frame
  * `write_tcsv`: Write table/csv files from a data frame
* `spirograph`: Draw Spirograph
* `ggbetweenstats_wrapper`: Wrapper for `(grouped_)ggbetweenstats` in `ggstatsplot`
* `ggchrom`: Annotate chromosome using `ggplot2`
  * `calcDensity`: Calculate density in a given region
    * `calcDensityGFF`: Calculate gene density on chromosome
  * `ggfanplot`: Add "fanplot" to chromosome
  * `ggideogram`: Plot ideogram and annotations on chromosome

## Dependencies

* In `tidyverse`: `ggplot2`, `tibble`, `tidyr`, `readr`, `purrr`, `dplyr`, `stringr`
* Data: `magrittr`, `reshape2`, `pairwiseComparisons`
* `ggplot2` extensions: `ggstatsplot`, `ggrepel`, `ggsignif`, `ggtree`
* Color: `RColorBrewer`, `paletteer`
* Bio: `Rsamtools`, `IRanges`, `GenomicRanges`
* Others:`export`, `eoffice`, `Rcpp`, `gmp`, `rlang`, `ipmisc`

## Bugs

If you find bugs, please open an [issue](https://github.com/liuyujie0136/tinyfuncr/issues).
