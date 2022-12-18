# tinyfuncr <img src="logo-tinyfuncr.png" width=200 align="right" />

A collection of tiny R functions for handy use.

## Installation

```r
devtools::install_github("liuyujie0136/tinyfuncr")
```

## Functions

* `bm2md`: Convert html format bookmarks to markdown
* `spirograph`: Draw Spirograph
* `mergeDups`: Merge duplicated lines into one entry
* `expandDups`: Expand one entry into multiple lines
* `RWrapper`: Wrapper for reading and writing files
  * `read_tcsv`: Read table/csv files into a data frame
  * `write_tcsv`: Write table/csv files from a data frame
* `geom_signif_wrapper`: Wrapper for significance layer in `ggsignif`
* `ggbetweenstats_wrapper`: Wrapper for `(grouped_)ggbetweenstats` in `ggstatsplot`
* `ggchrom`: Annotate chromosome using `ggplot2`
  * `calcDensity`: Calculate density in a given region
  * `calcDensityGFF`: Calculate gene density on chromosome
  * `ggfanplot`: Add "fanplot" to chromosome
  * `ggideogram`: Plot ideogram and annotations on chromosome
* `extractPos`: Extract all elements from GFF3 file
* `genometracks`: Plot genome tracks for NGS data
* `ggepitracks`: Plot epigenetic data tracks using ggplot2

## Dependencies

* In `tidyverse`: `ggplot2`, `dplyr`, `stringr`, `tidyr`, `purrr`
* Data: `magrittr`, `data.table`
* `ggplot2` extensions: `ggstatsplot`, `ggrepel`, `ggsignif`, `ggtree`, `patchwork`, `cowplot`
* Color: `RColorBrewer`, `paletteer`
* Bio: `GenomicRanges`, `rtracklayer`
* Others: `gmp`, `rlang`, `ipmisc`, `parallel`

## Bugs

If you find bugs, please open an [issue](https://github.com/liuyujie0136/tinyfuncr/issues).
