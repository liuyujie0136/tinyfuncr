#' Wrapper for `(grouped_)ggbetweenstats` in `ggstatsplot`
#'
#' @description
#' A combination of box and violin plots along with jittered data points for between-subjects designs with statistical details included in the plot as a subtitle.
#'
#' * Remove outliers from violin plot for clarity, while using all of the data for pairwise test and boxplot.
#' * Remove useless parameters for outliers.
#'
#' @param data A dataframe (or a tibble) from which variables specified are to be taken. Other data types (e.g., matrix,table, array, etc.) will **not** be accepted.
#' @param x The grouping (or independent) variable from the dataframe `data`.
#' @param y The response (or outcome or dependent) variable from the dataframe `data`.
#' @param facet whether to use `grouping.var`, default `TRUE`.
#' @param grouping.var A single grouping variable (can be entered either as a bare name `x` or as a string `"x"`).
#' @param plotgrid.args A `list` of additional arguments passed to `patchwork::wrap_plots`, except for `guides` argument which is already separately specified here.
#' @param annotation.args A `list` of additional arguments passed to `patchwork::plot_annotation`.
#' @param plot.type Character describing the *type* of plot. Currently supported
#'   plots are `"box"` (for only boxplots), `"violin"` (for only violin plots),
#'   and `"boxviolin"` (for a combination of box and violin plots; default).
#' @param xlab,ylab Labels for `x` and `y` axis variables. If `NULL` (default),
#'   variable names for `x` and `y` will be used.
#' @param pairwise.comparisons Logical that decides whether pairwise comparisons
#'   are to be displayed (default: `TRUE`). Please note that only
#'   **significant** comparisons will be shown by default. To change this
#'   behavior, select appropriate option with `pairwise.display` argument. The
#'   pairwise comparison dataframes are prepared using the
#'   `ggstatsplot::pairwise_comparisons` function. For more details
#'   about pairwise comparisons, see the documentation for that function.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param pairwise.display Decides *which* pairwise comparisons to display.
#'   Available options are:
#'   - `"significant"` (abbreviation accepted: `"s"`)
#'   - `"non-significant"` (abbreviation accepted: `"ns"`)
#'   - `"all"`
#'
#'   You can use this argument to make sure that your plot is not uber-cluttered
#'   when you have multiple groups being compared and scores of pairwise
#'   comparisons being displayed.
#' @param bf.prior A number between `0.5` and `2` (default `0.707`), the prior
#'   width to use in calculating Bayes factors.
#' @param bf.message Logical that decides whether to display Bayes Factor in
#'   favor of the *null* hypothesis. This argument is relevant only **for
#'   parametric test** (Default: `TRUE`).
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as a subtitle (Default: `TRUE`). If set to `FALSE`, only
#'   the plot will be returned.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle. Will work only if
#'   `results.subtitle = FALSE`.
#' @param caption The text for the plot caption.
#' @param outlier.coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `outlier.coef` times the Inter-Quartile Range (IQR) (Default:
#'   `1.5`).
#' @param centrality.plotting Logical that decides whether centrality tendency
#'   measure is to be displayed as a point with a label (Default: `TRUE`).
#'   Function decides which central tendency measure to show depending on the
#'   `type` argument.
#'   - **mean** for parametric statistics
#'   - **median** for non-parametric statistics
#'   - **trimmed mean** for robust statistics
#'   - **MAP estimator** for Bayesian statistics
#'
#'   If you want default centrality parameter, you can specify this using
#'   `centrality.type` argument.
#' @param centrality.type Decides which centrality parameter is to be displayed.
#'   The default is to choose the same as `type` argument. You can specify this
#'   to be:
#'   - `"parameteric"` (for **mean**)
#'   - `"nonparametric"` (for **median**)
#'   - `robust` (for **trimmed mean**)
#'   - `bayes` (for **MAP estimator**)
#'
#'   Just as `type` argument, abbreviations are also accepted.
#' @param point.args A list of additional aesthetic arguments to be passed to
#'   the `geom_point` displaying the raw data.
#' @param violin.args A list of additional aesthetic arguments to be passed to
#'   the `geom_violin`.
#' @param ggplot.component A `ggplot` component to be added to the plot prepared
#'   by `ggstatsplot`. This argument is primarily helpful for `grouped_`
#'   variants of all primary functions. Default is `NULL`. The argument should
#'   be entered as a `ggplot2` function or a list of `ggplot2` functions.
#' @param package,palette Name of the package from which the given palette is to
#'   be extracted. The available palettes and packages can be checked by running
#'   `View(paletteer::palettes_d_names)`.
#' @param centrality.point.args,centrality.label.args A list of additional aesthetic
#'   arguments to be passed to `ggplot2::geom_point` and
#'   `ggrepel::geom_label_repel` geoms, which are involved in mean plotting.
#' @param  ggsignif.args A list of additional aesthetic
#'   arguments to be passed to `ggsignif::geom_signif`.
#'
#' @import ggplot2
#' @import ggstatsplot
#'
#' @importFrom dplyr select group_by arrange mutate
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats t.test oneway.test quantile
#' @importFrom rlang enquo as_name !! as_string ensym
#' @importFrom paletteer scale_color_paletteer_d scale_fill_paletteer_d
#' @importFrom ggsignif geom_signif
#' @importFrom purrr pmap
#' @importFrom ipmisc stats_type_switch
#' @importFrom tidyr drop_na
#'
#' @seealso [ggstatsplot::ggbetweenstats()] and [ggstatsplot::grouped_ggbetweenstats()]
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html}
#'
#' @author
#' * Originally by Indrajeet Patil
#' * Wrapped by Yujie Liu


################################################################
#' @export ggbetweenstats_wrapper
ggbetweenstats_wrapper <- function(data,
                                   x,
                                   y,
                                   facet = TRUE,
                                   grouping.var = NULL,
                                   plotgrid.args = list(),
                                   annotation.args = list(),
                                   ...) {

  # ======================== preparing dataframe ==========================

  if (facet) {
    # grouped data
    df <-
      data %>%
      dplyr::select({{ grouping.var }}, {{ x }}, {{ y }}) %>%
      ggstatsplot:::grouped_list(grouping.var = {{ grouping.var }})
  } else {
    # ungrouped data
    df <- list(dplyr::select(data, {{ x }}, {{ y }}))
    names(df) <- "data"
  }

  # ============== creating a list of plots using `pmap`=======================

  plotlist <-
    purrr::pmap(
      .l = list(data = df, title = names(df)),
      .f = tinyfuncr:::ggbetweenstats_wrapper_main,
      # put common parameters here
      x = {{ x }},
      y = {{ y }},
      ...
    )

  # combining the list of plots into a single plot
  return(
    ggstatsplot::combine_plots(
      plotlist,
      plotgrid.args = plotgrid.args,
      annotation.args = annotation.args
    )
  )
}


################################################################
ggbetweenstats_wrapper_main <-
  function(data,
           x,
           y,
           plot.type = "boxviolin",
           type = "parametric",
           pairwise.comparisons = TRUE,
           pairwise.display = "significant",
           p.adjust.method = "holm",
           effsize.type = "unbiased",
           bf.prior = 0.707,
           bf.message = TRUE,
           results.subtitle = TRUE,
           xlab = NULL,
           ylab = NULL,
           caption = NULL,
           title = NULL,
           subtitle = NULL,
           k = 2L,
           var.equal = FALSE,
           conf.level = 0.95,
           nboot = 100L,
           tr = 0.2,
           centrality.plotting = TRUE,
           centrality.type = type,
           centrality.point.args = list(size = 5, color = "darkred"),
           centrality.label.args = list(size = 3,
                                        nudge_x = 0.4,
                                        segment.linetype = 4),
           outlier.coef = 1.5,
           point.args = list(
             position = ggplot2::position_jitterdodge(dodge.width = 0.60),
             alpha = 0.4,
             size = 3,
             stroke = 0
           ),
           violin.args = list(width = 0.5, alpha = 0.2),
           ggsignif.args = list(textsize = 3),
           ggtheme = ggplot2::theme_bw(),
           ggstatsplot.layer = TRUE,
           package = "RColorBrewer",
           palette = "Dark2",
           ggplot.component = NULL
           ) {


  # convert entered stats type to a standard notation
  type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # --------------------------------- data -----------------------------------

  # creating a dataframe
  data %<>%
    dplyr::select({{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    dplyr::mutate({{ x }} := droplevels(as.factor({{ x }})))

  # no outliers data for each group defined by `x`
  no_outliers <- data.frame()
  for (g in levels(data %>% dplyr::select({{ x }}) %>% .[[1]])) {
    tmp <- data %>% dplyr::filter({{ x }} == g)
    qt <- quantile(tmp[[2]], probs = c(0.25, 0.75))
    min <- max(min(tmp[[2]]), qt[1] - outlier.coef * (qt[2] - qt[1]))
    max <- min(max(tmp[[2]]), qt[2] + outlier.coef * (qt[2] - qt[1]))
    no_outliers <- rbind(no_outliers,
                         dplyr::filter(tmp, {{ y }} >= min & {{ y }} <= max))
  }

  # --------------------- subtitle/caption preparation ------------------------

  # figure out which test to run based on the no. of levels of the independent variable
  test <- ifelse(nlevels(data %>% dplyr::pull({{ x }}))[[1]] < 3, "t", "anova")

  if (isTRUE(results.subtitle)) {
    # preparing the Bayes factor message
    if (type == "parametric" && isTRUE(bf.message)) {
      caption_df <- tryCatch(
        ggstatsplot:::function_switch(
          test = test,
          # arguments relevant for expression helper functions
          data = data,
          x = rlang::as_string(x),
          y = rlang::as_string(y),
          type = "bayes",
          bf.prior = bf.prior,
          top.text = caption,
          paired = FALSE,
          k = k
        ),
        error = function(e) NULL
      )

      caption <- if (!is.null(caption_df)) caption_df$expression[[1]]
    }

    # extracting the subtitle using the switch function
    subtitle_df <- tryCatch(
      ggstatsplot:::function_switch(
        test = test,
        # arguments relevant for expression helper functions
        data = data,
        x = rlang::as_string(x),
        y = rlang::as_string(y),
        paired = FALSE,
        type = type,
        effsize.type = effsize.type,
        var.equal = var.equal,
        bf.prior = bf.prior,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        k = k
      ),
      error = function(e) NULL
    )

    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1]]
  }

  # -------------------------- basic plot -----------------------------------

  # add only the points which are not outliers
  plot <-
    ggplot2::ggplot() +
    rlang::exec(
      .fn = ggplot2::geom_point,
      data = no_outliers,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ x }}),
      !!!point.args
    )

  # add boxplot
  if (plot.type %in% c("box", "boxviolin")) {
    plot <- plot +
      rlang::exec(
        .fn = ggplot2::stat_boxplot,
        data = data,
        mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
        width = 0.3,
        alpha = 0.2,
        fill = "white",
        coef = outlier.coef,
        outlier.shape = NA,
        geom = "boxplot",
        position = ggplot2::position_dodge(width = NULL)
      )
  }

  # add violin plot
  if (plot.type %in% c("violin", "boxviolin")) {
    plot <- plot +
      rlang::exec(
        .fn = ggplot2::geom_violin,
        data = no_outliers,
        mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
        fill = "white",
        !!!violin.args
      )
  }

  # ---------------- centrality tagging -------------------------------------

  # add labels for centrality measure
  if (isTRUE(centrality.plotting)) {
    plot <-
      ggstatsplot:::centrality_ggrepel(
        plot = plot,
        data = data,
        x = {{ x }},
        y = {{ y }},
        k = k,
        type = ipmisc::stats_type_switch(centrality.type),
        tr = tr,
        centrality.point.args = centrality.point.args,
        centrality.label.args = centrality.label.args
      )
  }

  # ggsignif labels -----------------------------------------------------------

  if (isTRUE(pairwise.comparisons) && test == "anova") {
    # creating dataframe with pairwise comparison results
    df_pairwise <-
      ggstatsplot::pairwise_comparisons(
        data = data,
        x = {{ x }},
        y = {{ y }},
        type = type,
        tr = tr,
        paired = FALSE,
        var.equal = var.equal,
        p.adjust.method = p.adjust.method,
        k = k
      )

    # preparing the caption for pairwise comparisons test
    if (type != "bayes") {
      caption <-
        ggstatsplot::pairwise_caption(
          caption,
          unique(df_pairwise$test.details),
          pairwise.display
        )
    }

    # check whether to add signif layer
    df_pairwise %<>%
      dplyr::mutate(groups =
                      purrr::pmap(.l = list(group1, group2), .f = c))
    if (pairwise.display %in% c("s", "significant")) {
      df_pairwise %<>%
        dplyr::filter(p.value < 0.05) %>%
        dplyr::arrange(group1, group2)
    }
    if (pairwise.display %in% c("ns",
                                "nonsignificant",
                                "non-significant")) {
      df_pairwise %<>%
        dplyr::filter(p.value >= 0.05) %>%
        dplyr::arrange(group1, group2)
    }

    # calc scale values
    min_y_data <- min(no_outliers[[{{ y }}]])
    max_y_data <- max(no_outliers[[{{ y }}]])
    range_y_data <- max_y_data - min_y_data

    # calc init plot scale
    max_y_plot <- max_y_data + range_y_data * 0.02
    min_y_plot <- min_y_data

    # adding the layer for pairwise comparisons
    if (dim(df_pairwise)[[1]] != 0) {
      plot <-
        plot + rlang::exec(
          .f = tinyfuncr::geom_signif_wrapper,
          data = no_outliers,
          mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
          comparisons = df_pairwise$groups,
          map_signif_level = TRUE,
          y_position = max_y_data + range_y_data * 0.1,
          margin_top = 0,
          step_increase = range_y_data * 0.06,
          tip_length = range_y_data * 0.01,
          annotations = df_pairwise$label,
          test = NULL,
          parse = TRUE,
          vjust = 0,
          !!!ggsignif.args
        )
      max_y_plot <-
        max_y_plot + range_y_data * 0.1 + range_y_data * 0.06 * (nrow(df_pairwise) - 1)
    }
  }

  # # ------------------------ annotations and themes -------------------------

  # add y lim
  plot <- plot + coord_cartesian(ylim = c(min_y_plot, max_y_plot))
  # using `coord_cartesian` instead of `scale_y_continuous(limits = c(0,100))` will only zoom the plot, without changing boxplot's shape!

  # specifying annotations and other aesthetic aspects for the plot
  ggstatsplot:::aesthetic_addon(
    plot = plot,
    x = data %>% dplyr::pull({{ x }}),
    xlab = xlab,
    ylab = ylab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    ggstatsplot.layer = ggstatsplot.layer,
    package = package,
    palette = palette,
    ggplot.component = ggplot.component
  )
}
