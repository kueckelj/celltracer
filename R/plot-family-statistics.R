#' @title Plot numeric distribution and statistical tests
#'
#' @description These functions display the distribution of numeric
#' variables for the whole sample or in a comparative manner if argument
#' \code{across} is specified. \code{plotViolinplot()} and \code{plotBoxplot}
#' allow for statistical tests such as t-test or ANOVA.
#'
#' @inherit argument_dummy params
#' @inherit variables_num params
#'
#' @param test_groupwise Character value or NULL. Specifies the groupwise statistical
#' test to be conducted. If character, one of \emph{'anova', 'kruskal.test'}. If set
#' to NULL the testing is skipped.
#' @param test_pairwise Character value or NULL. Specifies the pairwise statistical
#' test to be conducted. If character, one of \emph{'t.test', 'wilcox.test'}. If set
#' to NULL the testing is skipped.
#' @param ref_group Character value or NULL. Specifies the reference group against
#' which all other groups are compared in the test denoted in \code{test_groupwise}
#' is conducted. If set to NULL the first group found is taken.
#' @param step_increase Numeric value. Denotes the increase in fraction of total
#' height for every additional comparison to minimize overlap.
#' @param vjust Numeric value. Denotes the relative, vertical position of the results of
#' the test denoted in \code{test.groupwise}. Negative input highers, positive
#' input lowers the position.
#'
#' @param ... Additional arguments given to the respective \code{ggplot2::geom_<plot_type>()}
#' function. E.g. \code{plotViolinplot()} relies on \code{ggplot2::geom_violin()}.
#'
#' @inherit ggplot_family return
#'
#' @export

plotBoxplot <- function(object,
                        variables,
                        phase = NULL,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){
  
  stat_df <- getStatsDf(object = object, phase = phase, verbose = FALSE)
  
  confuns::plot_boxplot(df = stat_df,
                        variables = variables, 
                        across = across,
                        across.subset = across_subset,
                        relevel = relevel,
                        test.pairwise = test_pairwise,
                        test.groupwise = test_groupwise,
                        ref.group = ref_group,
                        step.increase = step_increase,
                        vjust = vjust,
                        scales = scales,
                        nrow = nrow,
                        ncol = ncol,
                        display.facets = display_facets,
                        display.points = display_points,
                        pt.alpha = pt_alpha,
                        pt.color = pt_clr,
                        pt.num = pt_num,
                        pt.shape = pt_shape,
                        pt.size = pt_size,
                        clrp = clrp,
                        clrp.adjust = clrp_adjust,
                        verbose = verbose,
                        ...)
  
}

#' @rdname plotBoxplot
#' @export
plotDensityplot <- function(object,
                            variables,
                            phase = NULL,
                            across = NULL,
                            across_subset = NULL,
                            relevel = NULL,
                            clrp = "milo",
                            clrp_adjust = NULL,
                            display_facets = TRUE,
                            scales = "free",
                            nrow = NULL,
                            ncol = NULL,
                            verbose = TRUE,
                            ...){
  
  stat_df <- getStatsDf(object = object, phase = phase, verbose = FALSE)
  
  confuns::plot_density(df = stat_df,
                        variables = variables,
                        across = across,
                        across.subset = across_subset,
                        relevel = relevel,
                        scales = scales,
                        display.facets = display_facets,
                        nrow = nrow,
                        ncol = ncol,
                        clrp = clrp,
                        clrp.adjust = clrp_adjust,
                        verbose = verbose,
                        ...)
  
}

#' @rdname plotBoxplot
#' @export
plotHistogram <- function(object,
                          variables,
                          phase = NULL,
                          across = NULL,
                          across_subset = NULL,
                          relevel = TRUE,
                          clrp = "milo",
                          clrp_adjust = NULL,
                          scales = "free",
                          nrow = NULL,
                          ncol = NULL,
                          verbose = TRUE,
                          ...){
  
  stat_df <- getStatsDf(object = object, phase = phase, verbose = FALSE)
  
  confuns::plot_histogram(df = stat_df,
                          variables = variables,
                          across = across,
                          across.subset = across_subset,
                          relevel = relevel,
                          scales = scales,
                          nrow = nrow,
                          ncol = ncol,
                          clrp = clrp,
                          clrp.adjust = clrp_adjust,
                          verbose = verbose,
                          ...)
  
}

#' @rdname plotBoxplot
#' @export
plotRidgeplot <- function(object,
                          variables,
                          phase = NULL,
                          across = NULL,
                          across_subset = NULL,
                          relevel = TRUE,
                          alpha = 0.8,
                          clrp = "milo",
                          clrp_adjust = NULL,
                          scales = "free",
                          nrow = NULL,
                          ncol = NULL,
                          verbose = TRUE,
                          ...){
  
  stat_df <- getStatsDf(object = object, phase = phase, verbose = FALSE)
  
  confuns::plot_ridgeplot(df = stat_df,
                          variables = variables,
                          across = across,
                          across.subset = across_subset,
                          relevel = relevel,
                          scales = scales,
                          nrow = nrow,
                          ncol = ncol,
                          alpha = alpha,
                          clrp = clrp,
                          clrp.adjust = clrp_adjust,
                          verbose = verbose)
  
}

#' @rdname plotBoxplot
#' @export
plotViolinplot <- function(object,
                           variables,
                           phase = NULL,
                           across = NULL,
                           across_subset = NULL,
                           relevel = TRUE,
                           clrp = "milo",
                           clrp_adjust = NULL,
                           test_groupwise = NULL,
                           test_pairwise = NULL,
                           ref_group = NULL,
                           step_increase = 0.01,
                           display_facets = TRUE,
                           vjust = 0,
                           scales = "free",
                           nrow = NULL,
                           ncol = NULL,
                           display_points = FALSE,
                           pt_alpha = 0.8,
                           pt_clr = "black",
                           pt_num = 100,
                           pt_size = 1.25,
                           pt_shape = 21,
                           verbose = TRUE,
                           ...){
  
  stat_df <- getStatsDf(object = object, phase = phase, verbose = FALSE)
  
  confuns::plot_violin(df = stat_df,
                       variables = variables,
                       across = across,
                       across.subset = across_subset,
                       relevel = relevel,
                       test.pairwise = test_pairwise,
                       test.groupwise = test_groupwise,
                       ref.group = ref_group,
                       step.increase = step_increase,
                       vjust = vjust,
                       scales = scales,
                       display.facets = display_facets,
                       nrow = nrow,
                       ncol = ncol,
                       display.points = display_points,
                       pt.alpha = pt_alpha,
                       pt.color = pt_clr,
                       pt.num = pt_num,
                       pt.shape = pt_shape,
                       pt.size = pt_size,
                       clrp = clrp,
                       clrp.adjust = clrp_adjust,
                       verbose = verbose,
                       ...)
  
}


#' @title Plot distribution of discrete variables
#'
#' @description Visualizes the count or the proportion of barcode spots falling
#' into certain groups via barcharts. It does so either for the whole sample or
#' in a comparing manner if \code{across} is specified.
#'
#' @inherit plotBoxplot params return
#'
#' @param variables Character vector. The grouping variables whose group count or
#' proportion you want to display. Must not contain the feature specified in
#' \code{across} - if \code{across} is not set to NULL.

plotBarchart <- function(object,
                         variables,
                         phase = NULL,
                         across = NULL,
                         across_subset = NULL,
                         relevel = TRUE,
                         clrp = "milo",
                         clrp_adjust = NULL,
                         position = "fill",
                         display_facets = TRUE,
                         ncol = NULL,
                         nrow = NULL,
                         ...){
  
  stat_df <- getStatsDf(object = object, phase = phase, verbose = FALSE)
  
  confuns::plot_barplot(df = stat_df,
                        variables = variables,
                        across = across,
                        across.subset = across_subset,
                        relevel = relevel,
                        display.facets = display_facets,
                        nrow = nrow,
                        ncol = ncol,
                        clrp = clrp,
                        clrp.adjust = clrp_adjust, 
                        position = position, 
                        ...)
  
}





#' @title Plot statistics related plots interactively
#' 
#' @description Opens an interactive application in which statistic related 
#' plots like violinplots, boxplots as well as statistical tests across 
#' groups can be conducted.
#'
#' @inherit argument_dummy params
#'
#' @export
#'

plotStatisticsInteractive <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  stats_df <-
    getStatsDf(object, phase = phase, verbose = FALSE) %>% 
    dplyr::select(-cell_id)
  
  
  confuns::plot_statistics_interactive(df = stats_df, n.across.subset = 100)
  
}

