


#' @title Set up correlation with celltracer
#' 
#' @description Set up the necessary object to perform correlation analysis. 
#'
#' @inherit argument_dummy
#' @param variables_subset Character vector or NULL. Specifies the numeric variables the subsequent correlation 
#' steps will include.. 
#' 
#' If set to NULL all of them are chosen. You can prefix variables you do NOT want to influence the clustering
#' with a \emph{'-'}. (Saves writing if there are more variables you are interested in
#' than variables you are not interested in.)
#' 
#' Use \code{getNumericVariableNames()} to obtain all valid input options.
#' 
#' @details All grouping variables that exist at the time this function is used are added to the correlation slot which can be used to compare 
#' correlation results between different groups. Clustering variables that are added to the overall data via 
#' \code{addHierarchicalCluster(), addKmeansCluster() etc.} are added as options for \code{correlateAcross()}
#' automatically. 
#'
#' @return An updated celltracer object. 
#' @export
#'
initiateCorrelation <- function(object,
                                phase = NULL, 
                                variables_subset = NULL,
                                force = FALSE, 
                                verbose = NULL, 
                                ...){
  
  check_object(object)
  assign_default(object)
  
  corr_obj <- object@analysis$correlation[[phase]]
  
  if(base::class(corr_obj) != "corr_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(-phase)
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    numeric_df <- hlpr_select(stat_df, variables_subset = variables_subset) # only numeric variables for feedback
    
    stat_df <- base::as.data.frame(numeric_df)
    base::rownames(stat_df) <- cell_ids
    
    stat_df <- 
      dplyr::left_join(x = tibble::rownames_to_column(stat_df, var = "cell_id"), y = getClusterDf(object, phase = phase), by = "cell_id") %>% 
      dplyr::select(-phase) %>% 
      dplyr::left_join(x = ., y = getMetaDf(object, phase = phase), by = "cell_id") %>% 
      dplyr::select(-phase) %>% 
      base::as.data.frame() %>%
      tibble::column_to_rownames(var = "cell_id") %>% 
      dplyr::select(-dplyr::starts_with(match = "well"))
      
    
    corr_obj <- confuns::initiate_corr_object(corr.data = stat_df)
    
    msg <- glue::glue("Successfully initiated correlation analysis for {phase} phase with variables: '{remaining_vars}'", 
                      remaining_vars = glue::glue_collapse(x = base::colnames(numeric_df), sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
  } else {
    
    msg <- glue::glue("Correlation analysis for {phase} phase already exists. Set argument 'force' to TRUE in order to overwrite it.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object@analysis$correlation[[phase]] <- corr_obj
  
  base::return(object)  
  
}





#' @title Compute correlation between variables 
#'
#' @description Performs correlation analysis on the variables with which 
#' the correlation analysis has been initiated via \code{initiateCorrelation()}.
#' 
#' @inherit argument_dummy params
#' @param across Character vector or NULL. Specifies the grouping variables across which 
#' correlation is computed. Meaning that before correlation is computed the data is split
#' into the groups suggested by the respective grouping variable. 
#' 
#' If set to NULL defaults to all grouping variables. 
#' 
#' 
#' @details \code{correlateAcross()} iterates over all grouping variables and computes the 
#' correlation analysis for every group the respective grouping variables suggests. Use the 
#' \code{across}-argument of \code{plotCorrplot()} to visualize the results of \code{correlateAcross()}.
#'
#' @return 
#' @export
#'
correlateAll <- function(object, method_corr = NULL, phase = NULL, print_errors = FALSE, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_obj <- getCorrConv(object, phase = phase)
  
  corr_obj <- confuns::correlate_all(corr.obj = corr_obj, methods.corr = method_corr)
  
  object@analysis$correlation[[phase]] <- corr_obj
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}

#' @rdname correlateAll
#' @export
correlateAcross <- function(object, 
                            phase = NULL, 
                            across = NULL, 
                            method_corr = NULL, 
                            verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_obj <- getCorrConv(object, phase = phase)
  
  corr_obj <- 
    confuns::correlate_across(
      corr.obj = corr_obj, 
      across = across, 
      methods.corr = method_corr, 
      verbose = verbose
    )
  
  object@analysis$correlation[[phase]] <- corr_obj
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}




# get ---------------------------------------------------------------------








# plotting ----------------------------------------------------------------

#' @title Plot a correlation plot
#' 
#' @description Visualizes the correlation results computed by \code{correlateAll()} and \code{correlateAcross()}. 
#'
#' @inherit argument_dummy params
#' @param variables_subset Character vector or NULL. Specifies the numeric variables you 
#' want to be included in the correlation plot.
#' 
#' If set to NULL all of them are chosen. You can prefix variables you do NOT want to influence the clustering
#' with a \emph{'-'}. (Saves writing if there are more variables you are interested in
#' than variables you are not interested in.)
#' 
#' Use \code{getNumericVariableNames()} to obtain all valid input options.
#' @param plot_type Character value. Either \emph{'upper'}, \emph{'lower'} or \emph{'complete'}. Specifies
#' how the correlation matrix is displayed. 
#' @param display_diagonal Logical value. Specifies if the diagonal of the correlation matrix is supposed to be included 
#' in the correlation plot. 
#' @param signif_level Numeric value or NULL. If numeric, specifies the minimum significance level a correlation pair 
#' must feature in order to be displayed. Insignificant correlation values are crossed out. 
#' @param clr_low Character value. Specifies the color used for the lower end of the colorspectrum (negative correlations).
#' @param clr_high Character value. Specifies the color used for the upper end of the colorspectrum (positive correlations).
#' @param shape Character value. Specifies the geometric objects with which to display the correlation pairs. Either \emph{'tile'} or one of
#' \emph{'circle'} and \emph{'rect'}. In the latter two cases the size of the geometric objects can be used to emphasize the correlation 
#' in addtion to the colorspectrum if \code{size_aes} is set to TRUE. 
#' @param shape_size Numeric value. Specifies the size with which to display the circles or rectangulars. If \code{shape_aes} is set to TRUE
#' it Specifies the maximum size possible. 
#' @param size_aes Logical value. If set to TRUE the size of the circles or rectangulars is used to display the correlation. 
#' @param display_values Logical value. If set to TRUE the actual correlation values are printed on the geometric objects.
#' @param values_alpha Numeric value. Specifies the transparency of the values. 
#' @param values_clr Character value. Specifies the color of the values.
#' @param values_digits Numeric value. Specifies the number of digits to which the correlation values are rounded before beeing displayed. 
#' @param values_size Numeric value. Specifies the size of the values. 
#' @param draw_grid Logical value. If set to TRUE a grid is drawn separating all circles or shapes.
#' @param grid_clr Character value. Specifies the color of the grid. 
#' @param grid_size Numeric value. Specifies the thickness of the grid's lines.
#' 
#' @inherit ggplot_family return
#' @export
#'
plotCorrplot <- function(object, 
                         phase = NULL,
                         method_corr = NULL, 
                         across = NULL, 
                         across_subset = NULL, 
                         relevel = NULL, 
                         variables_subset = NULL, 
                         plot_type = "lower", 
                         display_diagonal = TRUE, 
                         signif_level = NULL, 
                         clr_low = "darkred", 
                         clr_high = "steelblue",
                         shape = "tile",
                         shape_size = 15,
                         size_aes = FALSE,
                         display_values = TRUE, 
                         values_alpha = 0.9, 
                         values_clr = "black", 
                         values_digits = 2, 
                         values_size = 4, 
                         draw_grid = TRUE, 
                         grid_clr = "black", 
                         grid_size = 0.5, 
                         ncol = NULL, 
                         nrow = NULL){
  
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_obj <- getCorrConv(object, phase = phase)
  
  
  if(base::is.null(across)){
    
    confuns::plot_corrplot(
      corr.input = corr_obj, 
      variables.subset = variables_subset, 
      plot.type = plot_type, 
      display.diagonal = display_diagonal, 
      signif.level = signif_level, 
      clr.low = clr_low, 
      clr.high = clr_high, 
      shape = shape, 
      size.max = shape_size, 
      display.with.size = size_aes, 
      display.values = display_values, 
      values.alpha = values_alpha, 
      values.clr = values_clr, 
      values.digits = values_digits, 
      values.size = values_size, 
      draw.grid = draw_grid,
      grid.clr = grid_clr, 
      grid.size = grid_size
    )
    
  } else if(base::is.character(across)){
    
    confuns::plot_corrplots(
      corr.obj = corr_obj, 
      across = across, 
      across.subset = across_subset, 
      variables.subset = variables_subset, 
      method.corr = method_corr, 
      plot.type = plot_type, 
      display.diagonal = display_diagonal, 
      signif.level = signif_level, 
      clr.low = clr_low, 
      clr.high = clr_high, 
      shape = shape, 
      size.max = shape_size, 
      display.with.size = size_aes, 
      display.values = display_values, 
      values.alpha = values_alpha, 
      values.clr = values_clr, 
      values.digits = values_digits, 
      values.size = values_size, 
      draw.grid = draw_grid,
      grid.clr = grid_clr, 
      grid.size = grid_size, 
      nrow = nrow, 
      ncol = ncol
    )
    
  }
  
  
  
  
}



#' @title Plot correlation variance across groups
#' 
#' @description Uses the style of correlation matrices to visualize 
#' the standard deviation of the correlation values across groups of 
#' a grouping variable - indicating which grouping is responsible for changes
#' in correlation. 
#'
#' @inherit plotCorrelation params 
#' 
#' @param across Character vector (!). Denotes all grouping variables of interest. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotCorrelationSD <- function(object, 
                              method_corr = NULL, 
                              phase = NULL,
                              across = NULL, 
                              relevel = NULL, 
                              variables_subset = NULL, 
                              signif_level = NULL, 
                              clrsp = NULL){
  
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_obj <- getCorrConv(object, phase = phase)
  
  confuns::plot_correlation_sd(
    corr.obj = corr_obj, 
    method.corr = method_corr, 
    across = across, 
    aes.fill = "sd", 
    signif.level = signif_level
  ) + 
  confuns::scale_color_add_on(
    aes = "fill", 
    clrsp = clrsp
  ) + 
    ggplot2::labs(fill = "SD")
  
}




