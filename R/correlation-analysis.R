


#' @title Set up correlation with cypro
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
#' @inherit update_object return
#' @export
#'
initiateCorrelation <- function(object,
                                variable_set,
                                phase = NULL, 
                                force = FALSE, 
                                verbose = NULL, 
                                ...){
  
  check_object(object)
  assign_default(object)
  
  variables <- getVariableSet(object, variable_set = variable_set)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    corr_object <- object@analysis$correlation[[variable_set]][[phase]]
    
  } else {
    
    corr_object <- object@analysis$correlation[[variable_set]]
    
  }
  
  
  if(base::class(corr_object) != "corr_conv" | base::isTRUE(force)){

    stat_df <- 
      getStatsDf(object, with_cluster = FALSE, with_meta = FALSE, with_well_plate = FALSE) %>% 
      dplyr::select(cell_id, dplyr::all_of(variables))
    
    corr_object <- confuns::initiate_corr_object(corr.data = stat_df, key.name = "cell_id")
    
    msg <- glue::glue("Successfully initiated correlation analysis {ref_phase}with '{variable_set}'-variables: '{variables}'", 
                      variables = glue::glue_collapse(x = variables, sep = "', '", last = "' and '", width = 100), 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
  } else {
    
    msg <- glue::glue("Correlation analysis {ref_phase}with variable set '{variable_set}' already exists. Set argument 'force' to TRUE in order to overwrite it.", 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object <-
    setCorrConv(object = object,
                corr_object = corr_object,
                variable_set = variable_set,
                phase = phase)
  
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
#' If set to NULL defaults to all grouping variables obtained via \code{getGroupingVariableNames()}.
#' 
#' 
#' @details \code{correlateAcross()} iterates over all grouping variables and computes the 
#' correlation analysis for every group the respective grouping variables suggests. Use the 
#' \code{across}-argument of \code{plotCorrplot()} to visualize the results of \code{correlateAcross()}.
#'
#' @return 
#' @export
#'
correlateAll <- function(object, variable_set, method_corr = NULL, phase = NULL, print_errors = FALSE, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_object <- getCorrConv(object, variable_set = variable_set, phase = phase)
  
  corr_object <- confuns::correlate_all(corr.obj = corr_object, methods.corr = method_corr)
  
  object <- setCorrConv(object, corr_object = corr_object, variable_set = variable_set, phase = phase)
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}

#' @rdname correlateAll
#' @export
correlateAcross <- function(object, 
                            variable_set, 
                            phase = NULL, 
                            across = NULL, 
                            method_corr = NULL, 
                            verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_object <- getCorrConv(object, variable_set = variable_set, phase = phase)
  
  corr_object <- 
    confuns::correlate_across(
      corr.obj = corr_object, 
      across = across, 
      methods.corr = method_corr, 
      verbose = verbose, 
      print.errors = TRUE
    )
  
  object <-
    setCorrConv(object, corr_object = corr_object, variable_set = variable_set, phase = phase)
  
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
#' @param plot_type Character value. Either \emph{'upper'}, \emph{'lower'} or \emph{'complete'}. Specifies
#' how the correlation matrix is displayed. 
#' @param display_diagonal Logical value. Specifies if the diagonal of the correlation matrix is supposed to be included 
#' in the correlation plot. 
#' @param signif_level Numeric value or NULL. If numeric, specifies the minimum significance level a correlation pair 
#' must feature in order to be displayed. Insignificant correlation values are crossed out. Argument \code{shape_size} denotes 
#' the size of the crosses.  
#' @param clr_low Character value. Specifies the color used for the lower end of the colorspectrum (negative correlations).
#' @param clr_high Character value. Specifies the color used for the upper end of the colorspectrum (positive correlations).
#' @param shape Character value. Specifies the geometric objects with which to display the correlation pairs. Either \emph{'tile'} or one of
#' \emph{'circle'} and \emph{'rect'}. In the latter two cases the size of the geometric objects can be used to emphasize the correlation 
#' in addtion to the colorspectrum if \code{size_aes} is set to TRUE. 
#' @param shape_size Numeric value. Specifies the size with which to display the circles and rectangulars. If \code{shape_aes} is set to TRUE
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
                         variable_set,
                         method_corr = NULL, 
                         across = NULL, 
                         across_subset = NULL, 
                         relevel = NULL, 
                         variables_subset = NULL, 
                         phase = NULL,
                         plot_type = "lower", 
                         display_diagonal = TRUE, 
                         signif_level = NULL, 
                         clr_low = "darkred", 
                         clr_high = "steelblue",
                         shape = "tile",
                         shape_size = 15,
                         size_aes = FALSE,
                         display_values = FALSE, 
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
  
  corr_object <- getCorrConv(object, variable_set = variable_set, phase = phase)
  
  
  if(base::is.null(across)){
    
    confuns::plot_corrplot(
      corr.input = corr_object, 
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
      corr.obj = corr_object, 
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
#' @inherit plotCorrplot params 
#' 
#' @param across Character vector (!). Denotes all grouping variables of interest. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotCorrelationSD <- function(object, 
                              variable_set,
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
  
  corr_object <- getCorrConv(object, variable_set = variable_set, phase = phase)
  
  confuns::plot_correlation_sd(
    corr.obj = corr_object, 
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




