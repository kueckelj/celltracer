


#' Title
#'
#' @param object 
#' @param phase 
#' @param force 
#' @param verbose 
#' @param ... 
#'
#' @return
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
      
    
    corr_obj <- initiate_corr_object(corr.data = stat_df)
    
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





#' Title
#'
#' @param object 
#' @param phase 
#' @param method_corr 
#' @param verbose 
#'
#' @return
#' @export
#'
correlateAll <- function(object, phase = NULL, method_corr = NULL, print_errors = FALSE, verbose = NULL){
  
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


#' Title
#'
#' @param object 
#' @param phase 
#'
#' @return
#' @export
#'
getCorrConv <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$correlation[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "corr_conv",
    phase = phase, 
    ref_input = "correlation object", 
    ref_fun = "initiateCorrelation()"
  )
  
  base::return(cluster_obj)
  
}





# plotting ----------------------------------------------------------------

#' Title
#'
#' @param object 
#' @param phase 
#' @param method_corr 
#' @param across 
#' @param across_subset 
#' @param relevel 
#' @param variables_subset 
#' @param plot_type 
#' @param display_diagonal 
#' @param signif_level 
#' @param clr_low 
#' @param clr_high 
#' @param shape 
#' @param shape_size 
#' @param size_aes 
#' @param display_values 
#' @param values_alpha 
#' @param values_clr 
#' @param values_digits 
#' @param values_size 
#' @param draw_grid 
#' @param grid_clr 
#' @param grid_size 
#' @param ncol 
#' @param nrow 
#'
#' @return
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



#' Title
#'
#' @param object 
#' @param phase 
#' @param method_corr 
#' @param across 
#' @param relevel 
#' @param variables_subset 
#' @param signif.level 
#' @param clrsp 
#'
#' @return
#' @export
#'
plotCorrerlationSD <- function(object, 
                               phase = NULL, 
                               method_corr = NULL, 
                               across = NULL, 
                               relevel = NULL, 
                               variables_subset = NULL, 
                               signif_level = NULL, 
                               clrsp = "viridis"){
  
  
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




