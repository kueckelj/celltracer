

#' @title Detect outliers
#' 
#' @description There are several methods according to which outliers can be detected or 
#' defined. Which one to use strongly depends on the researchers question at hand. See details
#' for more information on which method does what exactly.
#'
#' @inherit argument_dummy
#' 
#' @details This function only detects outliers. It does not remove them from the object. Use \code{removeOutliers()}
#' for that matter.
#'
#' @return An updated celltracer object.
#' 
#' @seealso removeOutliers()
#' 
#' @export

detectOutliers <- function(object, method_outlier = "iqr", variable_names = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if("iqr" %in% method_outlier){
    
    object <- detect_outliers_iqr(object, variable_names = variable_names, verbose = verbose)
    
  }
  
  base::return(object)
  
}


#' @title Remove outliers from object
#' 
#' @description Removes the data from the cell ids detected by \code{detectOutliers()} from 
#' the object. As this changes the data set all clustering-, correlation- and dimensional 
#' reduction results are reset. 
#'
#' @inherit argument_dummy params 
#'
#' @return An updated celltracer object. 
#' 
#' @seealso detectOutliers()
#' @export

removeOutliers <- function(object, method_outlier = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  outlier_methods <-
    getOutlierResults(object, method_outlier = method_outlier) %>% 
    base::names()
  
  outlier_ids <- getOutlierIds(object, check = TRUE)
  
  n_outliers <- base::length(outlier_ids)
  
  if(n_outliers == 0){
    
    confuns::give_feedback(msg = "No outliers were detected by all specified methods. Returning original input object.")
    
  } else {
    
    msg <- glue::glue("Removing {n_outliers} {ref_outliers}.", 
                      ref_outliers = confuns::adapt_reference(outlier_ids, "outlier", "outliers"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    
    object@data$tracks <- 
      purrr::map(.x = object@data$tracks, .f = ~ dplyr::filter(.x, !cell_id %in% {{outlier_ids}}))
    
    object@data$stats <- 
      purrr::map(.x = object@data$stats, .f = ~ dplyr::filter(.x, !cell_id %in% {{outlier_ids}}))
    
    object@data$meta <- 
      dplyr::filter(object@data$meta, !cell_id %in% {{outlier_ids}})
    
    object@data$grouping <- 
      purrr::map(.x = object@data$grouping,
                 .f = ~ dplyr::filter(.x, !cell_id %in% {{outlier_ids}})
      )
    
    object@analysis <- list()
    
    object@information$outliers_removed <-
      list(ids = outlier_ids, methods = outlier_methods)
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


#' @title Outlier detection functions
detect_outliers_iqr <- function(object, verbose = NULL, variable_names = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  confuns::give_feedback(msg = "Running outlier detection with method = 'iqr'")
  
  outlier_results <- 
    purrr::map(.x = getPhases(object), 
               .f = function(phase){
                 
                 stat_df <-
                   getStatsDf(object, phase = phase, with_cluster = FALSE, with_meta = FALSE) %>% 
                   dplyr::select(cell_id, dplyr::all_of(variable_names))
                 
                 numeric_vars <-
                   dplyr::select_if(stat_df, .predicate = base::is.numeric) %>% 
                   base::colnames()
                 
                 outlier_list <- 
                   purrr::map(.x = numeric_vars, .f = function(num_var){
                     
                     variable <- base::as.numeric(stat_df[[num_var]])
                     
                     iqr_res <- grDevices::boxplot.stats(variable)
                     
                     outlier_values <- iqr_res$out
                     
                     outlier_positions <- base::which(variable %in% outlier_values)
                     
                     outlier_ids <- dplyr::pull(stat_df[outlier_positions,], var = "cell_id")
                     
                     base::return(outlier_ids)
                     
                   }) %>% 
                   purrr::set_names(nm = numeric_vars)
                 
                 
                 
               }) %>% 
    purrr::set_names(getPhases(object))
  
  object@analysis$outlier_detection$iqr <- outlier_results
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}



