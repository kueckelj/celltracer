

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
#' @inherit updated_object return
#' 
#' @seealso removeOutliers()
#' 
#' @export

detectOutliers <- function(object,
                           method_outlier = "iqr",
                           threshold_pval = 0.001,
                           variable_names = NULL,
                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if("iqr" %in% method_outlier){
    
    object <- detect_outliers_iqr(object, variable_names = variable_names, verbose = verbose)
    
  }
  
  if("mahalanobis" %in% method_outlier){
    
    object <- detect_outliers_mahalanobis(object, variable_names = variable_names, 
                                          verbose = verbose, threshold_pval = threshold_pval)
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  
  
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
#' @inherit updated_object return
#' 
#' @seealso detectOutliers()
#' @export

removeOutliers <- function(object, method_outlier = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  outlier_methods <-
    getOutlierResults(object, method_outlier = method_outlier) %>% 
    base::names()
  
  outlier_ids <- getOutlierIds(object, method_outlier = method_outlier, check = TRUE)
  
  cell_ids <- getStatsDf(object) %>% dplyr::pull(cell_id)
  
  keep_ids <- cell_ids[!cell_ids %in% outlier_ids]
  
  n_outliers <- base::length(outlier_ids)
  
  if(n_outliers == 0){
    
    confuns::give_feedback(msg = "No outliers were detected by all specified methods. Returning original input object.")
    
  } else {
    
    msg <- glue::glue("Removing {n_outliers} {ref_outliers}.", 
                      ref_outliers = confuns::adapt_reference(outlier_ids, "outlier", "outliers"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    object <- subsetByCellId(object, cell_ids = keep_ids, verbose = verbose)
    
    object@analysis <- list()
    
    object@information$outliers_removed <-
      list(ids = outlier_ids, methods = outlier_methods)
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


#' @title Outlier detection functions
detect_outliers_iqr <- function(object, variable_names = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  confuns::give_feedback(msg = "Running outlier detection with method = 'iqr'")
  
  outlier_results <- 
    purrr::map(.x = getPhases(object), # in case of non time lapse experiment phase is ignored anyway
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
  
  if(!multiplePhases(object)){
    
    outlier_results <- outlier_results[[1]]
    
  }
  
  object@analysis$outlier_detection$iqr <- outlier_results
  
  msg <- glue::glue("Found {n} {ref_outliers}.",
                    n = base::length(getOutlierIds(object, method_outlier = "iqr")), 
                    ref_outliers = confuns::adapt_reference(getOutlierIds(object), "outlier", "outliers"))
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}


#' @rdname detect_outliers_iqr
detect_outliers_mahalanobis <- function(object, variable_names = NULL, threshold_pval = 0.001, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  outlier_results <- 
    purrr::map(.x = getPhases(object), # in case of non time lapse experiment phase is ignored anyway
               .f = function(phase){
                 
                 stat_df <-
                   getStatsDf(object, phase = phase, with_cluster = FALSE, with_meta = FALSE) %>% 
                   dplyr::select(cell_id, dplyr::all_of(variable_names)) %>% 
                   tibble::column_to_rownames(var = "cell_id")
                 
                 stat_df$mahal <- 
                   stats::mahalanobis(x = stat_df,
                                      center = base::colMeans(stat_df), 
                                      cov = stats::cov(stat_df)
                                      )
                 
                 stat_df$pval <- 
                   stats::pchisq(q = stat_df$mahal, 
                                 df = base::length(variable_names)-1, 
                                 lower.tail = FALSE
                                 )
                 
                 outlier_ids <- 
                   tibble::rownames_to_column(stat_df, var = "cell_id") %>% 
                   dplyr::filter(pval <= {{threshold_pval}}) %>% 
                   pull(cell_id)
                 
                 base::return(outlier_ids)
                 
               }) %>% 
    purrr::set_names(getPhases(object))
  
  if(!multiplePhases(object)){
    
    outlier_results <- outlier_results[[1]]
    
  }
  
  object@analysis$outlier_detection$mahalanobis <- 
    list(
      outlier_ids = outlier_results, 
      threshold_pval = threshold_pval, 
      variable_names = variable_names
    )
  
  msg <- glue::glue("Found {n} {ref_outliers}.",
                    n = base::length(getOutlierIds(object, method_outlier = "mahalanobis")), 
                    ref_outliers = confuns::adapt_reference(getOutlierIds(object), "outlier", "outliers"))
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}




