





#' @title Compute cluster accordint to Partitioning Around Medoids (PAM)
#'
#' @description Performs partitioning around medoids for every combination of 
#' \code{method_pam} and \code{k} and saves the results in the celltracer object. 
#'
#' @inherit argument_dummy params 
#' @param k Numeric vector. Denotes the numbers of clusters the pam-algorithm 
#' is supposed to assign return. Values must be bigger than 2. 
#' @param ... Additional arguments given to \code{cluster::pam()}.
#' 
#' @details As this function iterates over all valid combinations of \code{method_pam}
#' and \code{k} both inputs can be specified as vectors.
#'
#' @return An updated celltracer object. 
#' @export
#'
computePamCluster <- function(object,
                              variable_set,
                              k,
                              phase = NULL, 
                              method_pam = NULL, 
                              verbose = NULL, 
                              ...){
  
  check_object(object)
  assign_default(object)
  
  confuns::is_vec(k, mode = "numeric")
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_object <- getPamConv(object, variable_set = variable_set, phase = phase)
  
  cluster_object <- 
    confuns::perform_pam_clustering(
      pam.obj = cluster_object, 
      k = k, 
      metric.pam = method_pam, 
      verbose = verbose, 
      verbose.pb = FALSE, 
      ...
    )
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "pam", 
                           phase = phase, 
                           variable_set = variable_set)
  
  base::return(object)
  
}




# get ---------------------------------------------------------------------










# plotting ----------------------------------------------------------------

#' @title Plot pam cluster quality
#' 
#' @description Visualizes the cluster quality of different partitioning 
#' around medoids results. 
#'
#' @inherit argument_dummy params
#' @param k Numeric vector. The k-values of interest. 
#' 
#' @details Specify \code{method_pam} as a character value.
#'
#' @inherit ggplot_family return
#' @export
#'
plotAvgSilhouetteWidths <- function(object,
                                    variable_set,
                                    k, 
                                    method_pam = NULL, 
                                    phase = NULL, 
                                    clr = "steelblue", 
                                    display_cols = TRUE, 
                                    display_line = TRUE, 
                                    display_points = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  confuns::is_vec(k, mode = "numeric", min.length = 2)
  
  cluster_object <- getPamConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)
  
  confuns::plot_avg_silhouette_widths(
    pam.obj = cluster_object, 
    metric.pam = method_pam, 
    k = k, 
    display.cols = display_cols, 
    display.line = display_line, 
    display.points = display_points
  )
  
}

#' @rdname plotAvgSilhouetteWidths
#' @export
plotSilhouetteWidths <- function(object, 
                                 variable_set, 
                                 k, 
                                 method_pam = NULL, 
                                 phase = NULL, 
                                 clrp = NULL, 
                                 ncol = NULL, 
                                 nrow = NULL, 
                                 verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  confuns::is_vec(k, mode = "numeric", min.length = 2)
  
  cluster_object <- getPamConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)
  
  confuns::plot_silhouette_widths(
    pam.obj = cluster_object, 
    metric.pam = method_pam, 
    k = k, 
    clrp = clrp, 
    ncol = ncol, 
    nrow = nrow, 
    verbose = verbose
  )
  
  
}




#' @title Plot medoid results
#' 
#' @description Visualizes the values of the observations that were identified 
#' as the medoids giving insight in the overall qualities that constitute the 
#' clusters. 
#'
#' @inherit argument_dummy params
#'
#' @inherit ggplot_family return
#' @export
#'
plotPamMedoids <- function(object, 
                           variable_set,
                           k, 
                           method_pam = NULL, 
                           phase = NULL, 
                           clrp = NULL, 
                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  confuns::is_value(k, mode = "numeric")
  
  cluster_object <- getPamConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)
  
  confuns::plot_medoid_barchart(
    pam.obj = cluster_object, 
    metric.pam = method_pam,
    k = k, 
    verbose = verbose
  )
  
}


