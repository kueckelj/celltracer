





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
                              phase = NULL, 
                              method_pam = NULL, 
                              k = NULL, 
                              verbose = NULL, 
                              ...){
  
  check_object(object)
  assign_default(object)
  
  confuns::is_vec(k, mode = "numeric")
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$clustering$pam[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "pam_conv",
    phase = phase, 
    ref_input = "partitioning around medoids (PAM) clustering object", 
    ref_fun = "initiatePamClustering()"
  )
  
  cluster_obj <- 
    confuns::perform_pam_clustering(
      pam.obj = cluster_obj, 
      k = k, 
      metric.pam = method_pam, 
      verbose = verbose, 
      verbose.pb = FALSE, 
      ...
    )
  
  object@analysis$clustering$pam[[phase]] <- cluster_obj
  
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
  
  cluster_obj <- getPamConv(object, phase = phase)
  
  confuns::plot_avg_silhouette_widths(
    pam.obj = cluster_obj, 
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
  
  cluster_obj <- getPamConv(object, phase = phase)
  
  confuns::plot_silhouette_widths(
    pam.obj = cluster_obj, 
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
                           k, 
                           method_pam = NULL, 
                           phase = NULL, 
                           clrp = NULL, 
                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  confuns::is_value(k, mode = "numeric")
  
  cluster_obj <- getPamConv(object, phase = phase)
  
  confuns::plot_medoid_barchart(
    pam.obj = cluster_obj, 
    metric.pam = method_pam,
    k = k, 
    verbose = verbose
  )
  
}


