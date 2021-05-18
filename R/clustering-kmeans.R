







#' @title Compute cluster according to Kmeans
#' 
#' @description Performs kmeans clustering for every combination of \code{method_kmeans} and 
#' \code{k} and saves the results in the celltracer object. 
#'
#' @inherit argument_dummy params
#' @param k Numeric vector. For every value the kmeans cluster assignment is calculated via 
#' \code{stats::kmeans()}. 
#' 
#' @details As this function iterates over all valid combinations of \code{k} and \code{method_kmeans}
#' both inputs can be specified as vectors. 
#' 
#' @return An updated celltracer object. 
#' 
#' @export
#'

computeKmeansCluster <- function(object,
                                 variable_set,
                                 k,
                                 phase = NULL, 
                                 method_kmeans = NULL, 
                                 verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  confuns::is_vec(k, mode = "numeric")
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  cluster_object <- getKmeansConv(object, variable_set = variable_set, phase = phase)
  
  cluster_object <- 
    confuns::perform_kmeans_clustering(
      kmeans.obj = cluster_object, 
      centers = k, 
      methods.kmeans = method_kmeans, 
      verbose = verbose, 
      verbose.pb = verbose
    )
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "kmeans", 
                           phase = phase, 
                           variable_set = variable_set)
  
  base::return(object)
  
}



# plotting ----------------------------------------------------------------


#' @title Plot a scree plot
#' 
#' @description Visualizes the within-group sum of squares of every calculated kmeans
#' clustering. 
#'
#' @inherit argument_dummy params
#' @param clr Character value. Denotes the color of the columns.
#' 
#' @details If \code{method_kmeans} is a vector a subplot for all valid input values 
#' is displayed via \code{ggplot2::facet_wrap()}. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotScreeplot <- function(object, 
                          variable_set, 
                          phase = NULL, 
                          method_kmeans = NULL,
                          k = 2:10, 
                          clr = "steelblue",
                          display_cols = TRUE, 
                          display_line = TRUE, 
                          display_points = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_object <- 
    getKmeansConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)
  
  confuns::plot_screeplot(
    kmeans.obj = cluster_object, 
    methods.kmeans = method_kmeans, 
    clr = clr, 
    display.cols = display_cols, 
    display.line = display_line, 
    display.points = display_points
  )
  
}



