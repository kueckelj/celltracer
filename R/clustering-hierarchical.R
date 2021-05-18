




#' @title Compute distance matrices 
#' 
#' @description Calculates distance matrices for every valid input value of argument \code{method_dist} and stores 
#' the results in the celltracer object. Requires that \code{initiateHierarchicalClustering()} has been called. 
#'
#' @inherit argument_dummy params
#' 
#' @details \code{computeDistanceMatrices()} is the second step in the convenient hierarchical clustering 
#' of celltracer. It calculates the distance matrices according to all methods you are interested in in a
#' loop. Argument \code{method_dist} therefore either takes a value or a vector of valid character strings.  
#' Each input value is given to argument \code{method} of function \code{stats::dist()}.
#' 
#' Based on the distance matrices computed and saved this way the function \code{agglomerateHierarchicalCluster()} can be used as the third 
#' step of the hierarchical clustering pipeline.
#'
#' @return An updated celltracer object.
#' @export

computeDistanceMatrices <- function(object,
                                    variable_set,
                                    phase = NULL,
                                    method_dist = NULL,
                                    force = FALSE,
                                    p = 2,
                                    verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_object <- getHclustConv(object, variable_set = variable_set, phase = phase)
  
  cluster_object <- 
    confuns::compute_distance_matrices(
      hcl.obj = cluster_object, 
      methods.dist = method_dist, 
      p = p, 
      force = force, 
      verbose = verbose
    )
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "hclust", 
                           phase = phase, 
                           variable_set = variable_set)  
  base::return(object)
  
}


#' @title Agglomerate hierarchical cluster 
#' 
#' @description Agglomerates the existing distance measurements to hierarchical trees. 
#'
#' @inherit argument_dummy params
#' 
#' @details \code{agglomerateHierarchicalCluster()} is the third step in the convenient hierarchical clustering
#' pipeline of celltracer. It uses the distance matrices calculated with \code{computeDistanceMatrices()} and denoted in the 
#' argument \code{method_dist} and agglomerates them to hierarchical trees according to all methods denoted 
#' in argument \code{method_aggl} (input for the latter is given to argument \code{method} of function \code{stats::hclust()}).
#' Both \code{method_*}-arguments therefore take a vector of character strings as input.
#' 
#' Use \code{plotDendrogram()} to visualize the agglomeration results of the distance/agglomeration combinations of interest and 
#' \code{addHierarchicalCluster} - the fourth and final step - to make the clustering results available for all other plotting 
#' functions and the \code{across}-argument. 
#'
#' @return An updated celltracer object. 
#' @export
#'
agglomerateHierarchicalCluster <- function(object,
                                           variable_set,
                                           phase = NULL,
                                           method_dist = NULL,
                                           method_aggl = NULL,
                                           force = FALSE,
                                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_object <- getHclustConv(object, variable_set = variable_set, phase = phase)
  
  cluster_object <-
    confuns::compute_hierarchical_cluster(
      hcl.obj = cluster_object, 
      methods.aggl = method_aggl, 
      methods.dist = method_dist, 
      verbose = verbose
    )
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "hclust", 
                           phase = phase, 
                           variable_set = variable_set)  
  base::return(object)
  
}




# get-functions -----------------------------------------------------------


#' @title Obtain the original clustering objects 
#' 
#' @description These functions extract the resulting objects 
#' of the respective algorithms calculated with the respective methods denoted 
#' by the \code{method_*}-arguments. 
#'
#' @inherit argument_dummy params
#' 
#' @details In this case all \code{method_*}-arguments must be specified as 
#' character values as the objects are returned one by one. 
#'
#' @return The respective clustering results as their original objects. 
#' @export
#'
getHclustObj <- function(object,
                         variable_set, 
                         phase = NULL,
                         method_dist = NULL,
                         method_aggl = NULL){
  
  check_object(object)
  assign_default(object)
  
  confuns::are_values("method_dist", "method_aggl")
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_object <- getHclustConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)
  
  hclust_obj <- cluster_object@hclust_results[[method_dist]][[method_aggl]]
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "hclust_conv",
    phase = phase, 
    ref_input = glue::glue("object hclust of distance method '{method_dist}' and agglomerative method '{method_aggl}'"), 
    ref_fun = "agglomerateHierarchicalCluster()"
  )
  
  base::return(hclust_obj)
  
}




# miscellaneous -----------------------------------------------------------


#' @title Discard calculated distance matrix
#' 
#' @description Allows to discard distance matrices that have been already used 
#' by \code{agglomerateHierarchicalCluster()}. (Distance matrices are usually 
#' objects of several hundred megabytes.) Once discarded it can not be used any longer but 
#' can be calculated again via \code{computeDistanceMatrix()}.
#'
#' @inherit argument_dummy params
#' 
#' @details Distance matrices are discarded one by one. Input for argument \code{method_dist}
#' must be a single character value.
#' 
#' @return An updated celltracer object. 
#' @export
#'
discardDistanceMatrix <- function(object,
                                  variable_set,
                                  phase = NULL,
                                  method_dist = NULL,
                                  verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  confuns::is_value(method_dist, mode = "character")
  
  cluster_object <- getHclustConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)

  check_availability(
    evaluate = !base::is.null(cluster_object@dist_matrices[[method_dist]]), 
    phase = phase, 
    ref_input = glue::glue("distance matrix (method = {method_dist})"), 
    ref_fun = "computeDistanceMatrices()"
  )
  
  cluster_object@dist_matrices[[method_dist]] <- NULL
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "hclust", 
                           phase = phase, 
                           variable_set = variable_set)
  
  confuns::give_feedback(msg = "Discarded.", verbose = verbose)

  base::return(object)    
  
}



# plotting ----------------------------------------------------------------



#' @title Visualize hierarchical clustering 
#' 
#' @description Plots the hierarchical tress agglomerated by \code{agglomerateHierarchicalCluster()}. 
#'
#' @inherit argument_dummy params 
#' @param k Numeric value or NULL. If numeric the branches are colored according to their cluster assignment
#' whereby the number of clusters is equal to \code{k}. 
#' @param h Numeric value or NULL. If numeric the branches are colored according to their cluster assignment
#' whereby the number of clusters is equal to the results of cutting the hierarchical tree at height \code{h}..
#' @param branch_size Numeric value. Denotes the thickness of the branches. 
#' @param ... Additional arguments given to \code{ggdendro::ggdendrogram()}.
#' 
#' @details Iterates over all valid combinations of the \code{method_*} arguments and creates a dendrogram
#' if the respective clustering results are found. \code{nrow} and \code{ncol} can be used to 
#' align the plots. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotDendrogram <- function(object, 
                           variable_set,
                           phase = NULL, 
                           method_dist = NULL, 
                           method_aggl = NULL, 
                           k = NULL, 
                           h = NULL,
                           branch_size = 1, 
                           display_legend = TRUE, 
                           display_title = TRUE, 
                           clrp = "milo", 
                           clrp_adjust = NULL, 
                           ncol = NULL, 
                           nrow = NULL, 
                           verbose = NULL, 
                           ...
                           ){

  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_object <- getHclustConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)  
  
  if(base::length(method_dist) == 1 & base::length(method_aggl) == 1){
    
    msg <- "Plotting dendrogram. This might take a few moments."
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    p <- 
    confuns::plot_dendrogram(
      hcl.obj = cluster_object, 
      method.dist = method_dist, 
      method.aggl = method_aggl, 
      k = k, 
      h = h, 
      branch.size = branch_size, 
      display.legend = display_legend, 
      display.title = display_title, 
      clrp = clrp, 
      clrp.adjust = clrp_adjust, 
      display.labels = FALSE,
      ...
    )
    
  } else {
    
    p <- 
    confuns::plot_dendrograms(
      hcl.obj = cluster_object, 
      methods.dist = method_dist, 
      methods.aggl = method_aggl, 
      k = k, 
      h = h, 
      branch.size = branch_size, 
      display.legend = display_legend, 
      display.title = display_title, 
      clrp = clrp, 
      clrp.adjust = clrp_adjust, 
      display.labels = FALSE,
      nrow = nrow, 
      ncol = ncol, 
      ...
    )
    
  }
  
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(p)
  
}




