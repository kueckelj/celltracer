
# Pam - clustering --------------------------------------------------------

#' Title
#' 
#' @description Performs pam-clustering with the respective variables, during
#' the denoted phase for all values denoted in input for argument \code{k_values}.
#' 
#' The name of the respective clustering results is assembled as a combination of 
#' \emph{'pam_k'} and the respective value for k. 
#'
#' @inherit check_object params
#' @param k_values Numeric vector. Denotes all k-values of interest. 
#' @param stand Given to argument \code{stand} of function \code{cluster::pam()}. Denotes 
#' whether the numeric variables are scaled before the clsutering process or not. Default 
#' is TRUE. 
#' @param variables The numeric variables upon which the clusters are calculated. Valid inputs can 
#' be obtained with the function \code{getStatVariableNames()}.
#' 
#' The default is set to \emph{'all'} which includes all numeric values. 
#' 
#' @inherit phase_single params 
#' @param ... Additional arguments given to \code{cluster::pam()}.
#' @inherit verbose params
#'
#' @inherit updated_object return
#' 
#' @seealso getPamObject(), getClusterNames()
#' 
#' @export
#'

doPamClustering <- function(object,
                            k_values,
                            stand = TRUE,
                            phase = "first_tmt",
                            variables = "all",
                            ...,
                            verbose = TRUE){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  if(!base::all(k_values %in% 3:10)){
    
    base::stop("All input values for argument k need to be between 3 and 10.")
    
  }
  
  n_k_values <- base::length(k_values)
  
  for(i in base::seq_along(k_values)){
    
    k <- k_values[i]
    
    if(base::isTRUE(verbose)){
      
      base::message(glue::glue("Calculating clusters with k = {k}. ({i}/{n_k_values})"))
      
    }
    
    num_mtr <- 
      getStats(object, phase = phase) %>% 
      dplyr::select_if(.predicate = base::is.numeric) %>% 
      base::as.matrix()
    
    cluster_name <- stringr::str_c("pam_k", k, sep = "")
    
    cluster_obj <- 
      cluster::pam(x = num_mtr, k = k,
                   cluster.only = FALSE, 
                   stand = stand, ...)
    
    object@cluster_info[["pam"]][[phase]][[cluster_name]] <- 
      cluster_obj
    
    cluster_df <- 
      getStats(object) %>% 
      dplyr::select(cell_id)
    
    cluster_vec <- cluster_obj$clustering
    
    cluster_df[[cluster_name]] <- 
      base::factor(x = cluster_vec, levels = base::unique(cluster_vec))
    
    object <- addClusterVariable(object = object, 
                                 cluster_df = cluster_df,
                                 cluster_name = cluster_name,
                                 phase = phase)
    
  }

  
  base::return(object)
  
}











