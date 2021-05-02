

# Documentations dummies --------------------------------------------------

#' dummy 
#' @return An updated cell tracer object that contains the data added.
add_family <- function(){}



# Functions ---------------------------------------------------------------

#' @title Add discrete/categorical variables
#' 
#' @description Allows to join new discrete/categorical variables that can be referred 
#' to via the \code{across}-argument of many functions.
#' 
#' @inherit argument_dummy params
#' @inherit check_object params 
#' @inherit cluster_df params
#' @inherit input_df params
#' @param cluster_name,variable_name Character value. The name of the variable that is to be joined.
#' 
#' @inherit add_family return
#'
#' @export

addClusterVariable <- function(object,
                               cluster_df,
                               cluster_name,
                               phase = NULL, 
                               overwrite = FALSE, 
                               verbose = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  new_cluster_df <- 
    dplyr::select(cluster_df, dplyr::all_of(x = c("cell_id", cluster_name))) %>% 
    dplyr::select(-phase)
                  
  old_cluster_df <- getClusterData(object, phase = phase)
  
  if(cluster_name %in% base::colnames(old_cluster_df)){
    
    if(base::isTRUE(overwrite)){
      
      base::message(glue::glue("Overwriting already existing cluster variable {cluster_name}."))
      
      old_cluster_df[[cluster_name]] <- NULL
      
    } else {
      
      base::stop(glue::glue("Cluster variable {cluster_name} already exists. Set argument 'overwrite' to TRUE in order to continue."))
      
    }
    
  }
  
  updated_cluster_df <- 
    dplyr::left_join(
      x = old_cluster_df, 
      y = new_cluster_df, 
      by = "cell_id"
    )
  
  #!!! add dplyr::rename() to equip the added variable with a
  
  object@data$cluster[[phase]] <- updated_cluster_df
  
  base::return(object)
  
}




#' @title Add hierarchical clustering results to overall data
#' 
#' @description Adds hierarchical clustering results in form of 
#' grouping variables to the object's overall data - making them available for the
#' \code{across}-argument.. 
#'
#' @inherit argument_dummy
#' @param k Numeric vector. Denotes the exact number of clusters in which the tree created 
#' according to the distance- and agglomeration method is supposed to be cut. 
#' @param h Numeric vector. Denotes the heights at which the hierarchical tree created 
#' according to the distance- and agglomeration method is supposed to be cut. 
#' 
#' @details The last step of the hierarchical clustering pipeline. This function iterates
#' over all combinations of \code{method_dist}, \code{method_aggl}, \code{k} and \code{h} and 
#' adds the respective clustering variable to the object's overall data named according to 
#' the following syntax: \emph{hcl_\code{method_dist}_\code{method_aggl}_k/h_\code{k}/\code{h}}.
#' 
#' Use \code{getgroupingOptions()} afterwards to obtain the exact names.
#'
#' @return
#' @export
#'
addHierarchicalCluster <- function(object,
                                   phase = NULL,
                                   method_dist = NULL,
                                   method_aggl = NULL,
                                   k = NULL,
                                   h = NULL, 
                                   verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  cluster_obj <- getHclustConv(object, phase = phase)
  
  new_cluster_df <- 
    confuns::get_hclust_df(
      hcl.obj = cluster_obj, 
      methods.dist = method_dist, 
      methods.aggl = method_aggl, 
      k = k, 
      h = h
    )
  
  new_cluster_names <- 
    dplyr::select(new_cluster_df, -cell_id) %>% 
    base::colnames()
  
  cluster_df <- object@data$cluster[[phase]]
  
  existing_cluster_names <- 
    dplyr::select(cluster_df, -cell_id, -phase) %>% 
    base::colnames()
  
  if(base::length(existing_cluster_names) >= 1){
    
    new_cluster_names <- 
      confuns::discard_if(
        input = new_cluster_names, 
        one_of = existing_cluster_names, 
        ref.input = "cluster variables to be added", 
        ref.of = "already part of existing cluster variables", 
        v.empty = NULL,
        ref.empty = "Cluster data stays the same",
        verbose = TRUE
      )
    
  }
  
  cluster_df <-
    dplyr::left_join(x = cluster_df, y = new_cluster_df[, c("cell_id", new_cluster_names)], by = "cell_id")
  
  object@data$cluster[[phase]] <- cluster_df
  
  if(!base::is.null(new_cluster_names)){
    
    msg <- glue::glue("Successfully added {n} cluster {ref_variables} to data of {phase} phase: '{ref_new_cluster_names}'.", 
                      n = base::length(new_cluster_names), 
                      ref_variables = confuns::adapt_reference(new_cluster_names, sg = "variable", pl = "variables"),
                      ref_new_cluster_names = glue::glue_collapse(new_cluster_names, sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
  }
  
  base::return(object)
  
}


#' @title Add Kmeans clustering results to overall data
#' 
#' @description Adds the clustering results of \code{computeKmeansCluster()} in form
#' of grouping variables to the object's overall data - making them available for the \code{across}-
#' argument. 
#'
#' @inherit argument_dummy params
#' @param k Numeric vector. All k-values of interest. 
#' 
#' @details The last step of the kmeans clustering pipeline. This function iterates
#' over all combinations of \code{method_kmeans} and \code{k} and 
#' adds the respective clustering variable to the object's overall data named according to 
#' the following syntax: \emph{kmeans_\code{method_kmeans}_k_\code{k}}.
#' 
#' Use \code{getgroupingOptions()} afterwards to obtain the exact names.
#' 
#' @return An updated celltracer object.
#' @export
#'
addKmeansCluster <- function(object,
                             phase = NULL,
                             method_kmeans = NULL, 
                             k = NULL, 
                             verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  cluster_obj <- getKmeansConv(object, phase = phase)
  
  new_cluster_df <- 
    confuns::get_kmeans_df(
      kmeans.obj = cluster_obj, 
      centers = k, 
      methods.kmeans = method_kmeans, 
      centers.string = "k"
    )
  
  new_cluster_names <- 
    dplyr::select(new_cluster_df, -cell_id) %>% 
    base::colnames()
  
  cluster_df <- object@data$cluster[[phase]]
  
  existing_cluster_names <- 
    dplyr::select(cluster_df, -cell_id, -phase) %>% 
    base::colnames()
  
  if(base::length(existing_cluster_names) >= 1){
    
    new_cluster_names <- 
      confuns::discard_if(
        input = new_cluster_names, 
        one_of = existing_cluster_names, 
        ref.input = "cluster variables to be added", 
        ref.of = "already part of existing cluster variables", 
        v.empty = NULL,
        ref.empty = "Cluster data stays the same",
        verbose = TRUE
      )
    
  }
  
  cluster_df <-
    dplyr::left_join(x = cluster_df, y = new_cluster_df[, c("cell_id", new_cluster_names)], by = "cell_id")
  
  object@data$cluster[[phase]] <- cluster_df
  
  if(!base::is.null(new_cluster_names)){
    
    msg <- glue::glue("Successfully added {n} cluster {ref_variables} to data of {phase} phase: '{ref_new_cluster_names}'.", 
                      n = base::length(new_cluster_names), 
                      ref_variables = confuns::adapt_reference(new_cluster_names, sg = "variable", pl = "variables"),
                      ref_new_cluster_names = glue::glue_collapse(new_cluster_names, sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
  }
  
  base::return(object)
  
}


#' @title Add PAM clustering results to overall data 
#' 
#' @description Adds the clustering results of \code{computePamClusters()} in form 
#' of grouping variables to the object's overall data - making them available for the \code{across}-
#' argument. 
#'
#' @inherit argument_dummy params 
#' @param k Numeric vector. All k-values of interest. 
#' 
#' @details The last step of the PAM clustering pipeline. This function iterates
#' over all combinations of \code{method_pam} and \code{k} and 
#' adds the respective clustering variable to the object's overall data named according to 
#' the following syntax: \emph{pam_\code{method_pam}_k_\code{k}}.
#' 
#' Use \code{getgroupingOptions()} afterwards to obtain the exact names.
#'
#' @return An updated celltracer object. 
#' 
#' @export
#'
addPamCluster <- function(object, 
                          phase = NULL, 
                          method_pam = NULL,
                          k = NULL, 
                          verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  cluster_obj <- getPamConv(object, phase = phase)
  
  new_cluster_df <-
    confuns::get_pam_df(
      pam.obj = cluster_obj, 
      metric.pam = method_pam, 
      k = k
    )
  
  new_cluster_names <- 
    dplyr::select(new_cluster_df, -cell_id) %>% 
    base::colnames()
  
  cluster_df <- object@data$cluster[[phase]]
  
  existing_cluster_names <- 
    dplyr::select(cluster_df, -cell_id, -phase) %>% 
    base::colnames()
  
  if(base::length(existing_cluster_names) >= 1){
    
    new_cluster_names <- 
      confuns::discard_if(
        input = new_cluster_names, 
        one_of = existing_cluster_names, 
        ref.input = "cluster variables to be added", 
        ref.of = "already part of existing cluster variables", 
        v.empty = NULL,
        ref.empty = "Cluster data stays the same",
        verbose = TRUE
      )
    
  }
  
  cluster_df <-
    dplyr::left_join(x = cluster_df, y = new_cluster_df[, c("cell_id", new_cluster_names)], by = "cell_id")
  
  object@data$cluster[[phase]] <- cluster_df
  
  if(!base::is.null(new_cluster_names)){
    
    msg <- glue::glue("Successfully added {n} cluster {ref_variables} to data of {phase} phase: '{ref_new_cluster_names}'.", 
                      n = base::length(new_cluster_names), 
                      ref_variables = confuns::adapt_reference(new_cluster_names, sg = "variable", pl = "variables"),
                      ref_new_cluster_names = glue::glue_collapse(new_cluster_names, sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
  }
  
  base::return(object)
  
}
