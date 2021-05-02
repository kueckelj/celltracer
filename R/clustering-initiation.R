



# Initiation --------------------------------------------------------------

#' @title Set up clustering objects with celltracer
#' 
#' @description These functions set up the necessary objects to perform clustering with the 
#' respective algorithm. 
#'
#' @inherit argument_dummy
#' @param variables_subset Character vector or NULL. Denotes the numeric variables the subsequent clustering 
#' steps will include which influences the clustering results. 
#' 
#' If set to NULL all of them are chosen. You can prefix variables you do NOT want to influence the clustering
#' with a \emph{'-'}. (Saves writing if there are more variables you are interested in
#' than variables you are not interested in.)
#' 
#' Use \code{getNumericVariableNames()} to obtain all valid input options.
#'
#' @return An updated celltracer object. 
#' @export
#'
initiateHierarchicalClustering <- function(object,
                                           phase = NULL, 
                                           variables_subset = NULL, 
                                           force = FALSE, 
                                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$clustering$hclust[[phase]]
  
  if(base::class(cluster_obj) != "hclust_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(-phase)
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- hlpr_select(stat_df, variables_subset = variables_subset)
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    cluster_obj <- 
      confuns::initiate_hclust_object(
        hclust.data = stat_df, 
        key.name = "cell_id",
        default.aggl = object@default$method_aggl, 
        default.dist = object@default$method_dist, 
        verbose = FALSE
      )
    
    msg <- glue::glue("Successfully initiated hierarchical clustering for {phase} phase with variables: '{remaining_vars}'", 
                      remaining_vars = glue::glue_collapse(x = base::colnames(stat_df), sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
    
  } else {
    
    msg <- glue::glue("Hierarchical clustering for {phase} phase already exists. Set argument 'force' to TRUE in order to overwrite it.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object@analysis$clustering$hclust[[phase]] <- cluster_obj
  
  base::return(object)
  
}

#' @rdname initiateHierarchicalClustering
#' @export
initiateKmeansClustering <- function(object,
                                     phase = NULL, 
                                     variables_subset = NULL, 
                                     force = FALSE, 
                                     verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$clustering$kmeans[[phase]]
  
  if(base::class(cluster_obj) != "kmeans_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(-phase)
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- hlpr_select(stat_df, variables_subset = variables_subset)
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    cluster_obj <- 
      confuns::initiate_kmeans_object(
        kmeans.data = stat_df, 
        key.name = "cell_id",
        default.method.kmeans = object@default$method_kmeans, 
        default.centers = object@default$k, 
        verbose = FALSE
        
      )
    
    msg <- glue::glue("Successfully initiated kmeans clustering for {phase} phase with variables: '{remaining_vars}'", 
                      remaining_vars = glue::glue_collapse(x = base::colnames(stat_df), sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
    
  } else {
    
    msg <- glue::glue("Kmeans clustering for {phase} phase already exists. Set argument 'force' to TRUE in order to overwrite it.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object@analysis$clustering$kmeans[[phase]] <- cluster_obj
  
  base::return(object)
  
}

#' @rdname initiateHierarchicalClustering
#' @export
initiatePamClustering <- function(object,
                                  phase = NULL, 
                                  variables_subset = NULL, 
                                  force = FALSE, 
                                  verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$clustering$pam[[phase]]
  
  if(base::class(cluster_obj) != "pam_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(-phase)
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- hlpr_select(stat_df, variables_subset = variables_subset)
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    cluster_obj <- 
      confuns::initiate_pam_object(
        pam.data = stat_df, 
        key.name = "cell_id",
        verbose = FALSE
      )
    
    msg <- glue::glue("Successfully initiated partitioning around medoids (PAM) clustering for {phase} phase with variables: '{remaining_vars}'", 
                      remaining_vars = glue::glue_collapse(x = base::colnames(stat_df), sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
    
  } else {
    
    msg <- glue::glue("Partitioning around medoids (PAM) clustering clustering for {phase} phase already exists. Set argument 'force' to TRUE in order to overwrite it.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object@analysis$clustering$pam[[phase]] <- cluster_obj
  
  base::return(object)
  
}