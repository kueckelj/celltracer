



# Initiation --------------------------------------------------------------

#' @title Set up clustering objects with cypro
#' 
#' @description These functions set up the necessary objects to perform clustering with the 
#' respective algorithm. See details for more. 
#'
#' @inherit argument_dummy
#' @param scale Logical value. If set to TRUE (the default) all variables will be scaled before clustering is 
#' performed to ensure that all variables are weighted equally.
#' @param variables_subset Character vector or NULL. Denotes the numeric variables the subsequent clustering 
#' steps will include which influences the clustering results. 
#' 
#' If set to NULL all of them are chosen. You can prefix variables you do NOT want to influence the clustering
#' with a \emph{'-'}. (Saves writing if there are more variables you are interested in
#' than variables you are not interested in.)
#' 
#' Use \code{getNumericVariableNames()} to obtain all valid input options.
#' 
#' @details The clustering initiation functions set up the S4 object that is used by cypro to do the clustering. 
#' Every downstream analysis function depends on the input you specify here. This means in particular the input for 
#' argument \code{scale} and the input for argument \code{variables_subset}. The latter denotes the variables on which 
#' the clustering will base on. Changing these might influence the clustering results. If you realize later on that 
#' you want to change the set up use the respective \code{initiate*Clustering()} function again and set argument \code{force}
#' to TRUE in order to overwrite the current set up. 
#' 
#' @seealso getHclustConv(), getKmeansConv(), getPamConv()
#'
#' @inherit update_object return 
#' @export
#'
initiateHierarchicalClustering <- function(object,
                                           variable_set,
                                           phase = NULL, 
                                           scale = TRUE,
                                           force = FALSE, 
                                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$hclust[[variable_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$hclust[[variable_set]]
    
  }
  
  variables <- getVariableSet(object, variable_set)
  
  if(base::class(cluster_object) != "hclust_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(cell_id, dplyr::all_of(variables))
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    cluster_object <- 
      confuns::initiate_hclust_object(
        hclust.data = stat_df, 
        key.name = "cell_id",
        scale = scale,
        default.aggl = object@default$method_aggl, 
        default.dist = object@default$method_dist, 
        verbose = FALSE
      )
    
    msg <- glue::glue("Successfully initiated hierarchical clustering{ref_phase}with '{variable_set}'-variables: '{variables}'", 
                      variables = glue::glue_collapse(x = variables, sep = "', '", last = "' and '", width = 100), 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
    
  } else {
    
    msg <- glue::glue("Hierarchical clustering{ref_phase}with variable set '{variable_set}' already exists. Set argument 'force' to TRUE in order to overwrite it.", 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "hclust", 
                           phase = phase, 
                           variable_set = variable_set)
  
  base::return(object)
  
}

#' @rdname initiateHierarchicalClustering
#' @export
initiateKmeansClustering <- function(object,
                                     variable_set,
                                     phase = NULL, 
                                     scale = TRUE, 
                                     force = FALSE,
                                     verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$kmeans[[variable_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$kmeans[[variable_set]]
    
  }
  
  variables <- getVariableSet(object, variable_set = variable_set)
  
  if(base::class(cluster_object) != "kmeans_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(cell_id, dplyr::all_of(x = variables))
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    cluster_object <- 
      confuns::initiate_kmeans_object(
        kmeans.data = stat_df, 
        key.name = "cell_id",
        scale = scale, 
        default.method.kmeans = object@default$method_kmeans, 
        default.centers = 2, 
        verbose = FALSE
      )
    
    msg <- glue::glue("Successfully initiated kmeans clustering{ref_phase}with '{variable_set}'-variables: '{variables}'", 
                      variables = glue::glue_collapse(x = variables, sep = "', '", last = "' and '", width = 100), 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
    
  } else {
    
    msg <-
      glue::glue("Kmeans clustering{ref_phase}with variable set '{variable_set}' already exists. Set argument 'force' to TRUE in order to overwrite it.", 
                 ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "kmeans", 
                           phase = phase, 
                           variable_set = variable_set)  
  base::return(object)
  
}

#' @rdname initiateHierarchicalClustering
#' @export
initiatePamClustering <- function(object,
                                  variable_set,
                                  phase = NULL, 
                                  scale = TRUE, 
                                  force = FALSE, 
                                  verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$pam[[variable_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$pam[[variable_set]]
    
  }
  
  variables <- getVariableSet(object, variable_set)
  
  if(base::class(cluster_object) != "pam_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(cell_id, dplyr::all_of(variables))
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    cluster_object <- 
      confuns::initiate_pam_object(
        pam.data = stat_df, 
        scale = scale, 
        key.name = "cell_id",
        verbose = FALSE
      )
    
    msg <- glue::glue("Successfully initiated partitioning around medoids (PAM) clustering{ref_phase}with '{variable_set}'-variables: '{variables}'", 
                      variables = glue::glue_collapse(x = variables, sep = "', '", last = "' and '", width = 100), 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
  } else {
    
    msg <- glue::glue("Partitioning around medoids (PAM) clustering clustering{ref_phase}with variable set '{variable_set}' already exists. Set argument 'force' to TRUE in order to overwrite it.", 
                      ref_phase = hlpr_glue_phase(object, phase))
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object <- setClusterConv(object = object, 
                           cluster_object = cluster_object, 
                           method = "pam", 
                           phase = phase, 
                           variable_set = variable_set)
  
  base::return(object)
  
}