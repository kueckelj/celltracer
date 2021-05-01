


#' Title
#'
#' @param object 
#' @param force 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#'
initiateHierarchicalClustering <- function(object,
                                           phase = NULL, 
                                           variables_subset = NULL, 
                                           force = FALSE, 
                                           verbose = NULL, 
                                           ...){
  
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
        default.dist = object@default$method_dist
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

#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dist 
#'
#' @return
#' @export
#'
computeDistanceMatrices <- function(object,
                                    phase = NULL,
                                    method_dist = NULL,
                                    force = FALSE,
                                    p = 2,
                                    verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_obj <- object@analysis$clustering$hclust[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "hclust_conv",
    phase = phase, 
    ref_input = "hierarchical clustering object", 
    ref_fun = "initiateHierarchicalClustering()"
  )
  
  cluster_obj <- 
    confuns::compute_distance_matrices(
      hcl.obj = cluster_obj, 
      methods.dist = method_dist, 
      p = p, 
      force = force, 
      verbose = verbose
    )
  
  object@analysis$clustering$hclust[[phase]] <- cluster_obj
  
  base::return(object)
  
}


#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dist 
#'
#' @return
#' @export
#'
agglomerateHierarchicalCluster <- function(object, phase = NULL, method_dist = NULL, method_aggl = NULL, force = FALSE, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_obj <- object@analysis$clustering$hclust[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "hclust_conv",
    phase = phase, 
    ref_input = "hierarchical clustering object", 
    ref_fun = "initiateHierarchicalClustering()"
  )
  
  cluster_obj <-
    confuns::compute_hierarchical_cluster(
      hcl.obj = cluster_obj, 
      methods.aggl = method_aggl, 
      methods.dist = method_dist, 
      verbose = verbose
    )
  
  object@analysis$clustering$hclust[[phase]] <- cluster_obj
  
  base::return(object)
  
}



#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dist 
#' @param method_aggl 
#' @param k 
#' @param h 
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




# get-functions -----------------------------------------------------------

#' Title
#'
#' @param object 
#' @param phase 
#'
#' @return
#' @export
#'
getHclustConv <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_obj <- object@analysis$clustering$hclust[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "hclust_conv",
    phase = phase, 
    ref_input = "hierarchical clustering object", 
    ref_fun = "initiateHierarchicalClustering()"
  )
  
  base::return(cluster_obj)
  
}


#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dist 
#' @param method_aggl 
#'
#' @return
#' @export
#'
getHclustObj <- function(object, phase = NULL, method_dist = NULL, method_aggl = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_obj <- getHclustConv(object, phase = phase)
  
  hclust_obj <- cluster_obj@hclust_results[[method_dist]][[method_aggl]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "hclust_conv",
    phase = phase, 
    ref_input = glue::glue("object hclust of distance method '{method_dist}' and agglomerative method '{method_aggl}'"), 
    ref_fun = "agglomerateHierarchicalCluster()"
  )
  
  base::return(hclust_obj)
  
}




# miscellaneous -----------------------------------------------------------


#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dist 
#' @param verbose 
#'
#' @return
#' @export
#'
discardDistanceMatrix <- function(object, phase = NULL, method_dist = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  confuns::is_value(method_dist, mode = "character")
  
  cluster_obj <- getHclustConv(object, phase = phase)

  check_availability(
    evaluate = !base::is.null(cluster_obj@dist_matrices[[method_dist]]), 
    phase = phase, 
    ref_input = glue::glue("distance matrix (method = {method_dist})"), 
    ref_fun = "computeDistanceMatrices()"
  )
  
  cluster_obj@dist_matrices[[method_dist]] <- NULL
  
  object@analysis$clustering$hclust[[phase]] <- cluster_obj
  
  confuns::give_feedback(msg = "Discarded.", verbose = verbose)

  base::return(object)    
  
}



# plotting ----------------------------------------------------------------



#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dist 
#' @param method_aggl 
#' @param k 
#' @param h 
#' @param branch_size 
#' @param display_legend 
#' @param display_title 
#' @param clrp 
#' @param clrp_adjust 
#' @param ncol 
#' @param nrow 
#' @param ... 
#'
#' @return
#' @export
#'
plotDendrogram <- function(object, 
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
                           ...
                           ){

  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_obj <- getHclustConv(object, phase = phase)  
  
  if(base::length(method_dist) == 1 & base::length(method_aggl) == 1){
    
    confuns::plot_dendrogram(
      hcl.obj = cluster_obj, 
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
    
    confuns::plot_dendrograms(
      hcl.obj = cluster_obj, 
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
  
  
}




