


#' Title
#'
#' @param object 
#' @param phase 
#' @param variables_subset 
#' @param force 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#'
initiatePamClustering <- function(object,
                                  phase = NULL, 
                                  variables_subset = NULL, 
                                  force = FALSE, 
                                  verbose = NULL, 
                                  ...){
  
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
        key.name = "cell_id"#, verbose = FALSE
        
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


#' Title
#'
#' @param object 
#' @param phase 
#' @param method_pam 
#' @param k 
#' @param verbose 
#'
#' @return
#' @export
#'
computePamCluster <- function(object,
                              phase = NULL, 
                              method_pam = NULL, 
                              k = NULL, 
                              verbose = NULL){
  
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
      verbose.pb = FALSE
    )
  
  object@analysis$clustering$pam[[phase]] <- cluster_obj
  
  base::return(object)
  
}



#' Title
#'
#' @param object 
#' @param phase 
#' @param method_pam 
#' @param k 
#' @param verbose 
#'
#' @return
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







# get ---------------------------------------------------------------------


#' Title
#'
#' @param object 
#' @param phase 
#'
#' @return
#' @export
#'
getPamConv <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$clustering$pam[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "pam_conv",
    phase = phase, 
    ref_input = "partitioning around medoids (PAM) clustering object", 
    ref_fun = "initiatePamClustering()"
  )
  
  base::return(cluster_obj)
  
}







# plotting ----------------------------------------------------------------

#' Title
#'
#' @param object 
#' @param k 
#' @param phase 
#' @param clr 
#' @param display_cols 
#' @param display_line 
#' @param display_points 
#'
#' @return
#' @export
#'
plotAvgSilhouetteWidths <- function(object,
                             k = NULL, 
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
    k = k, 
    display.cols = display_cols, 
    display.line = display_line, 
    display.points = display_points
  )
  
}



#' Title
#'
#' @param object 
#' @param k 
#' @param method_pam 
#' @param clrp 
#' @param ncol 
#' @param nrow 
#' @param verbose 
#'
#' @return
#' @export

plotSilhouetteWidths <- function(object, 
                                 k = NULL, 
                                 method_pam = NULL, 
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




#' Title
#'
#' @param object 
#' @param phase 
#' @param k 
#' @param method_pam 
#' @param clrp 
#' @param verbose 
#'
#' @return
#' @export
#'
plotPamMedoids <- function(object, 
                           phase = NULL, 
                           k = NULL, 
                           method_pam = NULL, 
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


