

#' @title Rename cluster of cells
#' 
#' @description Allows to rename groups within a cluster variable 
#' of the cluster data of the specified phase.
#'
#' @inherit argument_dummy params
#' @param cluster_variable Character value. The name of the cluster variable
#' whoose cluster are to be renamed.  
#' @param ... The clusters to be renamed specified according to the following
#' syntax: \emph{'new_cluster_name'} \code{=} \emph{'old_cluster_name'}.
#' 
#' @return An updated cto object.
#' @export
#'
renameCluster <- function(object, phase = NULL, cluster_variable = NULL, ...){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  confuns::check_one_of(
    input = cluster_variable, 
    against = getVariableNames(object, phase = phase, variable_classes = "cluster"), 
    ref.input = base::as.character(glue::glue("input for argument 'cluster_variable' (phase = {phase})"))
  )
  
  rename_input <- confuns::keep_named(c(...))
  
  if(base::length(rename_input) == 0){
    
    msg <- ct_warnings$how_to_name_input
    
    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop"
    )
    
  }
  
  cluster_df <- getClusterDf(object, phase = phase)
  
  valid_rename_input <-
    confuns::check_vector(
      input = base::unname(rename_input),
      against = base::levels(cluster_df[[cluster_variable]]),
      fdb.fn = "warning",
      ref.input = "groups to rename",
      ref.against = glue::glue("all groups of feature '{cluster_variable}'. ({ct_warnings$how_to_name_input})")
    )
  
  rename_input <- rename_input[rename_input %in% valid_rename_input]
  
  # rename cluster
  renamed_cluster_df <-
    dplyr::mutate(
      .data = cluster_df,
      {{cluster_variable}} := forcats::fct_recode(.f = !!rlang::sym(cluster_variable), !!!rename_input)
    )

  
  object@data$cluster[[phase]] <- renamed_cluster_df
  
  base::return(object)
  
}