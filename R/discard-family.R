


#' @title Discard unwanted variables from your data
#' 
#' @description Discards variables from the provided object. They can not be restored apart
#' from adding them again artificially. Use with caution.
#'
#' @inherit argument_dummy 
#' @param cluster_variables,meta_variables Character vector. The variables you want to discard.
#'
#' @inherit updated_object return
#' @export
#'
discardClusterVariables <- function(object, cluster_variables, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = cluster_variables, 
    against = getClusterVariableNames(object, phase = phase)
  )
  
  cluster_df <-
    getClusterDf(object, phase = phase) %>% 
    dplyr::select(-dplyr::all_of(cluster_variables))
  
  object <- setCellDf(object, slot = "cluster", df = cluster_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname discardClusterVariables
#' @export
discardMetaVariables <- function(object, meta_variables, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = meta_variables, 
    against = getMetaVariableNames(object, phase = phase)
  )
  
  meta_df <-
    getMetaDf(object, phase = phase) %>% 
    dplyr::select(-dplyr::all_of(meta_variables))
  
  object <- setCellDf(object, slot = "meta", df = meta_df, phase = phase)
  
  base::return(object)
  
}



#' @title Discard unwanted variables from your data
#'
#' @inherit argument_dummy 
#' @param cluster_variables Character vector. The cluster variables you want to discard.
#' @param stat_variables Character vector. The variables from the cell statistics data you want to discard. 
#'
#' @inherit updated_object return
#' @export
#'
discardStatVariables <- function(object, stat_variables, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = stat_variables, 
    against = getStatVariableNames(object, phase = phase)
  )
  
  stat_df <- 
    getStatsDf(object, phase = phase, with_cluster = FALSE, with_meta = FALSE) %>% 
    dplyr::select(-dplyr::all_of(x = stat_variables))
  
  object <- setCellDf(object, slot = "stats", df = stat_df, phase = phase)
  
  object@vdata$total <- 
    object@vdata$total%>% dplyr::filter(!variable %in% {{stat_variables}})
  
  if(multiplePhases(object)){
    
    object@vdata$by_phase <- 
      purrr::map(.x = object@vdata$by_phase, 
                 .f = ~ dplyr::filter(.x, !variable %in% {{stat_variables}}))
    
  }
  
  base::return(object)
  
}



