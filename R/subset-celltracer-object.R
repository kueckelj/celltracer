

#' @title Create subsets of your experiment
#' 
#' @description These function allow to conveniently split your data by certain characteristics such 
#' as cell lines, conditions, cluster etc. This might be useful if you want apply some machine learning 
#' techniques such as clustering and correlation on only a subset of cells. 
#'
#' @inherit argument_dummy params
#' @param cell_ids Character vector. Denotes the cells to keep unambiguously with their cell ids.
#' @param conditions Character vector. Denotes the conditions to be kept.
#' @param cell_lines Character vector. Denotes the cell lines to be kept. 
#' @param cluster_variable Character value. Denotes the clustering variable to be used in order to subset
#' the celltracer object. 
#' @param cluster Character vector. Denotes the cluster of the clustering variable specified in argument
#' \code{cluster_variable} to be kept. 
#' 
#' @details The valid input options for argument \code{phase} vary As clustering is computed for every 
#' phase respectively and a cell's condition might differ from phase to phase the functions \code{subsetByCondition()} and 
#' \code{subsetByCluster()} need a character value as input for argument \code{phase} which filters the 
#' data by the respective input as well as by the specified phase. Functions \code{subsetByCellId()} and \code{subsetByCellLine()} allow to create subsets over more than 
#' just one phase. 
#' 
#' Correlation, clustering and dimensional reduction results must be computed again as the data set changes.
#' 
#' @return A celltracer object that contains the data for the subsetted cells over the specified phase(s). 
#' @export
#'
subsetByCellId <- function(object, cell_ids, phase = "all", verbose = NULL, ...){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    if(base::all(phase %in% "all")){
      
      phases <- getPhases(object)
      
    } else {
      
      phases <- check_phase(object, phase = phase)
      
    }
    
    confuns::give_feedback(
      msg = glue::glue("Subsetting celltracer object by cell ID over {ref_phase} phase.", 
                       ref_phase = glue::glue_collapse(x = phases, sep = ",", last = " and ")), 
      verbose = verbose
    )
    
  }
  

  if(multiplePhases(object)){
    
    # subsetting tracks 
    object@cdata$tracks <- 
      purrr::map(.x = object@data$tracks[phases],
                 .f = function(track_df){
                   
                   dplyr::filter(track_df, cell_id %in% {{cell_ids}})
                   
                 }) %>% 
      purrr::set_names(nm = phases)
    
    # subsetting stats
    object@cdata$stats <- 
      purrr::map(.x = object@data$stats[phases], 
                 .f = function(stat_df){
                   
                   dplyr::filter(stat_df, cell_id %in% {{cell_ids}})
                   
                 })
    
    # subsetting meta 
    object@cdata$meta <- 
      purrr::map(.x = object@data$meta[phases], 
                 .f = function(meta_df){
                   
                   dplyr::filter(meta_df, cell_id %in% {{cell_ids}})
                   
                 })
    
    # new cluster data 
    object@cdata$cluster <- 
      purrr::map(.x = object@data$cluster[phases], 
                 .f = function(cluster_df){
                   
                   dplyr::filter(cluster_df, cell_id %in% {{cell_ids}}) %>% 
                     dplyr::select(cell_id)
                   
                 })
    
  } else {
    
    object@cdata <- 
      purrr::imap(.x = object@cdata, 
                  .f = function(df, slot){
                    
                    df <- dplyr::filter(df, cell_id %in% {{cell_ids}})
                    
                    if(slot == "cluster"){
                      
                      df <- dplyr::select(df, cell_id)
                      
                    }
                    
                    base::return(df)
                    
                  })
    
  }
  
  empty_named_list <- 
    base::vector(mode = "list", length = base::length(phases)) %>% 
    purrr::set_names(nm = phases)
  
  object@analysis <- list()
  
  wp_subset_info <- object@data$meta[[1]] %>% select(well_plate_name, well)
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    phases_subset <- base::which(all_phases %in% phases)
    
    object@well_plates <- 
      purrr::imap(.x = object@well_plates, 
                  .f = function(wp_list, wp_name){
                    
                    keep_wells <-
                      dplyr::filter(wp_subset_info, well_plate_name == {{wp_name}}) %>% 
                      dplyr::pull(well) %>% 
                      base::unique()
                    
                    wp_list$wp_df_eval <- 
                      dplyr::mutate(dplyr::ungroup(wp_list$wp_df_eval), 
                                    condition = dplyr::case_when(well %in% {{keep_wells}} ~ condition, TRUE ~ "Discarded"), 
                                    cell_line = dplyr::case_when(well %in% {{keep_wells}} ~ cell_line, TRUE ~ "Discarded"),
                                    cl_condition = dplyr::case_when(well %in% {{keep_wells}} ~ cl_condition, TRUE ~ "Discarded"), 
                                    information_status = base::as.character(information_status),
                                    information_status = dplyr::case_when(well %in% {{keep_wells}} ~ information_status, TRUE ~ "Discarded"),
                                    information_status = base::factor(information_status, levels = c("Complete", "Incomplete", "Missing", "Discarded")),
                                    condition_df = purrr::map2(.x = condition_df, .y = well, 
                                                               .f = function(cdf, well){
                                                                 
                                                                 if(!well %in% keep_wells){
                                                                   
                                                                   cdf[1,] <- base::rep(NA, base::ncol(cdf))
                                                                   
                                                                 }
                                                                 
                                                                 cdf <- cdf[, phases_subset]
                                                                 
                                                                 base::return(cdf)
                                                                 
                                                                 
                                                               })
                      )
                    
                    base::return(wp_list)
                    
                    
                  })
    
    
  }
  
  # subset information
  subset_by <- list(...)[["subset_by"]]
  
  if(!confuns::is_list(input = subset_by)){
    
    subset_by <- list(by = "cell_id")
    
  }
  
  if(multiplePhases(object)){
    
    if(!base::all(getPhases(object) %in% phases)){
      
      subset_by$phases <- phases
      
      object@set_up$phases <- object@set_up$phases[phases]
      
    } else {
      
      subset_by$phases <- "all kept"
      
    }
    
  }
  
  object@information$subset <- subset_by
  
  object@default$directory_cto <- NULL
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}

#' @rdname subsetByCellId
#' @export
subsetByCellLine <- function(object, cell_lines, phase = "all", verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::all(phase %in% "all")){
    
    phases <- getPhases(object)
    
  } else {
    
    phases <- check_phase(object, phase = phase)
    
  }
  
  confuns::check_one_of(
    input = cell_lines, 
    against = getCellLines(object)
  )
  
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by {ref_cell_line} '{cell_lines}'..", 
                     ref_cell_line = confuns::adapt_reference(cell_lines, "cell line", "cell lines"),
                     cell_lines = glue::glue_collapse(cell_lines, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  cell_ids <-
    getGroupingDf(object, phase = 1) %>% 
    dplyr::filter(cell_line %in% {{cell_lines}}) %>% 
    dplyr::pull(cell_id)
  
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phases,
                   verbose = FALSE,
                   subset_by = list(by = "cell_lines", cell_lines = cell_lines)
                   )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}

#' @rdname subsetByCellId
#' @export
subsetByCondition <- function(object, conditions, phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = conditions, 
    against = getConditions(object)
  )
  
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by {ref_conditions} '{conditions}'.", 
                     ref_phase = glue::glue_collapse(x = phases, sep = ",", last = " and "), 
                     ref_conditions = confuns::adapt_reference(conditions, "condition", "conditions"),
                     conditions = glue::glue_collapse(conditions, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  cell_ids <-
    getGroupingDf(object, phase = phase) %>% 
    dplyr::filter(condition %in% {{conditions}}) %>% 
    dplyr::pull(cell_id)
  
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phases,
                   verbose = FALSE,
                   subset_by = list(by = "conditions", conditions = conditions)
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}

#' @rdname subsetByCellId
#' @export
subsetByCluster <- function(object, cluster_variable, cluster, phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = cluster_variable, 
    against = getClusterVariableNames(object, phase = phase)
  )
  
  cluster <- base::as.character(cluster)
  
  confuns::check_one_of(
    input = cluster, 
    against = getGroupNames(object, option = cluster_variables, phase = phase)
  )
  
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by cluster '{cluster}' of cluster variable '{cluster_variable}'.",
                     cluster = glue::glue_collapse(cluster, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  cell_ids <-
    getGroupingDf(object, phase = phase) %>% 
    dplyr::filter(!!rlang::sym(cluster_variable) %in% {{cluster}}) %>% 
    dplyr::pull(cell_id)
  
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phases,
                   verbose = FALSE,
                   subset_by = list(by = "cluster", cluster_variable = cluster_variable, cluster = cluster)
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}



