# phase independent subsetting --------------------------------------------


#' @title Create data subset by cell ids
#' 
#' @description Subset functions allow to conveniently split your data by certain characteristics such 
#' as cell lines, conditions, cluster etc. or for specific cell ids. This might be useful if you want apply some machine learning 
#' techniques such as clustering and correlation on only a subset of cells. See details for more information.
#'
#' @inherit argument_dummy params
#' @param cell_ids Character vector. Denotes the cells to keep unambiguously with their cell ids.
#' @param new_name Character value. Denotes the name of the output object. If set to NULL the name of the 
#' input object taken and suffixed with \emph{'_subset'}.
#' 
#' @details Creating subsets of your data affects analysis results such as clustering and correlation which 
#' is why these results are reset in the subsetted object and must be computed again. To prevent inadvertent overwriting 
#' the default directory is reset as well. Make sure to set a new one via \code{setDefaultDirectory()}. 
#' 
#' The mechanism with which you create the subset is stored in the output object. Use \code{printSubsetHistory()}
#' to reconstruct the way from the original object to the current one. 
#' 
#' @return A celltracer object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCellId <- function(object, new_name, cell_ids, verbose = NULL, ...){
  
  check_object(object)
  assign_default(object)
  
  confuns::is_value(new_name, mode = "character", skip.allow = TRUE, skip.val = NULL)
  
  # extract info from ... (subsetByCellId() might be used by one of the other subset functions)
  subset_by <- list(...)[["subset_by"]]
  phase <- list(...)[["phase"]]
  
  if(!base::is.null(phase)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
  }
  
  confuns::give_feedback(
    msg = "Subsetting celltracer object by cell ID.", 
    verbose = verbose
  )

  if(multiplePhases(object)){
    
    phases <- getPhases(object)
    
    # subsetting tracks 
    object@cdata$tracks <- 
      purrr::map(.x = object@cdata$tracks,
                 .f = function(track_df){
                   
                   dplyr::filter(track_df, cell_id %in% {{cell_ids}})
                   
                 }) %>% 
      purrr::set_names(nm = phases)
    
    # subsetting stats
    object@cdata$stats <- 
      purrr::map(.x = object@cdata$stats, 
                 .f = function(stat_df){
                   
                   dplyr::filter(stat_df, cell_id %in% {{cell_ids}})
                   
                 })
    
    # subsetting meta 
    object@cdata$meta <- 
      purrr::map(.x = object@cdata$meta, 
                 .f = function(meta_df){
                   
                   dplyr::filter(meta_df, cell_id %in% {{cell_ids}}) %>% 
                     dplyr::mutate_if(.predicate = base::is.factor, .funs = base::droplevels)
                   
                 })
    
    # new cluster data 
    object@cdata$cluster <- 
      purrr::map(.x = object@cdata$cluster, 
                 .f = function(cluster_df){
                   
                   dplyr::filter(cluster_df, cell_id %in% {{cell_ids}}) %>% 
                     dplyr::select(cell_id)
                   
                 })
    
  } else {
    
    object@cdata <- 
      purrr::imap(.x = object@cdata, 
                  .f = function(df, slot){
                    
                    df <- 
                      dplyr::filter(df, cell_id %in% {{cell_ids}}) %>% 
                      dplyr::mutate_if(.predicate = base::is.factor, .funs = base::droplevels)
                    
                    if(slot == "cluster"){
                      
                      df <- dplyr::select(df, cell_id)
                      
                    }
                    
                    base::return(df)
                    
                  })
    
  }
  
  # reset analysis slot 
  object@analysis <- list()
  
  # subset well plate information (denote wells that are not longer in use as 'Discarded')
  wp_subset_info <- object@cdata$well_plate %>% select(well_plate_name, well)
  
  if(multiplePhases(object)){
    
    object@well_plates <- 
      purrr::imap(.x = object@well_plates, 
                  .f = function(wp_list, wp_name){
                    
                    keep_wells <-
                      dplyr::filter(wp_subset_info, well_plate_name == {{wp_name}}) %>% 
                      dplyr::pull(well) %>% 
                      base::levels()
                    
                    wp_list$wp_df_eval <- 
                      dplyr::mutate(dplyr::ungroup(wp_list$wp_df_eval), 
                                    condition = dplyr::case_when(well %in% {{keep_wells}} ~ condition, TRUE ~ "Discarded"), 
                                    cell_line = dplyr::case_when(well %in% {{keep_wells}} ~ cell_line, TRUE ~ "Discarded"),
                                    cl_condition = dplyr::case_when(well %in% {{keep_wells}} ~ cl_condition, TRUE ~ "Discarded"), 
                                    information_status = base::as.character(information_status),
                                    information_status = dplyr::case_when(well %in% {{keep_wells}} ~ information_status, TRUE ~ "Discarded"),
                                    information_status = base::factor(information_status, levels = c("Complete", "Incomplete", "Missing", "Discarded"))
                      )
                    
                    wp_list$wp_df_eval$condition_df <- 
                      purrr::map2(.x = wp_list$wp_df_eval$condition_df,
                                  .y = wp_list$wp_df_eval$well,
                                  .f = function(cdf, well){
                                                   
                                                   if(!well %in% keep_wells){
                                                     
                                                     cdf[1,] <- base::rep(NA, base::ncol(cdf))
                                                     
                                                   }
                                                   
                                                   base::return(cdf)
                                                   
                                                   
                                                 })
                    
                    base::return(wp_list)
                    
                    
                  })
    
    
  }
  
  # rename the object
  
  parent_name <- object@name
  
  if(base::is.null(new_name)){
    
    object@name <- stringr::str_c(object@name, "subset", sep = "_")
    
  } else if(base::is.character(new_name)){
    
    if(new_name == object@name){
      
      base::stop("Input for argument 'new_name' must not be identical with the objects name.")
      
    }
    
    object@name <- new_name
    
  }
  
  # save subset information, if not provided in ... subsetByCellId is the main subsetter
  # else its one of the other
  if(!confuns::is_list(input = subset_by)){
    
    subset_by <- list(by = "cell_id")
    
  }
  
  subset_by$ids_remaining = cell_ids
  subset_by$n_remaining <- nCells(object)
  
  subset_by$parent_object <- parent_name
  subset_by$new_object <- object@name
  
  if(multiplePhases(object)){
    
    subset_by$phase <- phase
    
  }
  
  # if first subset -> create subset info list 
  if(base::is.null(object@information$subset)){
    
    object@information$subset$first <- subset_by
    
  } else {
    
    # else add subset information and name accordingly
    n_subsets <- base::length(object@information$subset)
    
    slot_name <- english::ordinal(n_subsets + 1)
    
    object@information$subset[[slot_name]] <- subset_by
    
  }
  
  
  confuns::give_feedback(
    msg = glue::glue("New object name: {object@name}"), 
    verbose = TRUE
  )
  
  # reset default directory 
  object@information$directory_cto <- NULL
  
  confuns::give_feedback(
    msg = "Default directory has been reset. Make sure to set a new one via 'setDefaultDirectory()'",
    verbose = TRUE
  )
  
  # give feedback
  confuns::give_feedback(
    msg = glue::glue("A total of {nCells(object)} cells remain."), verbose = TRUE
  )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}

#' @title Create data subset by cell lines
#' 
#' @inherit subsetByCellId params description details
#' @param cell_lines Character vector. Denotes the cell lines to be kept.
#' 
#' @return A celltracer object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCellLine <- function(object, new_name, cell_lines, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  # check if input is valid
  confuns::check_one_of(
    input = cell_lines, 
    against = getCellLines(object)
  )
  
  # give feedback 
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by {ref_cell_line} '{cell_lines}'.", 
                     ref_cell_line = confuns::adapt_reference(cell_lines, "cell line", "cell lines"),
                     cell_lines = glue::glue_collapse(cell_lines, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  # extract cell ids
  cell_ids <-
    getGroupingDf(object, phase = 1, verbose = FALSE) %>% 
    dplyr::filter(cell_line %in% {{cell_lines}}) %>% 
    dplyr::pull(cell_id)
  
  # subset object
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   new_name = new_name,
                   verbose = FALSE,
                   subset_by = list(by = "cell_lines", cell_lines = cell_lines)
                   )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}

# -----

# phase dependent subsetting ----------------------------------------------

#' @title Create data subset by cluster
#' 
#' @inherit subsetByCellId params description details
#' @param cluster_variable Character value. Denotes cluster variable from which 
#' to subset the cells.
#' @param cluster Character vector. Denotes the exact cluster names carried by the
#' cluster variable specified with argument \code{cluster_variable} to be kept. 
#' 
#' @note In case of experiment set ups with multiple phases: 
#' 
#' As creating subsets of your data affects downstream analysis results you have to
#' manually specify the phase for which the clustering of interest has been calculated.
#' 
#' The output object contains data for all phases but only for those cells that matched
#' the input for argument \code{cluster} in the specified cluster variable during 
#' the specified phase.
#' 
#' @return A celltracer object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCluster <- function(object, new_name, cluster_variable, cluster, phase = NULL, verbose = NULL){
  
  check_object(object)
  check_phase_manually(object, phase = phase)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  # check if input is valid 
  confuns::is_value(cluster_variable, mode = "character")
  
  confuns::check_one_of(
    input = cluster_variable, 
    against = getClusterVariableNames(object, phase = phase)
  )
  
  cluster <- base::as.character(cluster)
  
  confuns::check_one_of(
    input = cluster, 
    against = getGroupNames(object, grouping_variable = cluster_variable, phase = phase)
  )
  
  # give feedback 
  if(multiplePhases(object)){
    
    ref_phase <- glue::glue(" of {phase} phase.")
    
  } else {
    
    ref_phase <- ""
    
  }
  
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by cluster '{cluster}' of cluster variable '{cluster_variable}'{ref_phase}.",
                     cluster = glue::glue_collapse(cluster, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  # extract cell ids
  cell_ids <-
    getGroupingDf(object, phase = phase, verbose = FALSE) %>% 
    dplyr::filter(!!rlang::sym(cluster_variable) %in% {{cluster}}) %>% 
    dplyr::pull(cell_id)
  
  # subset object
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phase,
                   new_name = new_name,
                   verbose = FALSE,
                   subset_by = list(by = "cluster", cluster_variable = cluster_variable, cluster = cluster)
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}

#' @title Create data subset by conditions
#' 
#' @inherit subsetByCellId params description details
#' @param conditions Character vector. Denotes the conditions to be kept.  
#' 
#' @note In case of experiment set ups with multiple phases: 
#' 
#' As creating subsets of your data affects downstream analysis results you have to
#' manually specify the phase you are referring to.
#' 
#' The output object contains data for all phases but only for those cells that matched
#' the input for argument \code{conditions} during the specified phase.
#' 
#' 
#' @return A celltracer object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCondition <- function(object, new_name, conditions, phase, verbose = NULL){
  
  check_object(object)
  check_phase_manually(object, phase = phase)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  # check if input is valid
  confuns::check_one_of(
    input = conditions, 
    against = getConditions(object, phase = phase)
  )
  
  # give feedback
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by {ref_conditions} '{conditions}'.", 
                     ref_conditions = confuns::adapt_reference(conditions, "condition", "conditions"),
                     conditions = glue::glue_collapse(conditions, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  # extract cell ids 
  cell_ids <-
    getGroupingDf(object, phase = phase, verbose = FALSE) %>% 
    dplyr::filter(condition %in% {{conditions}}) %>% 
    dplyr::pull(cell_id)
  
  # subset object
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phase,
                   new_name = new_name,
                   verbose = FALSE,
                   subset_by = list(by = "conditions", conditions = conditions)
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}

#' @title Create data subset by specified requirements
#' 
#' @inherit subsetByCellId params details description
#' @inherit dplyr::filter params
#' 
#' @details Creating subsets of your data affects analysis results such as clustering and correlation which 
#' is why these results are reset in the subsetted object and must be computed again. To prevent inadvertent overwriting 
#' the default directory is reset as well. Make sure to set a new one via \code{setDefaultDirectory()}. 
#' 
#' The mechanism with which you create the subset is stored in the output object. Use \code{printSubsetHistory()}
#' to reconstruct the way from the original object to the current one. 
#' 
#' The input for \code{...} must be supplied in the fashion of \code{dplyr::filter()}.
#' The expressions are applied to the stat data.frame (obtained via \code{getStatDf()}) and
#' must refer to the variables you obtain with \code{getStatVariableNames()}.
#' 
#' Cells that match all requirements are those that are kept in the returned celltracer object.
#' 
#' @note In case of experiment set ups with multiple phases: 
#' 
#' As creating subsets of your data affects downstream analysis results you have to
#' manually specify the phase you are referring to.
#' 
#' The output object contains data for all phases but only for those cells that matched
#' the input for argument \code{...} during the specified phase.
#' 
#' @seealso \code{dplyr::filter()}
#' 
#' @return A celltracer object that contains the data for the subsetted cells. 
#' @export
#'
subsetByFilter <- function(object, new_name, ..., phase = NULL, verbose = NULL){
  
  check_object(object)
  check_phase_manually(object, phase = phase)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  requirements <- rlang::enquos(...)
  
  # give feedback 
  n_reqs <- base::length(requirements)
  
  confuns::give_feedback(
    msg = glue::glue("Subsetting celltracer object by {n_reqs} filtering {ref_reqs}.", 
                     ref_reqs = confuns::adapt_reference(requirements, "requirement", "requirements")
    ), 
    verbose = TRUE
  )
  
  # extract cell ids 
  cell_ids <-
    getStatsDf(
      object = object,
      phase = phase,
      verbose = FALSE,
      with_cluster = FALSE,
      with_meta = FALSE,
      with_well_plate = FALSE
    ) %>% 
    dplyr::filter(...) %>% 
    dplyr::pull(cell_id)
  
  # subset object
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phase,
                   verbose = FALSE,
                   new_name = new_name,
                   subset_by = list(by = "filter", requirements = requirements)
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
}

# -----



