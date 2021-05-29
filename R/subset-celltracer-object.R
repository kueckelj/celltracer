# phase independent subsetting --------------------------------------------


#' @title Create data subset by cell ids
#' 
#' @description Subset functions allow to conveniently split your data by certain characteristics such 
#' as cell lines, conditions, cluster etc. or for specific cell ids. This might be useful if you want apply some machine learning 
#' algorithms such as clustering and correlation on only a subset of cells. See details for more information.
#'
#' @inherit argument_dummy params
#' @param cell_ids Character vector. Denotes the cells to keep unambiguously with their cell ids.
#' @param new_name Character value. Denotes the name of the output object. If set to NULL the name of the 
#' input object is taken and suffixed with \emph{'_subset'}.
#' @param reasoning Character value. Allows two write a short description of how the cell ids according 
#' to which the object is filtered were selected. This description is included in the output of \code{printSubsetHistory()}.
#' Ignored if set to NULL. 
#' 
#' @details Creating subsets of your data affects analysis results such as clustering and correlation which 
#' is why these results are reset in the subsetted object and must be computed again. To prevent inadvertent overwriting 
#' the default directory is reset as well. Make sure to set a new one via \code{setDefaultDirectory()}. 
#' 
#' The mechanism with which you create the subset is stored in the output object. Use \code{printSubsetHistory()}
#' to reconstruct the way from the original object to the current one. 
#' 
#' @return A cypro object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCellId <- function(object, new_name, cell_ids, reasoning = NULL, verbose = NULL, ...){
  
  check_object(object)
  assign_default(object)
  
  confuns::are_vectors("cell_ids", mode = "character")
  
  confuns::are_values("new_name", "reasoning", mode = "character", skip.allow = TRUE, skip.val = NULL)
  
  # extract info from ... (subsetByCellId() might be used by one of the other subset functions)
  subset_by <- list(...)[["subset_by"]]
  phase <- list(...)[["phase"]]
  
  if(!base::is.null(phase)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
  }
  
  confuns::give_feedback(
    msg = "Subsetting cypro object by cell ID.", 
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
  
  subset_by$reasoning <- reasoning
  
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
#' @return A cypro object that contains the data for the subsetted cells. 
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
    msg = glue::glue("Subsetting cypro object by {ref_cell_line} '{cell_lines}'.", 
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
#' @param cluster_variable,grouping_variable Character value. Denotes variable from which 
#' to subset the cells.
#' @param cluster,groups Character vector. Denotes the exact cluster/group names carried by the
#' variable specified with argument \code{cluster_variable}/\code{grouping_variable} to be kept. 
#' 
#' @note In case of experiment set ups with multiple phases: 
#' 
#' As creating subsets of your data affects downstream analysis results you have to
#' manually specify the phase for which the grouping of interest has been calculated.
#' 
#' The output object contains data for all phases but only for those cells that matched
#' the input for argument \code{cluster}/\code{groups} in the specified variable during 
#' the specified phase.
#' 
#' @return A cypro object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCluster <- function(object,
                            new_name,
                            cluster_variable,
                            cluster,
                            phase = NULL,
                            verbose = NULL){
  
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
    msg = glue::glue("Subsetting cypro object by cluster '{cluster}' of cluster variable '{cluster_variable}'{ref_phase}.",
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

#' @rdname subsetByCluster
#' @export
subsetByGroup <- function(object,
                          new_name,
                          grouping_variable,
                          groups,
                          phase = NULL,
                          verbose = NULL){
  
  check_object(object)
  check_phase_manually(object, phase = phase)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  # check if input is valid 
  confuns::is_value(grouping_variable, mode = "character")
  
  confuns::check_one_of(
    input = grouping_variable, 
    against = getGroupingVariableNames(object, phase = phase)
  )
  
  groups <- base::as.character(groups)
  
  confuns::check_one_of(
    input = groups, 
    against = getGroupNames(object, grouping_variable = grouping_variable, phase = phase)
  )
  
  # give feedback 
  if(multiplePhases(object)){
    
    ref_phase <- glue::glue(" of {phase} phase.")
    
  } else {
    
    ref_phase <- ""
    
  }
  
  confuns::give_feedback(
    msg = glue::glue("Subsetting cypro object by {ref_group} '{groups}' of grouping variable '{grouping_variable}'{ref_phase}.",
                     ref_group = confuns::adapt_reference(groups, "group", "groups"),
                     groups = glue::glue_collapse(groups, sep = "', '", last = "' and '")), 
    verbose = verbose
  )
  
  # extract cell ids
  cell_ids <-
    getGroupingDf(object, phase = phase, verbose = FALSE) %>% 
    dplyr::filter(!!rlang::sym(grouping_variable) %in% {{groups}}) %>% 
    dplyr::pull(cell_id)
  
  # subset object
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phase,
                   new_name = new_name,
                   verbose = FALSE,
                   subset_by = list(by = "group", grouping_variable = grouping_variable, groups = groups)
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
#' @return A cypro object that contains the data for the subsetted cells. 
#' @export
#'
subsetByCondition <- function(object, new_name, conditions, phase = NULL, verbose = NULL){
  
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
    msg = glue::glue("Subsetting cypro object by {ref_conditions} '{conditions}'.", 
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
#' Cells that match all requirements are those that are kept in the returned cypro object.
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
#' @return A cypro object that contains the data for the subsetted cells. 
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
    msg = glue::glue("Subsetting cypro object by {n_reqs} filtering {ref_reqs}.", 
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







#' @title Create data subset by reducing the number of cells
#'
#' @description Subset functions allow to conveniently split your data. \code{subsetByNumber()} does not subset by anything 
#' specific but simply reduces the number of cells in the object by random selection across specified grouping variables. 
#' This might be useful if the number of cells is to high for certain machine learning algorithms such as clustering and correlation.
#' See details for more information.
#'
#' @inherit subsetByGroup params details
#' @param across Character vector. The grouping variables across which to reduce the cell number. This ensures that 
#' the randomly selected cells are equally distributed across certain groups. Defaults to \emph{'cell_line'} and \emph{'condition'}.
#' @param n_by_group Numeric value or NA If numeric, denotes the number of cells that is randomly selected from 
#' every group. 
#' @param n_total Numeric value or NA If numeric, denotes the final number of cells that the subsetted object is supposed 
#' to contain. The number of cells that is randomly selected by group is calculated accordingly. 
#' 
#' @details Creating subsets of your data affects analysis results such as clustering and correlation which 
#' is why these results are reset in the subsetted object and must be computed again. To prevent inadvertent overwriting 
#' the default directory is reset as well. Make sure to set a new one via \code{setDefaultDirectory()}. 
#' 
#' The mechanism with which you create the subset is stored in the output object. Use \code{printSubsetHistory()}
#' to reconstruct the way from the original object to the current one. 
#' 
#' \code{subsetByNumber()} first unites all grouping variables across which the number of cells is supposed to be reduced to one single 
#' new variable. Cell IDs are then grouped by this variable via \code{dplyr::group_by()}. The number of cell IDs is then reduced 
#' via \code{dplyr::slice_sample()}. The exact number of remaining cells can be specified in two different ways by using either argument
#' \code{n_by_group} or \code{n_total}:
#' 
#' If specified with \code{n_by_group()}: The numeric value is given to argument \code{n} of \code{dplyr::slice_sample()}. E.g. 
#' \code{across} = \emph{'condition'} and \code{n_by_group} = 1000, if the cypro object contains 6 different 
#' conditions the returned object contains 6000 randomly selectd cells - 1000 of each condition. 
#' 
#' If specified with \code{n_total}: The numeric value given to argument \code{n} of \code{dplyr::slice_sample()} is calculated 
#' like this: 
#' 
#'  n = \code{n_total} / number of groups
#'  
#' E.g \code{across} = \emph{'condition'} and \code{n_total} = 10.000, if the cypro object contains 
#' 4 different conditions 2500 cells of each condition will be in the returned object. 
#' 
#' @note In case of experiment set ups with multiple phases: 
#' 
#' As creating subsets of your data affects downstream analysis results you have to
#' manually specify the phase you are referring to.
#' 
#' The output object contains data for all phases but only for those cells that resulted 
#' from the random selection.
#'
#' @return
#' @export
#'
subsetByNumber <- function(object, new_name, across = c("cell_line", "condition"), n_by_group = NA, n_total = NA, phase = NULL, verbose = NULL){
  
  check_object(object)
  check_phase_manually(object, phase = phase)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = across, 
    against = getGroupingVariableNames(object, phase = phase, verbose = FALSE)
  )
  
  confuns::are_values("n_by_group", "n_total", mode = "numeric", skip.allow = TRUE, skip.val = NA)
  
  # extract cell ids 
  combined_name <- glue::glue_collapse(x = across, sep = "_")
  
  cell_ids_df <-
    getGroupingDf(object, phase = phase, verbose = FALSE) %>% 
    tidyr::unite(col = {{combined_name}}, dplyr::all_of(across))
  
  if(!base::is.numeric(n_by_group) & !base::is.numeric(n_total)){
    
    base::stop("Please specify either 'n_by_group' or 'n_total' with a numeric value.")
    
  } else if(base::is.numeric(n_by_group) & base::is.numeric(n_total)){
    
    base::stop("Please specify only one of 'n_by_group' and 'n_total'.")
    
  } else if(base::is.numeric(n_by_group)){
    
    ref_arg <- "n_by_group"
    
    cell_ids <- 
      dplyr::group_by(cell_ids_df, !!rlang::sym(combined_name)) %>% 
      dplyr::slice_sample(n = n_by_group) %>% 
      dplyr::pull(cell_id)
    
  } else if(base::is.numeric(n_total)){
    
    ref_arg <- "n_total"
    
    n_groups <- 
      dplyr::pull(cell_ids_df, var = {{combined_name}}) %>% 
      dplyr::n_distinct()
    
    n_by_group <- base::round(n_total/n_groups, digits = 0)
    
    cell_ids <- 
      dplyr::group_by(cell_ids_df, !!rlang::sym(combined_name)) %>% 
      dplyr::slice_sample(n = n_by_group) %>% 
      dplyr::pull(cell_id)
    
  }
  
  # give feedback 
  confuns::give_feedback(
    msg = glue::glue("Subsetting cypro object across '{across}' with argument '{ref_arg}' = {n}.", 
                     across = glue::glue_collapse(across, sep = "', '", last = "' and '"), 
                     n = base::ifelse(ref_arg == "n_total", n_total, n_by_group)), 
    verbose = TRUE
  )
  
  # subset object
  object_new <-
    subsetByCellId(object,
                   cell_ids = cell_ids,
                   phase = phase,
                   verbose = FALSE,
                   new_name = new_name,
                   subset_by = list(by = "number", across = across, n_type = ref_arg, n_val = n_by_group)
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object_new)
  
  
}






# -----



