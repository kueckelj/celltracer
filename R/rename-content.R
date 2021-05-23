#' @title Rename groups
#' 
#' @description Allows to rename particular groups within a grouping variable
#' of the celltracer object's cell data.
#'
#' @inherit argument_dummy params
#' @param grouping_variable Character value. The name of the grouping variable
#' whose groups you want to rename.  
#' @param ... The groups to be renamed specified according to the following
#' syntax: \emph{'new_cluster_name'} \code{=} \emph{'old_cluster_name'}.
#' 
#' @details Renaming groups of variables \emph{well_plate_name, well_plate_index, well}
#' and \emph{well_image} is not allowed and will result in an error. 
#' 
#' Use \code{getGroupNames()} to check if renaming resulted in the desired output.
#' 
#' @return An updated celltracer object.
#' @export
#'
renameGroups <- function(object, grouping_variable = NULL, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  all_grouping_vars <- 
    getGroupingVariableNames(object, named = TRUE, phase = phase)
  
  valid_grouping_vars <- 
    all_grouping_vars[!all_grouping_vars %in% well_plate_vars]
  
  confuns::check_one_of(
    input = grouping_variable, 
    against = valid_grouping_vars
  )
  
  grouping_var <- valid_grouping_vars[valid_grouping_vars == grouping_variable]
  
  slot <- base::names(grouping_var)
  
  rename_input <- confuns::keep_named(c(...))
  
  if(base::length(rename_input) == 0){
    
    msg <- ct_warnings$how_to_name_input
    
    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop"
    )
    
  }
  
  df <- getCellDf(object, slot = slot, phase = phase)
  
  valid_rename_input <-
    confuns::check_vector(
      input = base::unname(rename_input),
      against = base::levels(df[[grouping_variable]]),
      fdb.fn = "warning",
      ref.input = "groups to rename",
      ref.against = glue::glue("all groups of variable '{grouping_variable}'. ({ct_warnings$how_to_name_input})")
    )
  
  rename_input <- rename_input[rename_input %in% valid_rename_input]
  
  # rename cluster
  renamed_df <-
    dplyr::mutate(
      .data = df,
      {{grouping_variable}} := forcats::fct_recode(.f = !!rlang::sym(grouping_variable), !!!rename_input)
    )
  
  object <- setCellDf(object, slot = slot, df = renamed_df, phase = phase)
  
  base::return(object)
  
}



#' @title Rename meta variables
#' 
#' @description Implementations of the functions \code{dplyr::rename()} and 
#' \code{dplyr::rename_with()} that can be used to rename the variables of the cell meta 
#' data.frame. 
#'
#' @inherit argument_dummy params
#' @inherit dplyr::rename params
#'
#' @details The variables \emph{cell_id, cell_line, condition,
#' well_plate_name, well_plate_index, well} and \emph{well_image} are removed 
#' prior to the renaming as they are protected. The renaming process must not
#' result in columns that carry one of these names.
#' 
#' Use \code{getGroupingVariableNames()} to check if renaming resulted in 
#' the desired output.
#' 
#' @seealso \code{dplyr::rename()}, \code{dplyr::rename_with()}
#' 
#' @return
#' @export
#'

renameMetaDf <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(cell_id, cell_line, condition)
  
  rename_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(-cell_line, -condition, -cell_id) %>% 
    dplyr::rename(...)
  
  check_renamed_variables(base::colnames(rename_df))
  
  final_df <- base::cbind(core_df, rename_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "cluster", df = final_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname renameMetaDf
#' @export
renameMetaDfWith <- function(object, .fn, .cols = dplyr::everything(), ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(cell_id, cell_line, condition)
  
  rename_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(-cell_line, -condition, -cell_id) %>% 
    dplyr::rename_with(.fn = .fn, .cols = .cols, ...)
  
  check_renamed_variables(base::colnames(rename_df))
  
  final_df <- base::cbind(core_df, rename_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "cluster", df = final_df, phase = phase)
  
  base::return(object)
  
  
}


#' @title Rename cluster variables
#' 
#' @description Implementations of the functions \code{dplyr::rename()} and 
#' \code{dplyr::rename_with()} that can be used to rename the variables of the cell cluster 
#' data.frame. 
#'
#' @inherit renameMetaDf params details 
#' 
#' @seealso \code{dplyr::rename()}, \code{dplyr::rename_with()}
#' 
#' @return
#' @export
#'

renameClusterDf <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getClusterDf(object, phase = phase) %>%
    dplyr::select(cell_id)
  
  rename_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(-cell_line, -condition, -cell_id) %>% 
    dplyr::rename(...)
  
  check_renamed_variables(base::colnames(rename_df))
  
  final_df <- base::cbind(core_df, rename_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "cluster", df = final_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname renameClusterDf
#' @export
renameClusterDfWith <- function(object, .fn, .cols = dplyr::everything(), ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getClusterDf(object, phase = phase) %>%
    dplyr::select(cell_id)
  
  rename_df <-
    getClusterDf(object, phase = phase) %>%
    dplyr::select(-cell_id) %>% 
    dplyr::rename_with(.fn = .fn, .cols = .cols, ...)
  
  check_renamed_variables(base::colnames(rename_df))
  
  final_df <- base::cbind(core_df, rename_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "cluster", df = final_df, phase = phase)
  
  base::return(object)
  
  
}










