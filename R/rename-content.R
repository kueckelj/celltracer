#' @title Rename groups
#' 
#' @description Allows to rename particular groups within a grouping variable
#' of the cypro object's cell data.
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
#' @inherit updated_object return
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
#' \code{renameMetaDf()} changes the names of individual variables using \emph{new_name} = \emph{old_name}
#' syntax. \code{renameMetaDfWith()} renames variables with a function specified in argument \code{.fn}.
#'
#' @inherit argument_dummy params
#' @param .fn A function used to transform the variable names. Should return a character vector of the same
#' length as the input. Can be specified with formula syntax. 
#'
#' @details The variables \emph{cell_id, cell_line} and \emph{condition} are removed 
#' prior to the renaming as they are protected. The renaming process must not
#' result in columns that carry one of these names nor in the names \emph{well_plate_name,
#' well_index, well_image} or \emph{well}. 
#' 
#' Use \code{getGroupingVariableNames()} to check if renaming resulted in 
#' the desired output.
#' 
#' @seealso \code{dplyr::rename()}, \code{dplyr::rename_with()}
#' 
#' @inherit updated_object return
#' @export
#'

renameMetaDf <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(cell_id, cell_line, condition)
  
  renamed_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(-cell_line, -condition, -cell_id) %>% 
    dplyr::rename(...)
  
  check_renamed_variables(base::colnames(renamed_df))
  
  final_df <- base::cbind(core_df, renamed_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "meta", df = final_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname renameMetaDf
#' @export
renameMetaDfWith <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(cell_id, cell_line, condition)
  
  renamed_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(-cell_line, -condition, -cell_id) %>% 
    dplyr::rename_with(...)
  
  check_renamed_variables(base::colnames(renamed_df))
  
  final_df <- base::cbind(core_df, renamed_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "meta", df = final_df, phase = phase)
  
  base::return(object)
  
  
}


#' @title Rename cluster variables
#' 
#' @description Implementations of the functions \code{dplyr::rename()} and 
#' \code{dplyr::rename_with()} that can be used to rename the variables of the cell cluster 
#' data.frame. 
#' 
#' \code{renameClusterDf()} changes the names of individual variables using \emph{new_name} = \emph{old_name}
#' syntax. \code{renameClusterDfWith()} renames variables with a function specified in argument \code{.fn}.
#'
#' @inherit renameMetaDf params details return
#' 
#' @seealso \code{dplyr::rename()}, \code{dplyr::rename_with()}
#' 
#' @export
#'

renameClusterDf <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getClusterDf(object, phase = phase) %>%
    dplyr::select(cell_id)
  
  renamed_df <-
    getMetaDf(object, phase = phase) %>%
    dplyr::select(-cell_line, -condition, -cell_id) %>% 
    dplyr::rename(...)
  
  check_renamed_variables(base::colnames(renamed_df))
  
  final_df <- base::cbind(core_df, renamed_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "cluster", df = final_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname renameClusterDf
#' @export
renameClusterDfWith <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  core_df <-
    getClusterDf(object, phase = phase) %>%
    dplyr::select(cell_id)
  
  renamed_df <-
    getClusterDf(object, phase = phase) %>%
    dplyr::select(-cell_id) %>% 
    dplyr::rename_with(...)
  
  check_renamed_variables(base::colnames(renamed_df))
  
  final_df <- base::cbind(core_df, renamed_df) %>% tibble::as_tibble()
  
  object <- setCellDf(object, slot = "cluster", df = final_df, phase = phase)
  
  base::return(object)
  
  
}



#' @title Rename statistic variables 
#' 
#' @description Implementations of the functions \code{dplyr::rename()} and 
#' \code{dplyr::rename_with()} that can be used to rename the variables of the cell statistic 
#' data.frame. 
#' 
#' \code{renameClusterDf()} changes the names of individual variables using \emph{new_name} = \emph{old_name}
#' syntax. \code{renameClusterDfWith()} renames variables with a function specified in argument \code{.fn}.
#' 
#' @inherit renameMetaDf params return
#' 
#' @details The variable \emph{cell_id} is removed prior to the renaming as it is protected. The renaming process must not
#' result in columns that carry either name of \emph{cell_id, condition, cell_line, well_plate_name,
#' well_index, well_image} or \emph{well}.
#' 
#' Renaming the statistic data.frame affects all slots of the cypro object that refer to 
#' statistic variables (correlation analysis, variable sets etc.). All are renamed according to 
#' the input. 
#' 
#' Use \code{getStatVariableNames()} to check if renaming resulted in 
#' the desired output.
#'
#' @export
#'
renameStatsDf <- function(object, ...){
  
  check_object(object)
  
  # rename stat_df 
  if(multiplePhases(object)){
    
    object@cdata$stats <- 
      purrr::map(.x  = object@cdata$stats,
                 .f = function(stat_df){
                   
                   cell_id_var <- stat_df$cell_id
                   
                   renamed_df <- 
                     dplyr::select(stat_df, -cell_id) %>% 
                     dplyr::rename(...) %>% 
                     dplyr::mutate(cell_id = {{cell_id_var}}) %>% 
                     dplyr::select(cell_id, dplyr::everything())
                   
                   base::return(cell_id_var)
                   
                 })
    
  } else {
    
    stat_df <-
      getStatsDf(object, with_cluster = FALSE, with_meta = FALSE, with_well_plate = FALSE)
    
    cell_id_var <- stat_df$cell_id
    
    renamed_df <- 
      dplyr::select(stat_df, -cell_id) %>% 
      dplyr::rename(...) %>% 
      dplyr::mutate(cell_id = {{cell_id_var}}) %>% 
      dplyr::select(cell_id, dplyr::everything())
    
    object@cdata$stats <- renamed_df
    
  }
  
  
  # rename variable sets 
  object@variable_sets <- 
    purrr::map(.x = object@variable_sets,
               .f = function(vset){
                 
                 renamed_vset <- base::tryCatch({
                   
                   confuns::vrename(input = vset, ...)
                   
                 }, error = function(error){
                   
                   NA
                   
                 })
                 
                 if(!base::is.character(renamed_vset)){
                   
                   base::return(vset)
                   
                 } else {
                   
                   base::return(renamed_vset)
                   
                 }
                 
               })
  
  # rename variables 
  if(multiplePhases(object)){
    
    object@vdata$summary <- 
      purrr::map(.x = object@vdata$summary, 
                 .f = function(summary_df){
                   
                   summary_df$variable <- 
                     confuns::vrename(input = summary_df$variable, ...)
                   
                   base::return(summary_df)
                   
                 })
    
  } else {
    
    object@vdata$summary$variable <- 
      confuns::vrename(input = object@vdata$summary$variable, ...)
    
  }
  
  # rename correlation
  if(multiplePhases(object)){
    
    object@analysis$correlation <- 
      purrr::map(.x = object@analysis$correlation, 
                 .f = function(vset_by_phases){
                   
                   vset_by_phases_renamed <- 
                     purrr::map(.x = vset_by_phases, 
                                .f = confuns::rename_numeric_vars, 
                                rename.data = FALSE, 
                                ...)
                   
                   base::return(vset_by_phases_renamed)
                   
                 })
    
  } else {
    
    object@analysis$correlation <- 
      purrr::map(.x = object@analysis$correlation, 
                 .f = confuns::rename_numeric_vars, 
                 rename.data = FALSE, 
                 ...)
    
  }
  
  # rename clustering 
  if(multiplePhases(object)){
    
    object@analysis$clustering <- 
      purrr::map(.x = object@analysis$clustering, # iterate over hclust, kmeans, pam 
                 .f = function(method_list){  # names(method_list): vsets for which clustering with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(vset_by_phases){
                                  
                                  vset_by_phases_renamed <-
                                    purrr::map(.x = vset_by_phases, 
                                               .f = function(conv_object){
                                                 
                                                 conv_object@variables <- 
                                                   confuns::vrename(input = conv_object@variables, ...)
                                                 
                                                 base::return(conv_object)
                                                 
                                               })
                                  
                                  base::return(vset_by_phases_renamed)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
    
  } else {
    
    object@analysis$clustering <- 
      purrr::map(.x = object@analysis$clustering, # iterate over hclust, kmeans, pam 
                 .f = function(method_list){  # names(method_list): vsets for which clustering with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(conv_object){
                                  
                                  conv_object@variables <- 
                                    confuns::vrename(input = conv_object@variables, ...)
                                  
                                  base::return(conv_object)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
  }
  
  
  # rename dim red 
  if(multiplePhases(object)){
    
    object@analysis$dim_red <- 
      purrr::map(.x = object@analysis$dim_red, # iterate over pca, tsne, umap
                 .f = function(method_list){  # names(method_list): vsets for which dim red with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(vset_by_phases){
                                  
                                  vset_by_phases_renamed <-
                                    purrr::map(.x = vset_by_phases, 
                                               .f = function(conv_object){
                                                 
                                                 conv_object@variables_num <- 
                                                   confuns::vrename(input = conv_object@variables_num, ...)
                                                 
                                                 base::return(conv_object)
                                                 
                                               })
                                  
                                  base::return(vset_by_phases_renamed)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
    
  } else {
    
    object@analysis$dim_red <- 
      purrr::map(.x = object@analysis$dim_red, # iterate over pca, tsne, umap 
                 .f = function(method_list){  # names(method_list): vsets for which dim red with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(conv_object){
                                  
                                  conv_object@variables_num <- 
                                    confuns::vrename(input = conv_object@variables_num, ...)
                                  
                                  base::return(conv_object)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
  }
  
  
  base::return(object)
  
  
}


#' @rdname renameStatsDf
#' @export
renameStatsDfWith <- function(object, ..., phase = NULL){
  
  check_object(object)
  
  # rename stat_df 
  if(multiplePhases(object)){
    
    object@cdata$stats <- 
      purrr::map(.x  = object@cdata$stats,
                 .f = function(stat_df){
                   
                   cell_id_var <- stat_df$cell_id
                   
                   renamed_df <- 
                     dplyr::select(stat_df, -cell_id) %>% 
                     dplyr::rename_with(...) %>% 
                     dplyr::mutate(cell_id = {{cell_id_var}}) %>% 
                     dplyr::select(cell_id, dplyr::everything())
                   
                   base::return(cell_id_var)
                   
                 })
    
  } else {
    
    stat_df <-
      getStatsDf(object, phase = phase, with_cluster = F, with_meta = F, with_well_plate = F)
    
    cell_id_var <- stat_df$cell_id
    
    renamed_df <- 
      dplyr::select(stat_df, -cell_id) %>% 
      dplyr::rename_with(...) %>% 
      dplyr::mutate(cell_id = {{cell_id_var}}) %>% 
      dplyr::select(cell_id, dplyr::everything())
    
    object@cdata$stats <- renamed_df
    
  }
  
  # rename variable sets 
  object@variable_sets <- 
    purrr::map(.x = object@variable_sets,
               .f = function(vset){
                 
                 confuns::vrename_with(input = vset, ...)
                 
               })
  
  # rename variables 
  if(multiplePhases(object)){
    
    object@vdata$summary <- 
      purrr::map(.x = object@vdata$summary, 
                 .f = function(summary_df){
                   
                   summary_df$variable <- 
                     confuns::vrename_with(input = summary_df$variable, ...)
                   
                   base::return(summary_df)
                   
                 })
    
  } else {
    
    object@vdata$summary$variable <- 
      confuns::vrename_with(input = object@vdata$summary$variable, ...)
    
  }
  
  # rename correlation
  if(multiplePhases(object)){
    
    object@analysis$correlation <- 
      purrr::map(.x = object@analysis$correlation, 
                 .f = function(vset_by_phases){
                   
                   vset_by_phases_renamed <- 
                     purrr::map(.x = vset_by_phases, 
                                .f = confuns::rename_numeric_vars_with, 
                                rename.data = FALSE,
                                ...)
                   
                   base::return(vset_by_phases_renamed)
                   
                 })
    
  } else {
    
    object@analysis$correlation <- 
      purrr::map(.x = object@analysis$correlation, 
                 .f = confuns::rename_numeric_vars_with, 
                 rename.data = FALSE,
                 ...)
    
  }
  
  # rename clustering 
  if(multiplePhases(object)){
    
    object@analysis$clustering <- 
      purrr::map(.x = object@analysis$clustering, # iterate over hclust, kmeans, pam 
                 .f = function(method_list){  # names(method_list): vsets for which clustering with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(vset_by_phases){
                                  
                                  vset_by_phases_renamed <-
                                    purrr::map(.x = vset_by_phases, 
                                               .f = function(conv_object){
                                                 
                                                 conv_object@variables <- 
                                                   confuns::vrename_with(
                                                     input = conv_object@variables, 
                                                     ...)
                                                 
                                                 base::return(conv_object)
                                                 
                                               })
                                  
                                  base::return(vset_by_phases_renamed)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
    
  } else {
    
    object@analysis$clustering <- 
      purrr::map(.x = object@analysis$clustering, # iterate over hclust, kmeans, pam 
                 .f = function(method_list){  # names(method_list): vsets for which clustering with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(conv_object){
                                  
                                  conv_object@variables <- 
                                    confuns::vrename_with(
                                      input = conv_object@variables, 
                                      ...)
                                  
                                  base::return(conv_object)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
  }
  
  
  # rename dim red 
  if(multiplePhases(object)){
    
    object@analysis$dim_red <- 
      purrr::map(.x = object@analysis$dim_red, # iterate over pca, tsne, umap
                 .f = function(method_list){  # names(method_list): vsets for which dim red with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(vset_by_phases){
                                  
                                  vset_by_phases_renamed <-
                                    purrr::map(.x = vset_by_phases, 
                                               .f = function(conv_object){
                                                 
                                                 conv_object@variables_num <- 
                                                   confuns::vrename_with(
                                                     input = conv_object@variables_num, 
                                                     ...)
                                                 
                                                 base::return(conv_object)
                                                 
                                               })
                                  
                                  base::return(vset_by_phases_renamed)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
    
  } else {
    
    object@analysis$dim_red <- 
      purrr::map(.x = object@analysis$dim_red, # iterate over pca, tsne, umap 
                 .f = function(method_list){  # names(method_list): vsets for which dim red with that method has been computed
                   
                   method_list_out <- 
                     purrr::map(.x = method_list, 
                                .f = function(conv_object){
                                  
                                  conv_object@variables_num <- 
                                    confuns::vrename_with(
                                      input = conv_object@variables_num, 
                                      ...)
                                  
                                  base::return(conv_object)
                                  
                                })
                   
                   base::return(method_list_out)
                   
                 })
    
  }
  
  
  base::return(object)
  
}






