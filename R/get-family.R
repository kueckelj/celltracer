
# Helper ------------------------------------------------------------------
time_displaced_tmt <- function(object){
  
  if(base::length(getPhases(object)) == 1){
    
    base::return(FALSE)
    
  } else {
    
    base::return(TRUE)
    
  }
  
}

join_with_meta <- function(object, df, phase){
  
  dplyr::left_join(x = df, y = purrr::map_df(phase, .f = ~ object@data$meta[[.x]]), by = "cell_id")
  
}

# -----


# Analysis extraction ------------------------------------------------------

#' @title Obtain celltracers clustering objects
#'
#' @inherit argument_dummy params 
#'
#' @return An S4 object of \emph{'hclust_conv'}, \emph{'kmeans_conv'} or \emph{'pam_conv'}.
#' @export
#'
getHclustConv <- function(object, variable_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phase = 1)
  
  cluster_object <- object@analysis$clustering$hclust[[phase]][[variable_set]]
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "hclust_conv",
    phase = phase, 
    ref_input = glue::glue("hierarchical clustering object with variable set '{variable_set}'"), 
    ref_fun = "initiateHierarchicalClustering()"
  )
  
  cluster_object <- 
    hlpr_add_data_to_cluster_object(object, cluster_object, with_data = with_data, phase = phase)
  
  base::return(cluster_object)
  
}

#' @rdname getHclustConv
#' @export
getKmeansConv <- function(object, variable_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_object <- object@analysis$clustering$kmeans[[phase]][[variable_set]]
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "kmeans_conv",
    phase = phase, 
    ref_input = glue::glue("kmeans clustering object with variable set '{variable_set}'"), 
    ref_fun = "initiateKmeansClustering()"
  )
  
  cluster_object <- 
    hlpr_add_data_to_cluster_object(object, cluster_object, with_data = with_data, phase = phase)
  
  base::return(cluster_object)
  
}

#' @rdname getHclustConv
#' @export
getPamConv <- function(object, variable_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_object <- object@analysis$clustering$pam[[phase]][[variable_set]]
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "pam_conv",
    phase = phase, 
    ref_input = glue::glue("PAM clustering object with variable set '{variable_set}'"), 
    ref_fun = "initiatePamClustering()"
  )
  
  cluster_object <- 
    hlpr_add_data_to_cluster_object(object, cluster_object, with_data = with_data, phase = phase)
  
  base::return(cluster_object)
  
}


#' @title Obtain celltracers correlation objects
#'
#' @inherit argument_dummy params
#'
#' @return An S4 object of class \emph{'corr_conv'}
#' @export
#'
getCorrConv <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_object <- object@analysis$correlation[[phase]]
  
  check_availability(
    evaluate = !base::is.null(corr_object) & base::class(corr_object) == "corr_conv",
    phase = phase, 
    ref_input = "correlation object", 
    ref_fun = "initiateCorrelation()"
  )
  
  base::return(corr_object)
  
}


# -----



# Data extraction ---------------------------------------------------------

#' @title Obtain cluster data
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame that contains the cell ids and their cluster belonging.
#' @export
#'

getGroupingDf <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  group_df <- object@data$grouping[[phase]]
  
  base::return(group_df)
  
  
}


#' @title Obtain meta data
#'
#' @inherit check_object params
#'
#' @return A data.frame with all variables providing meta information about cells of 
#' the track data.frame. (such as condition, cell line, well plate belonging etc.)
#' @export
#'

getMetaDf <- function(object){
  
  check_object(object)
  assign_default(object)
  
  meta_df <- object@data$meta
  
  base::return(meta_df)
  
}

#' @title Obtain stat data.frame 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame with all numeric variables summarizing the measurements of 
#' the track data.frame. 
#' 
#' @export
#'

getStatsDf <- function(object, phase = NULL, with_cluster = NULL, with_meta = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  stat_df <- object@data$stats[[phase]]
  
  # add cluster
  if(base::isTRUE(with_cluster)){
    
    cluster_df <- dplyr::select(object@data$grouping[[phase]], -phase)  
    
    if(base::ncol(cluster_df) == 1){
      
      if(time_displaced_tmt(object)){
        
        add <- glue::glue(" for the {phase} phase.")
        
      } else {
        
        add <- "."
        
      }
      
      msg <- glue::glue("You set 'with_cluster' to TRUE but no cluster variables have been calculated yet{add}")
      
      confuns::give_feedback(msg = msg, verbose = verbose)
      
    } else {
      
      stat_df <- dplyr::left_join(x = stat_df, y = cluster_df, by = "cell_id")  
      
    }
    
  }
  
  # add meta
  if(base::isTRUE(with_meta)){
    
    meta_df <- getMetaDf(object)
    
    stat_df <- dplyr::left_join(x = stat_df, y = meta_df, by = "cell_id")
    
  }
  
  base::return(stat_df)  
  
}

#' @rdname getStatsDf
#' @export
getStats <- getStatsDf



#' @title Obtain track data.frame. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation refers to a cell at a given frame.
#' 
#' @export
#'

getTracksDf <- function(object, phase = NULL, with_cluster = NULL, with_meta = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  track_df_final <- purrr::map_df(
    .x = phase, 
    .f = function(p){
      
      track_df <- object@data$tracks[[p]]
      
      if(base::isTRUE(with_meta)){
        
        meta_df <- getMetaDf(object)
        
        track_df <- dplyr::left_join(x = track_df, y = meta_df, by = "cell_id")
        
      }
      
      base::return(track_df)
      
    }
  )
  
  if(base::isTRUE(with_cluster) & base::length(phase) == 1){
    
    cluster_df <- dplyr::select(object@data$grouping[[phase]], - phase)
    
    if(base::ncol(cluster_df) == 1){
      
      msg <- glue::glue("No custer variables found for {phase} phase. Set argument 'with_cluster' to FALSE to proceed.")
      
      confuns::give_feedback(msg = msg, with.time = FALSE, verbose = verbose)
      
    } else {
      
      track_df_final <- dplyr::left_join(x = track_df_final, y = cluster_df, by = "cell_id")
      
    }
    
  }
  
  base::return(track_df_final)
  
}


#' @rdname getTracksDf
#' @export
getTracks <- getTracksDf


#' @title Obtain defined sets of variables
#' 
#' @description Convenient access to defined sets of variables or names 
#' mentioned sets. 
#'
#' @inherit argument_dummy params
#' @param variable_set Character value. The name of the variable set of interest.
#'
#' @return A list of character vectors or a character vector of names. 
#' @export
#'

getVariableSet <- function(object, variable_set){
  
  var_set <- object@variable_sets[[variable_set]]
  
  confuns::check_one_of(
    input = variable_set, 
    against = base::names(object@variable_sets), 
    fdb.opt = 2, 
    ref.opt.2 = "defined variable sets"
  )
  
  base::return(var_set)
  
}

#' @rdname getVariableSet
#' @export
getVariableSets <- function(object){
  
  object@variable_sets
  
}

#' @rdname getVariableSet
#' @export
getVariableSetNames <- function(object){
  
  base::names(object@variable_sets)
  
}


# -----





# Outlier detection -------------------------------------------------------

#' @title Obtain outlier detection results 
#' 
#' @description These functions can be used to extract the results of the outlier 
#' detection algorithms. 
#'
#' @inherit argument_dummy params
#' 
#' @return \code{getOutlierResults()} returns a list in which each slot contains 
#' the results for a specific method. \code{getOutlierIds()} returns a character 
#' vector of cell ids containing all cell ids that have been detected as outliers
#' by at least one method.
#' @export
#'
getOutlierResults <- function(object, method_outlier = NULL, check = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(base::isTRUE(check)){
    
    if(!existOutlierResults(object)){
      
      base::stop("Did not find any outlier detection results.")
      
    }
    
  }
  
  outlier_list <- object@analysis$outlier_detection
  
  if(base::is.character(method_outlier)){
    
    confuns::check_vector(
      input = method_outlier,
      against = base::names(outlier_list), 
      ref.input = "input for argument 'method_outlier'", 
      ref.against = "methods with which outliers have been detected", 
      fdb.fn = "stop")
    
    outlier_list <- outlier_list[method_outlier]
    
  }
  
  base::return(outlier_list)
  
}

#' @rdname getOutlierResults
#' @export
getOutlierIds <- function(object, method_outlier = NULL, check = FALSE){
  
  outlier_list <- getOutlierResults(object, check = check)
  
  outlier_ids <-  
    purrr::map(outlier_list, .f = ~ purrr::flatten(.x = .x)) %>% 
    purrr::flatten() %>% 
    purrr::flatten_chr() %>% 
    base::unique()
  
  base::return(outlier_ids)
  
}


# -----


# Other Info extraction ----------------------------------------------------

# Exported ---


#' @title Obtain group names a grouping variable contains
#' 
#' @description This function returns the names of the groups in which a specific grouping
#' variable groups the cells. Useful to obtain input options for arguments like \code{across_subset}. 
#'
#' @inherit argument_dummy params
#' @param grouping_variable Character value. Denotes the discrete variable - the grouping of cells - 
#' of interest. Use \code{getGroupingVariableNames()} to obtain all valid input options. 
#'
#' @return Character vector of group names. 
#' 
#' @examples 
#' 
#'  all_conditions <- getGroupNames(object, grouping_variable = "condition")
#'  
#'  all_cell_lines <- getGroupNames(object, grouping_variable = "cell_line")
#'  
#'  pam_k4_cluster <- getGroupNames(object, grouping_variable = "pam_euclidean_k_4")
#' 
#' @export

getGroupNames <- function(object, grouping_variable, phase = "all"){
  
  group_vec <- 
    getStats(object = object, phase = phase) %>% 
    dplyr::pull(var = {{grouping_variable}}) 
  
  if(base::is.factor(group_vec)){
    
    base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping variable '{option}' must be a character vector or a factor.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
}


#' @title Obtain variable names of your data
#' 
#' @description Convenient access to the names of your objects data variables. Useful to 
#' obtain vectors of variable names as input for recurring arguments like \code{variables}.
#'
#' @inherit argument_dummy params
#' @param ... Additional selection helpers from the \code{tidyselect} package that match 
#' variable names according to a given pattern. 
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export
#' 
#' @examples 
#' 
#'  # returns all stat variable names starting with 'mean'
#'  mean_vars <- getStatVariableNames(object, starts_with("mean"))
#'  
#'  plotBoxplot(object, variables = mean_vars)

getGroupingVariableNames <- function(object, ..., phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  group_df <- 
    getGroupingDf(object, phase = phase) %>% 
    dplyr::select(-phase, -cell_id)
  
  selected_df <- dplyr::select(group_df, ...)
  
  if(base::ncol(selected_df) == 0){
    
    # if TRUE then ncol == 0 because selection resulted in no vars
    selection_helpers_provided <- 
      base::tryCatch({
        
        # leads to error if tidyselection specified
        list(...)
        
      }, error = function(error){
        
         TRUE
        
      })
    
    if(base::isTRUE(selection_helpers_provided)){
      
      base::stop("Tidyselect input resulted in no variables.")
      
      # if FALSE then ncol == 0 because no tidyselection specified: return all variable names
    } else {
      
      selected_df <- group_df
      
    }
    
  }
  
  grouping_vars <- 
    base::colnames(selected_df)
  
  base::return(grouping_vars)
  
}


#' @rdname getClusterVariableNames
#' @export 
getStatVariableNames <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  stat_df <-
    getStatsDf(object, with_meta = FALSE, with_cluster = FALSE) %>% 
    dplyr::select(-cell_id, -phase)
  
  selected_df <- dplyr::select(stat_df, ...)
  
  if(base::ncol(selected_df) == 0){
    
    # if TRUE then ncol == 0 because selection resulted in no vars
    selection_helpers_provided <- 
      base::tryCatch({
        
        # leads to error if tidyselection specified
        list(...)
        
      }, error = function(error){
        
        TRUE
        
      })
    
    if(base::isTRUE(selection_helpers_provided)){
      
      base::stop("Tidyselect input resulted in no variables.")
      
      # if FALSE then ncol == 0 because no tidyselection specified: return all variable names
    } else {
      
      selected_df <- stat_df
      
    }
    
  }
  
  
  stat_variable_names <- 
    base::colnames(selected_df)
  
  base::return(stat_variable_names)
  
}




#' @title Obtain well plate information 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each row contains information about a well. 
#' @export
#'

getWellPlateDf <- function(object, well_plate = NULL){
  
  confuns::check_one_of(
    input = well_plate, 
    against = getWellPlateNames(object)
  )
  
  wp_df <- object@well_plates[[well_plate]]$wp_df_eval
  
  base::return(wp_df)
  
}

#' @title Obtain well plate names
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' @export

getWellPlateNames <- function(object){
  
  object@well_plates %>% base::names()
  
}


#' @title Obtain variable overview
#' 
#' @description If the variable denoted in \emph{variable_name} is categorical (character or factor)
#' all unique values/levels are returned. If the variable is numeric it is given to 
#' \code{psych::describe()} which returns a statistical summary. 
#'
#' @inherit argument_dummy params 
#' @param variable_name Character value. Denotes the variable of interest. Valid inputs can be 
#' obtained via the function \code{getVariableNames()}.
#'
#' @return A character vector or a data.frame of one row containing basic descriptive statistics.
#' @export
#'

getVariableValues <- function(object, phase = NULL, variable_name){
  
  check_object(object)
  assign_default(object)
  
  confuns::is_value(variable_name, "character", ref = "variable_name")
  
  extracted_var <- 
    getStatsDf(object, phase = phase) %>% 
    dplyr::pull(var = {{variable_name}})
  
  
  if(base::is.factor(extracted_var)){
    
    values <- base::levels(extracted_var)
    
  } else if(base::is.character(extracted_var)){
    
    values <- base::unique(extracted_var)
    
  } else if(base::is.numeric(extracted_var)){
    
    values <-
      psych::describe(x = extracted_var) %>% 
      magrittr::set_rownames(value = variable_name)
    
  }
  
  base::return(values)
  
}


#' @title Obtain cell line and condition names 
#' 
#' @description Quick wrapper around the functionality of getGroupingVariableNames(). As 
#' the cell line does not change from experiment phase to experiment phase there is no 
#' need to specify it.
#'
#' @inherit check_object params 
#'
#' @details Useful helper function when it comes to specify conditions and cell lines 
#' of interest via the \code{across_subset}-argument.
#' 
#' @return Character vector.
#' @export
#'

getCellLines <- function(object){
  
  check_object(object)
  assign_default(object)
  
  getMetaDf(object) %>%
    dplyr::pull(cell_line) %>%
    base::levels()
  
}

#' @rdname getCellLines
#' @export
#' 
getConditions <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getGroupingDf(object, phase = phase) %>% 
    dplyr::pull(condition) %>% 
    base::levels()
  
  
}



# NOT EXPORTED ------------------------------------------------------------




#' @title Helper functions to extract information from the
#'
#' @return

getCategoricalVariablesNames <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getStats(object = object, phase = phase) %>% 
    dplyr::select_if(.predicate = base::is.factor) %>% 
    base::colnames()
  
}

#' @rdname getCategoricalVariablesNames
getNumericVariableNames <- function(object){
  
  getStatsDf(object = object) %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::colnames()
  
}

#' @rdname getCategoricalVariablesNames
getFrameSeq <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_num") %>% 
    base::unique()
  
}

#' @rdname getCategoricalVariablesNames
getFrameTimeSeq <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}


#' @rdname getCategoricalVariablesNames
getInterval <- function(object){
  
  object@set_up$itvl
  
}


#' @rdname getCategoricalVariablesNames
getIntervalUnit <- function(object){
  
  object@set_up$itvl_u
  
}


#' @rdname getCategoricalVariablesNames
getPhases <- function(object){
  
  object@set_up$phases %>% base::names()
  
}

#' @rdname getCategoricalVariablesNames
getVariableNames <- function(object,
                             phase = NULL,
                             variable_classes = c("cluster", "meta", "stats", "well_plate"),
                             flatten = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  select_list <- list()
  
  if("cluster" %in% variable_classes){
    
    cluster <- 
      getClusterDf(object, phase = phase) %>% 
      dplyr::select(-cell_id, -phase) %>% 
      base::colnames()
    
    select_list$cluster <- cluster
    
  }
  
  # meta input
  
  if("meta" %in% variable_classes){
    
    meta <-
      getMetaDf(object, phase = phase) %>% 
      dplyr::select(-dplyr::all_of(x = invalid_groups), -phase) %>% 
      base::colnames()
    
    select_list$meta <- meta
    
  }
  
  # meta well
  
  if("well_plate" %in% variable_classes){
    
    select_list$well_plate <- 
      getMetaDf(object, phase = phase) %>% 
      dplyr::select(dplyr::starts_with(match = "well"), -well_plate_index) %>% 
      base::colnames()
    
  }
  
  # stats
  
  if("stats" %in% variable_classes){
    
    select_list[["stats"]] <- 
      getStatVariableNames(object)
    
  }
  
  # ...
  
  if(base::isTRUE(flatten) & base::length(select_list) == 1){
    
    select_list <- purrr::flatten_chr(select_list)
    
  }
  
  base::return(select_list)
  
}

