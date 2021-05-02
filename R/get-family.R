
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


# Data extraction ---------------------------------------------------------

#' @title Obtain cluster data
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame that contains the cell ids and their cluster belonging.
#' @export
#'

getClusterDf <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_df <- object@data$cluster[[phase]]
  
  base::return(cluster_df)
  
}


#' @rdname getClusterDf
#' @export
getClusterData <- function(object, phase = NULL){
  
  warning("deprecatd in favor of getClusterDf()")
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  object@data$cluster[[phase]]
  
}

#' @title Obtain data slots
#' 
#' @description A wrapper around \code{purr::map_df()} and the respective 
#' list of the data slot of interest. 
#'
#' @inherit argument_dummy params
#' @param data_slot Character value. One of \emph{'stats', 'tracks', 'meta'} or \emph{'cluster'}.
#'
#' @return The data.frame of interest. 
#' @export
#'
getData <- function(object, data_slot, phase){
  
  warning("This function is deprecated and might be deleted in the future.")
  
  if(!time_displaced_tmt(object)){
    
    slot_df <- 
      purrr::map_df(.x = object@data[[data_slot]], .f = ~ .x)
    
  } else if(base::all(phase == "all")){
    
    slot_df <- 
      purrr::map_df(.x = object@data[[data_slot]], .f = ~ .x)
    
  } else {
    
    slot_df <- 
      purrr::map_df(.x = object@data[[data_slot]][phase], 
                    .f = ~ .x)
    
  }
  
  base::return(slot_df)
  
}



#' @title Obtain meta data
#'
#' @inherit check_object params
#'
#' @return The data.frame of interest. 
#' @export
#'

getMetaDf <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  object@data$meta[[phase]]
  
}

#' @title Obtain stat data.frame 
#'
#' @inherit check_object params
#' @inherit with_meta params
#' @inherit with_cluster params
#' @inherit phase_all params
#'
#' @return A data.frame with all numeric variables summarizing the measurements of 
#' the track data.frame. 
#' 
#' @export
#'

getStatsDf <- function(object, phase = NULL, with_cluster = NULL, with_meta = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  stat_df <- object@data$stats[[phase]]
  
  # add cluster
  if(base::isTRUE(with_cluster)){
    
    cluster_df <- dplyr::select(object@data$cluster[[phase]], -phase)  
    
    if(base::ncol(cluster_df) == 1){
      
      if(time_displaced_tmt(object)){
        
        add <- glue::glue(" for the {phase} phase.")
        
      } else {
        
        add <- "."
        
      }
      
      base::warning(glue::glue("You set 'with_cluster' to TRUE but no cluster variables have been calculated yet{add}"))
      
    } else {
      
      stat_df <- dplyr::left_join(x = stat_df, y = cluster_df, by = "cell_id")  
      
    }
    
  }
  
  # add meta
  if(base::isTRUE(with_meta)){
    
    meta_df <- dplyr::select(object@data$meta[[phase]], -phase)
    
    stat_df <- dplyr::left_join(x = stat_df, y = meta_df, by = "cell_id")
    
  }
  
  base::return(stat_df)  
  
}

#' @rdname getStatsDf
#' @export
getStats <- getStatsDf



#' @title Obtain track data.frame. 
#'
#' @inherit check_object params
#' @inherit with_meta params
#' @inherit with_cluster params
#' @inherit phase_all params
#' @inherit phase_cluster params
#' @inherit verbose params
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
        
        meta_df <- dplyr::select(object@data$meta[[p]], -phase)
        
        track_df <- dplyr::left_join(x = track_df, y = meta_df, by = "cell_id")
        
      }
      
      base::return(track_df)
      
    }
  )
  
  if(base::isTRUE(with_cluster) & base::length(phase) == 1){
    
    cluster_df <- dplyr::select(object@data$cluster[[phase]], - phase)
    
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



# -----




# Cluster extraction ------------------------------------------------------

#' @title Obtain celltracers clustering objects
#'
#' @inherit argument_dummy params 
#'
#' @return An S4 object of \emph{'hclust_conv'}, \emph{'kmeans_conv'} or \emph{'pam_conv'}.
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

#' @rdname getHclustConv
#' @export
getKmeansConv <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_obj <- object@analysis$clustering$kmeans[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "kmeans_conv",
    phase = phase, 
    ref_input = "kmeans clustering object", 
    ref_fun = "initiateKmeansClustering()"
  )
  
  base::return(cluster_obj)
  
}

#' @rdname getHclustConv
#' @export
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
  
  cluster_obj <- object@analysis$correlation[[phase]]
  
  check_availability(
    evaluate = !base::is.null(cluster_obj) & base::class(cluster_obj) == "corr_conv",
    phase = phase, 
    ref_input = "correlation object", 
    ref_fun = "initiateCorrelation()"
  )
  
  base::return(cluster_obj)
  
}


# -----

# Other Info extraction ----------------------------------------------------

# Exported ---


#' @title Obtain group names a grouping variable contains
#'
#' @inherit check_object params
#' @param option Character value. Denotes the discrete variable - the grouping of cells - 
#' of interest. Use \code{getGroupingOptions()} to obtain all valid input options. 
#'
#' @return Character vector of group names. 
#' 
#' @export

getGroupNames <- function(object, option, phase = "all"){
  
  group_vec <- 
    getStats(object = object, phase = phase) %>% 
    dplyr::pull(var = {{option}}) 
  
  if(base::is.factor(group_vec)){
    
    base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping option '{option}' must be a character vector or a factor.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
}

#' @rdname getGroupNames
#' @export
getGroups <- function(object, option){
  
  warning("getGroups() is deprecated. Use getGroupNames()")
  
  group_vec <- 
    getMeta(object) %>% 
    dplyr::pull(var = {{option}}) 
  
  if(base::is.factor(group_vec)){
    
    base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    base::unique(group_vec)
    
  } else {
    
    base::stop(glue::glue("The result of grouping option '{option}' must be a character vector or a factor."))
    
  }
  
}


#' @title Obtain variable names that group the cells 
#' 
#' @description This function returns the names of the variables that 
#' group cell ids and can therefore be used as input for the \code{across}
#' argument. 
#'
#' @inherit check_object params
#' @inherit phase_single params
#'
#' @return An informative list. 
#' @export

getGroupingOptions <- function(object, phase = NULL){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getVariableNames(object = object, 
                   phase = phase, 
                   variable_classes = c("meta", "cluster")
                   )
  
}

#' @rdname getGroupingOptions
#' @export
getAcrossOptions <- function(object, phase = NULL){
  
  warning("getAcrossOptions() is deprecated. Use getGroupingOptions()")
  
  getVariableNames(object = object, 
                   phase = phase, 
                   variable_classes = c("input", "cluster"))
  
}


#' @title Obtain all numeric stat-variables
#'
#' @inherit check_object params
#'
#' @return A character vector. 
#' @export
#'

getStatVariableNames <- function(object){
  
  getStatsDf(object, with_meta = FALSE, with_cluster = FALSE) %>% 
    dplyr::select(-cell_id, -phase) %>% 
    base::colnames()
  
}


#' @title Obtain well plate information 
#'
#' @inherit check_object params
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
#' @inherit check_object params
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
#' @inherit check_object params
#' @inherit phase_single params  
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
#' @inherit check_object params 
#'
#' @details Useful helper function when it comes to specify conditions and cell lines 
#' of interest via the \code{across_subset}-argument.
#' 
#' @return Character vector.
#' @export
#'

getCellLines <- function(object){
  
  getMetaDf(object, phase = "first") %>% 
    dplyr::pull(var = "cell_line") %>% 
    base::levels()
  
}

#' @rdname getCellLines
#' @export
#' 
getConditions <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getMeta(object, phase = phase) %>% 
    dplyr::pull(var = "condition") %>% 
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
  
  getStats(object = object) %>% 
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
  
  object@data$tracks %>% base::names()
  
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

