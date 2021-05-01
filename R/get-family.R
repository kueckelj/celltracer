
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
#' @inherit check_object params
#' @inherit phase_single params
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
#' @inherit check_object params
#' @inherit phase_single params
#' @param data_slot Character value. One of \emph{'stats', 'tracks', 'meta'} or \emph{'cluster'}.
#'
#' @return The data.frame of interest. 
#' @export
#'

getData <- function(object, data_slot, phase){
  
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

getTracksDf <- function(object, phase = NULL, with_cluster = NULL, with_meta = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  track_df_final <- purrr::map_df(
    .x = phase, 
    .f = function(p){
      
      track_df <- object@data$tracks[[p]]
      
      if(base::isTRUE(with_cluster)){
        
        cluster_df <- dplyr::select(object@data$cluster[[p]], - phase)
        
        if(base::ncol(cluster_df)){
          
          msg <- glue::glue("No custer variables found for {p} phase. Set argument 'with_cluster' to FALSE to proceed.")
          
          give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
          
        } else {
          
          track_df <- dplyr::left_join(x = track_df, y = cluster_df, by = "cell_id")
          
        }
        
      }
      
      if(base::isTRUE(with_meta)){
        
        meta_df <- dplyr::select(object@data$meta[[p]], -phase)
        
        track_df <- dplyr::left_join(x = track_df, y = meta_df, by = "cell_id")
        
      }
      
      base::return(track_df)
      
    }
  )
  
  base::return(track_df_final)
  
}


#' @rdname getTracksDf
#' @export
getTracks <- getTracksDf


#' @title Obtain pam-clustering results. 
#'
#' @inherit check_object params 
#' @param k Numeric value. The k-value for which the pam-object of interest has been computed.
#' @inherit phase_single params
#'
#' @return A pam object - the return value of the function \code{cluster::pam()} that has been 
#' computed via the function \code{doPamClustering()}.
#' 
#' @export
#'

getPamObject <- function(object, k, phase = "first_tmt"){
  
  cluster_ref <- stringr::str_c("pam_k", k, sep = "")
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  pam_obj <- 
    object@cluster_info[["pam"]][[phase]][[cluster_ref]]
  
  base::return(pam_obj)
  
}

# -----


# Cluster related ---------------------------------------------------------

#' @title Obtain valid cluster names 
#' 
#' @description Returns all valid cluster names that can be used as input 
#' for the \code{across}-argument.
#'
#' @inherit check_object params
#' @inherit phase_single params  
#' @param algorithm_subset Character value or NULL. If character only the cluster 
#' names of the algorithms denoted are returned. 
#'
#' @return A list named according to all cluster algorithms found. Each named slot
#' contains a character vector of all cluster names of the algorithm. 
#' 
#' @export
#'
getClusterNames <- function(object,
                            phase = "first_tmt",
                            algorithm_subset = NULL){
  
  phase <- check_phase(object, phase)
  
  # extract all algorithms that have been used
  algorithms <- base::names(object@cluster_info)
  
  if(base::is.character(algorithm_subset)){
    
    algorithms <- algorithm_subset[algorithm_subset %in% algorithms]
    
  }
  
  # filter the respective phases
  cluster_list <- 
    object@cluster_info %>% 
    purrr::keep(.p = base::names(.) %in% algorithms) %>% 
    purrr::map(.f = ~ purrr::keep(.x = .x, .p = base::names(.x) %in% phase)) %>% 
    purrr::map(.f = ~ purrr::map(.x = .x, .f = ~ base::names(.x)))
  
  base::return(cluster_list)
  
}

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
                   variable_classes = c("input", "cluster"))
  
}

#' @rdname getGroupingOptions
#' @export
getAcrossOptions <- function(object, phase = "first_tmt"){
  
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
#' @return
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

getVariableValues <- function(object, phase = "first_tmt", variable_name){
  
  confuns::is_value(variable_name, "character", ref = "variable_name")
  
  extracted_var <- 
    getStats(object, phase = phase) %>% 
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
#' @return
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




#' @title Helper functions to extract 
#'
#' @param object 
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


#' @rdname getNumericVariableNames
#' @export
getNumericVariableNames <- function(object){
  
  getStats(object = object) %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::colnames()
  
}



#' @rdname getNumericVariableNames
#' @export
getFrameSeq <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_num") %>% 
    base::unique()
  
}

#' @rdname getNumericVariableNames
#' @export
getFrameTimeSeq <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}


#' @rdname getNumericVariableNames
#' @export
getInterval <- function(object){
  
  object@set_up$itvl
  
}


#' @rdname getNumericVariableNames
#' @export
getIntervalUnit <- function(object){
  
  object@set_up$itvl_u
  
}


#' @rdname getNumericVariableNames
#' @export
getPhases <- function(object){
  
  object@data$tracks %>% base::names()
  
}

#' @rdname getNumericVariableNames
#' @export
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

