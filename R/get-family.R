
# Helper ------------------------------------------------------------------
time_displaced_tmt <- function(object){
  
  if(object@set_up$tmt_start %in% c("From beginning", "No treatment")){
    
    base::return(FALSE)
    
  } else {
    
    base::return(TRUE)
    
  }
  
}

join_with_meta <- function(object, df){
  
  dplyr::left_join(x = df, y = object@data$meta, by = "cell_id")
  
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

getClusterData <- function(object, phase = "first_tmt"){

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


#' @title Obtain dimensional reduction data 
#'
#' @inherit check_object params 
#' @inherit dim_red_method params
#' @inherit with_meta params
#' @inherit with_stats params
#' @inherit phase_single params
#'
#' @return The data.frame of interest. 
#' @export
#'

getDimRed <- function(object,
                      dim_red_method = "umap",
                      with_meta = TRUE,
                      with_stats = TRUE,
                      phase = "first_tmt"){
  
  if(!time_displaced_tmt(object)){
    
    dim_red_df <- 
      purrr::map_df(.x = object@data$dim_red_method[[dim_red_method]], .f = ~ .x)
    
  } else {
    
    dim_red_df <- 
      object@data$dim_red_method[[dim_red_method]][[phase]]
    
  }
  
  if(base::isTRUE(with_meta)){
    
    dim_red_df <- dplyr::left_join(x = dim_red_df, 
                                   y = getMeta(object, phase), 
                                   by = "cell_id")
    
  }
  
  if(base::isTRUE(with_stats)){
    
    stat_df <- getStats(object, phase = phase, with_meta = FALSE)
    
    dim_red_df <- dplyr::left_join(x = dim_red_df, 
                                   y = stat_df, 
                                   by = "cell_id")
    
  }
  
  base::return(dim_red_df)
  
}


#' @title Obtain meta data
#'
#' @inherit check_object params
#'
#' @return The data.frame of interest. 
#' @export
#'

getMeta <- function(object){
  
  object@data$meta
  
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

getStats <- function(object,
                     with_meta = TRUE,
                     with_cluster = TRUE,
                     phase = "first_tmt"){
  
  stat_df <-
    getData(object = object,
            data_slot = "stats", 
            phase = phase)
  
  if(base::isTRUE(with_meta)){
    
    stat_df <- 
      join_with_meta(object = object, df = stat_df)
    
  } 
  
  if(base::isTRUE(with_cluster)){
    
    stat_df <- 
      dplyr::left_join(
        x = stat_df, 
        y = getClusterData(object, phase = phase), 
        by = "cell_id"
      )
    
  }
  
  base::return(stat_df)
  
}



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

getTracks <- function(object,
                      with_meta = TRUE,
                      with_cluster = TRUE,
                      phase = "all",
                      phase_cluster = "first_tmt",
                      verbose = TRUE){
  
  
  # check if input for phase-arguments makes sense
  
  phase <-
    check_phase(object, phase = phase)
  
  if(time_displaced_tmt(object) & base::isTRUE(with_cluster)){
    
    if(base::length(phase_cluster) > 1){
      
      base::stop("To avoid ambiguous clusternames binding the data with clusters of more than one phase is not allowed.")
      
    }
    
    phase_cluster <- 
      check_phase(object, phase = phase_cluster)
    
    ref_phase <- 
      stringr::str_c(phase, collapse = "', '")
    
    if(!phase_cluster %in% phase){
      
      base::warning(glue::glue("You are adding cluster variables derived from data of phase '{phase_cluster}' to tracking data of phase(s) '{ref_phase}'."))
      
    } else if(!base::all(phase %in% phase_cluster)){
      
      if(base::isTRUE(verbose)){
        
        base::message(glue::glue("Note: The added clustering variables base on the data of phase '{phase_cluster}'. The tracking data includes phase(s) '{ref_phase}'. "))
      
      }
      
    }
    
  }
  
  track_df <- 
    getData(object = object, 
            data_slot = "tracks", 
            phase = phase)
  
  if(base::isTRUE(with_meta)){
    
    track_df <- 
      join_with_meta(object = object, df = track_df)
    
  } 
  
  if(base::isTRUE(with_cluster)){

    if(!time_displaced_tmt(object)){
      
      phase_cluster <- phase
      
    }
    
    track_df <- 
      dplyr::left_join(
        x = track_df, 
        y = getClusterData(object, phase = phase_cluster), 
        by = "cell_id"
      )
    
  }
  
  base::return(track_df)          
  
}


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


#' @title Obtain across input options
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
#'

getAcrossOptions <- function(object, phase = "first_tmt"){
  
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
  
  getStats(object, with_meta = FALSE, with_cluster = FALSE) %>% 
    dplyr::select(-cell_id) %>% 
    base::colnames()
  
}


#' @title Obtain well plate names 
#'
#' @inherit check_object params
#'
#' @return A character vector. 
#' @export

getWellPlateNames <- function(object){
  
  object@wp_info %>% base::names()
  
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
  
  getMeta(object) %>% 
    dplyr::pull(var = "cell_line") %>% 
    base::unique()
  
}

#' @rdname getCellLines
#' @export
#' 
getConditions <- function(object){
  
  getMeta(object) %>% 
    dplyr::pull(var = "condition") %>% 
    base::unique()
  
}


# Not exported ---

getNumericVariableNames <- function(object){
  
  getStats(object = object) %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::colnames()
  
}

getCategoricalVariablesNames <- function(object){
  
  getStats(object = object) %>% 
    dplyr::select_if(.predicate = base::is.factor) %>% 
    base::colnames()
  
}

getFrameSeq <- function(object, phase = "all"){
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_num") %>% 
    base::unique()
  
}

getFrameTimeSeq <- function(object, phase = "all"){
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}


getGroupingOptions <- function(object){
  
  groups <- 
    getMeta(object) %>% 
    base::colnames()
  
  #!!! add cluster options
  
  valid_groups <-
    groups[!groups %in% invalid_groups]
  
  base::return(valid_groups)
  
}

getGroups <- function(object, option){
  
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

getInterval <- function(object){
  
  object@set_up$itvl
  
}

getIntervalUnit <- function(object){
  
  object@set_up$itvl_u
  
}

getPhases <- function(object){
  
  object@data$tracks %>% base::names()
  
}

getVariableNames <- function(object,
                             phase = "first_tmt",
                             variable_classes = c("cluster", "input", "well_plate", "stats"),
                             algorithm_subset = NULL, 
                             flatten = TRUE){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  select_list <- list()
  
  # cluster
  if("cluster" %in% variable_classes){
    
    select_list$Cluster <- 
      getClusterNames(object,
                      phase = phase,
                      algorithm_subset = algorithm_subset) %>% 
      purrr::flatten() %>% 
      purrr::flatten_chr()
    
  }
  
  # meta input
  
  if("input" %in% variable_classes){
    
    select_list$Meta <- 
      getGroupingOptions(object)
    
  }
  
  # meta well
  
  if("well_plate" %in% variable_classes){
    
    select_list[["Well Plate"]] <- 
      getMeta(object) %>% 
      dplyr::select(dplyr::starts_with(match = "well"), -well_plate_index) %>% 
      base::colnames()
    
  }
  
  # stats
  
  if("stats" %in% variable_classes){
    
    select_list[["Cell Statistics"]] <- 
      getStatVariableNames(object)
    
  }
  
  # ...
  
  if(base::isTRUE(flatten) & base::length(select_list) == 1){
    
    select_list <- purrr::flatten_chr(select_list)
    
  }
  
  base::return(select_list)
  
}

