
# Helper ------------------------------------------------------------------
join_with_meta <- function(object, df, phase){
  
  dplyr::left_join(x = df, y = purrr::map_df(phase, .f = ~ object@data$meta[[.x]]), by = "cell_id")
  
}

# -----


# Analysis extraction ------------------------------------------------------

#' @title Obtain cypros clustering objects
#'
#' @inherit argument_dummy params 
#'
#' @return An S4 object of \emph{'hclust_conv'}, \emph{'kmeans_conv'} or \emph{'pam_conv'}.
#' @export
#'
getHclustConv <- function(object, variable_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase, max_phase = 1)
    
    cluster_object <- object@analysis$clustering$hclust[[variable_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$hclust[[variable_set]]
    
  }
  
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
getHclustObject <- getHclustConv

#' @rdname getHclustConv
#' @export
getKmeansConv <- function(object, variable_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$kmeans[[variable_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$kmeans[[variable_set]]
    
  }
  
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
getKmeansObject <- getKmeansConv

#' @rdname getHclustConv
#' @export
getPamConv <- function(object, variable_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$pam[[variable_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$pam[[variable_set]]
    
  }
  
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

#' @rdname getHclustConv
#' @export
getPamObject <- getPamConv


#' @title Obtain cypros correlation objects
#'
#' @inherit argument_dummy params
#'
#' @return An S4 object of class \emph{'corr_conv'}
#' @export
#'
getCorrConv <- function(object, variable_set, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    corr_object <- object@analysis$correlation[[variable_set]][[phase]]
    
  } else {
    
    corr_object <- object@analysis$correlation[[variable_set]]
    
  }
  
  check_availability(
    evaluate = !base::is.null(corr_object) & base::class(corr_object) == "corr_conv",
    phase = phase, 
    ref_input = "correlation object", 
    ref_fun = "initiateCorrelation()"
  )
  
  corr_object@meta <- 
    dplyr::left_join(
      x = corr_object@meta, 
      y = getGroupingDf(object, phase = phase, verbose = FALSE), 
      by = c("key" = "cell_id"))
  
  corr_object@data <- 
    getStatsDf(object, phase = phase, verbose = FALSE) %>% 
    tibble::column_to_rownames(var = "cell_id") %>% 
    dplyr::select(dplyr::all_of(corr_object@variables_num)) %>% 
    base::as.matrix()
  
  corr_object@variables_discrete <- getGroupingVariableNames(object, phase = phase)
  
  base::return(corr_object)
  
}

#' @rdname getCorrConv
#' @export
getCorrObject <- getCorrConv


# -----



# Cell Ids ----------------------------------------------------------------

#' @title Get cell ids
#' 
#' @description Allows to filter cell ids according to a variety of aspects.
#'
#' @inherit argument_dummy params 
#' @param na_max Numeric value. Sets the threshold of the maximum number of missing values 
#' a cell can have across all variables. Ignored if set to NULL or if the experiment 
#' is not of type \emph{timelapse}.
#'
#' @return Character vector of cell ids.
#' @export
#'

getCellIds <- function(object, ...,  na_max = Inf, phase = NULL){
  
  filtering <- rlang::enquos(...)
  
  check_object(object)
  assign_default(object)
  
  cell_ids <- 
    getStatsDf(object, verbose = FALSE) %>% 
    dplyr::pull(cell_id)
  
  if(isTimeLapseExp(object) & base::is.numeric(na_max)){
    
    confuns::is_value(na_max, mode = "numeric")
    
    cell_ids <-
      getMissingValuesMax(object) %>% 
      dplyr::filter(max_nas <= {{na_max}}) %>% 
      dplyr::pull(cell_id)
    
  }

  
  cell_ids <- 
    getStatsDf(object, phase = phase) %>% 
    dplyr::filter(cell_id %in% {{cell_ids}}) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id)
  
  base::return(cell_ids)
  
}


# Data extraction ---------------------------------------------------------


getCellDf <- function(object, slot, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    object@cdata[[slot]][[phase]]
    
  } else {
    
    object@cdata[[slot]]
    
  }
  
}


#' @title Obtain grouping information
#' 
#' @description These functions let you extract a data.frame that contain variables 
#' with which cells are grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame that contains the cell ids and their group belonging.
#' @export
#'

getGroupingDf <- function(object, phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  grouping_df <- 
    dplyr::left_join(x = getMetaDf(object, phase = phase),
                     y = getClusterDf(object, phase = phase, verbose = verbose), 
                     by = "cell_id") %>% 
    dplyr::left_join(x = .,
                     y = getWellPlateDf(object), 
                     by = "cell_id")
  
  base::return(grouping_df)
  
}

#' @rdname getGroupingDf
#' @export
getClusterDf <- function(object, phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_df <- object@cdata$cluster[[phase]]
    
  } else {
    
    cluster_df <- object@cdata$cluster
    
  }
  
  if(base::ncol(cluster_df) == 1){
    
    if(multiplePhases(object)){
      
      add <- glue::glue(" for {phase} phase.")
      
    } else {
      
      add <- "."
      
    }
    
    msg <- glue::glue("No cluster variables have been calculated yet{add}")
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
  } 
  
  base::return(cluster_df)
  
}

#' @rdname getGroupingDf
#' @export
getMetaDf <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    meta_df <- object@cdata$meta[[phase]]
    
  } else {
    
    meta_df <- object@cdata$meta
    
  }
  
  base::return(meta_df)
  
}

#' @rdname getGroupingDf
#' @export
getWellPlateDf <- function(object){
  
  base::return(object@cdata$well_plate)
  
}

#' @title Obtain stat data.frame 
#'
#' @inherit argument_dummy params
#' @param with_cluster,with_meta,with_well_plate Logical values. Denoting 
#' if the respective grouping information should be joined to the stats data.frame
#' or not.
#'
#' @return A data.frame with all numeric variables summarizing the measurements of 
#' the track data.frame. 
#' 
#' @export
#'

getStatsDf <- function(object,
                       phase = NULL,
                       with_grouping = NULL,
                       with_cluster = NULL,
                       with_meta = NULL,
                       with_well_plate = NULL, 
                       verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::isFALSE(with_grouping)){
    
    with_cluster <- FALSE
    with_meta <- FALSE
    with_well_plate <- FALSE
    
  }
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase, max_phases = 1)
    
    stat_df <- object@cdata$stats[[phase]]
    
  } else {
    
    stat_df <- object@cdata$stats
    
  }

  
  # add cluster
  if(base::isTRUE(with_cluster) | base::isTRUE(with_grouping)){
    
    cluster_df <- getClusterDf(object, phase = phase, verbose = FALSE)  
    
    stat_df <- 
      dplyr::left_join(x = stat_df, y = cluster_df, by = "cell_id")
    
  }
  
  # add meta
  if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
    
    meta_df <- getMetaDf(object, phase = phase)
    
    stat_df <- dplyr::left_join(x = stat_df, y = meta_df, by = "cell_id")
    
  }
  
  # add well plate info
  if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
    
    wp_df <- getWellPlateDf(object)
    
    stat_df <- dplyr::left_join(x = stat_df, y = wp_df, by = "cell_id")
    
  }
  
  base::return(stat_df)  
  
}



#' @title Obtain track data.frame. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation refers to a cell at a given frame.
#' 
#' @export
#'

getTracksDf <- function(object,
                        phase = NULL,
                        with_grouping = NULL, 
                        with_cluster = NULL,
                        with_meta = NULL,
                        with_well_plate = NULL,
                        verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::isFALSE(with_grouping)){
    
    with_cluster <- FALSE
    with_meta <- FALSE
    with_well_plate <- FALSE
    
  }
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase)
    
    track_df_final <- purrr::map_df(
      .x = phase, 
      .f = function(p){
        
        track_df <- object@cdata$tracks[[p]]
        
        if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
          
          meta_df <- getMetaDf(object, phase = p)
          
          track_df <- dplyr::left_join(x = track_df, y = meta_df, by = "cell_id")
          
        }
        
        base::return(track_df)
        
      }
    ) %>% dplyr::arrange(cell_id)
    
  } else {
    
    track_df_final <- object@cdata$tracks
    
    if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
      
      meta_df <- getMetaDf(object, phase = phase)
      
      track_df_final <- dplyr::left_join(x = track_df_final, y = meta_df, by = "cell_id")
      
    }
    
  }

  
  if((base::isTRUE(with_cluster) | base::isTRUE(with_grouping)) & base::length(phase) == 1){
    
    cluster_df <- getClusterDf(object, phase = phase, verbose = FALSE)
    
    track_df_final <- dplyr::left_join(x = track_df_final, y = cluster_df, by = "cell_id")
    
  }
  
  if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
    
    track_df_final <- dplyr::left_join(x = track_df_final, y = getWellPlateDf(object), by = "cell_id")
    
  }
  
  base::return(track_df_final)
  
}


#' @title Obtain well plate set up 
#' 
#' @description Access function to the experiment set up in a tidy-data fashion. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation represents the well of a well plate
#' @export
#'
getSetUpDf <- function(object, well_plate_name = NULL){
  
  check_object(object)
  
  if(base::is.null(well_plate_name)){
    
    well_plate_name <- getWellPlateNames(object)[1]
    
  }
  
  confuns::check_one_of(input = well_plate_name, against = getWellPlateNames(object))
  
  set_up_df <- object@well_plates[[well_plate_name]]$wp_df_eval
  
  base::return(set_up_df)
  
}


#' @title Obtain variable centered summarys
#' 
#' @description Acces function for the data.frame that contains summary information 
#' of all numeric data variables. 
#' 
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
getVariableDf <- function(object, variable_subset = NULL, phase = NULL){

  check_object(object)
  assign_default(object)
    
  vdata <- object@vdata$summary
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    vdata <- vdata[[phase]]
    
  }
  
  if(base::is.character(variable_subset)){
    
    confuns::check_one_of(
      input = variable_subset, 
      against = vdata$variable
    )
    
    vdata <- dplyr::filter(vdata, variable %in% {{variable_subset}})
    
  }
  
  base::return(vdata)
  
}

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







# Missing values ----------------------------------------------------------



#' @title Obtain missing value counts
#' 
#' @description This function returns a data.frame giving insight into 
#' the number of missing values every cell has across all variables. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
getMissingValuesDf <- function(object){
  
  check_object(object, exp_type_req = "timelapse")
  
  object@information$track_na_count
  
}


#' @title Obtain missing value counts
#' 
#' @description This function returns the maximum number of missing values 
#' every cell has across all variables. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
getMissingValuesMax <- function(object){
  
  check_object(object, exp_type_req = "timelapse")
  
  df <- hlpr_max_track_na_count(object)
  
}




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
    
    outlier_list <- outlier_list[[method_outlier]]
    
  }
  
  base::return(outlier_list)
  
}

#' @rdname getOutlierResults
#' @export
getOutlierIds <- function(object, method_outlier = NULL, check = FALSE, flatten = TRUE){
  
  if(base::is.null(method_outlier)){
    
    method_outlier <- base::names(object@analysis$outlier_detection)
    
  }
  
  outlier_ids <- list()
  
  if("iqr" %in% method_outlier){
    
    outlier_list <- getOutlierResults(object, check = check, method_outlier = "iqr")
    
    outlier_ids$iqr <-  
      purrr::flatten(outlier_list) %>% 
      purrr::flatten_chr() %>% 
      base::unique()
    
  }
  
  if("mahalanobis" %in% method_outlier){
    
    outlier_list <- getOutlierResults(object, check = check, method_outlier = "mahalanobis")
  
    outlier_ids$mahalanobis <- 
      outlier_list$outlier_ids
      
  }
  

  if(base::isTRUE(flatten)){
    
    outlier_ids <- 
      purrr::flatten_chr(.x = outlier_ids) %>% 
      base::unique()
    
  }

  
  base::return(outlier_ids)
  
}


# -----




# Names -------------------------------------------------------------------

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

getGroupNames <- function(object, grouping_variable, ..., phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  group_vec <- 
    getGroupingDf(object = object, phase = phase, verbose = FALSE) %>% 
    dplyr::pull(var = {{grouping_variable}}) 
  
  if(base::is.factor(group_vec)){
    
    group_vec <- base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    group_vec <- base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping variable '{option}' must be a character vector or a factor.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
  res <- confuns::vselect(input = group_vec, ...)
  
  base::return(res)
  
}


#' @title Obtain grouping variable names of cell data
#' 
#' @description Convenient access to the names of your objects data variables. Useful to 
#' obtain vectors of variable names as input for recurring arguments like \code{across} or
#' \code{grouping_variable}.
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

getGroupingVariableNames <- function(object, ..., named = FALSE, phase = NULL, verbose = TRUE){
  
  check_object(object)
  
  assign_default(object)
  
  group_df <- 
    getGroupingDf(object, phase = phase, verbose = verbose) %>% 
    dplyr::select(-cell_id)
  
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
  
  all_var_names <- 
    base::colnames(selected_df)
  
  if(base::isTRUE(named)){
    
    sources <- base::vector("character", base::length(all_var_names))
    
    cluster_names <- getClusterVariableNames(object, phase = phase)
    
    meta_names <- getMetaVariableNames(object, phase = phase)
    
    wp_names <- getWellPlateVariableNames(object)
    
    for(i in base::seq_along(all_var_names)){
      
      var <- all_var_names[i]
      
      if(var %in% cluster_names){
        
        sources[i] <- "cluster"
        
      } else if(var %in% meta_names){
        
        sources[i] <- "meta"
        
      } else if(var %in% wp_names){
        
        sources[i] <- "well_plate"
        
      }
      
    }
    
    base::names(all_var_names) <- sources
    
  }
  
  base::return(all_var_names)
  
}

#' @rdname getGroupingVariableNames
#' @export
getClusterVariableNames <- function(object, ..., phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  cluster_df <- 
    getClusterDf(object, phase = phase) %>% 
    dplyr::select(-cell_id)
  
  if(base::ncol(cluster_df) == 0){
    
    base::return(base::character(0L))
    
  }
  
  selected_df <- dplyr::select(cluster_df, ...)
  
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
      
      selected_df <- cluster_df
      
    }
    
  }
  
  all_var_names <- 
    base::colnames(selected_df)
  
  base::return(all_var_names)
  
}

#' @rdname getGroupingVariableNames
#' @export
getMetaVariableNames <- function(object, ..., phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  meta_df <- 
    getMetaDf(object, phase = phase) %>% 
    dplyr::select(-cell_id)
  
  selected_df <- dplyr::select(meta_df, ...)
  
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
      
      selected_df <- meta_df
      
    }
    
  }
  
  all_var_names <- 
    base::colnames(selected_df)
  
  base::return(all_var_names)
  
}

#' @rdname getGroupingVariableNames
#' @export
getWellPlateVariableNames <- function(object, ...){
  
  confuns::vselect(input = well_plate_vars, ...)
  
} 


#' @title Obtain numeric variables of cell data
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

getStatVariableNames <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  stat_df <-
    getStatsDf(object, with_meta = FALSE, with_cluster = FALSE, with_well_plate = FALSE) %>% 
    dplyr::select(-cell_id)
  
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


#' @rdname getStatVariableNames
#' @export
getTrackVariableNames <- function(object, ..., phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  track_df <-
    getTracksDf(object, with_meta = FALSE, with_cluster = FALSE, with_well_plate = FALSE) %>% 
    dplyr::select(-cell_id, -dplyr::all_of(x = non_data_track_variables))
  
  selected_df <- dplyr::select(track_df, ...)
  
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
      
      selected_df <- track_df
      
    }
    
  }
  
  
  stat_variable_names <- 
    base::colnames(selected_df)
  
  base::return(stat_variable_names)
  
  
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



#' @title Obtain cell line and condition names 
#' 
#' @description Quick wrapper around the functionality of getGroupingVariableNames().
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
  
  getMetaDf(object, phase = phase) %>% 
    dplyr::pull(condition) %>% 
    base::levels()
  
}



#' @title Obtain storage directory
#' 
#' @inherit argument_dummy params
#' 
#' @return Character value. 
#' 
getStorageDirectory <- function(object){
  
  check_object(object, set_up_req = "experiment_design")
  
  dir <- object@information$directory_cto
  
  if(base::is.null(dir) | dir == "not defined yet"){
    
    base::stop("Storage directory has not beend defined yet.")
    
  } else if(!stringr::str_detect(dir, pattern = "\\.{1}RDS")){
    
    base::stop("Storage directory must end with '.RDS'.")
    
  } else {
    
    base::return(dir)
    
  }
  
}




# NOT EXPORTED ------------------------------------------------------------


#' @title Helper functions to extract information from the
#'
#' @return

getCategoricalVariablesNames <- function(object, phase = NULL){
  
  warning("getCategoricalVariablesNames() is deprecated.")
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getStats(object = object, phase = phase) %>% 
    dplyr::select_if(.predicate = base::is.factor) %>% 
    base::colnames()
  
}

#' @rdname getCategoricalVariablesNames
getNumericVariableNames <- function(object){
  
  warning("getNumericVariableNames() is deprecated.")
  
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
  
  phase <- check_phase(object, phase = phase)
  
  getTracksDf(object, phase = phase) %>% 
    dplyr::arrange(frame_num) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}

#' @rdname getCategoricalVariablesNames
getFrameLevels <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  getTracksDf(object, phase = phase) %>% 
    dplyr::arrange(frame_num) %>% 
    dplyr::pull(frame_itvl) %>% 
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
  
  warning("getVariableNames() is deprecated.")
  
  
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




