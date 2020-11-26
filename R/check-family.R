#' @title Detect missing variables
#' 
#' @description Returns \code{variable} if it was not detected in the column names 
#' of the specified data.frame. To be used in \code{purrr::map()} with argument \code{.x}
#' set to \code{original_ct_variables}
#' @param variable Character value. The variable to be checked.
#' @param df A read in cell track data.frame.
#'

check_cell_track_variables <- function(variable, df){
  
  cnames <- base::colnames(df)
  
  if(variable %in% cnames){
    
    NULL
    
  } else if(base::any(stringr::str_detect(cnames, pattern = variable))){
    
    NULL
    
  } else {
    
    base::return(variable)
    
  }
  
}


#' @title Check phase input 
#' 
#' @inherit check_object params
#' @param phase A character value referring to the phase. 
#' @param max_phase Numeric value or NULL. If numeric it regulates the maximal number of phases allowed. 
#' 
check_phase <- function(object, phase, max_phases = NULL){
  
  if(!time_displaced_tmt(object)){
    
    phase <- "entire"
    
  } else {
    
    
    if(base::all(phase == "all")){
      
      phase <- getPhases(object)
      
    }
    
    if(base::is.numeric(max_phases)){
      
      base::stopifnot(base::length(phase) <= base::length(max_phases))
      
    }
    
    
    phase <- 
      confuns::check_vector(
        input = phase, 
        against = getPhases(object), 
        verbose = TRUE, 
        ref.input = "input for argument 'phase'", 
        ref.against = "valid phases"
      )
    
  }
  
  base::return(phase)
  
}


#' @title Check track data.frame 
#'
#' @param track_df A data.frame in which each observation refers to a cell at a given frame and 
#' that contains the following variables: 
#' 
#'  \describe{
#'   \item{\emph{cell_id}}{Character. Refers to the cell id's.}
#'   \item{\emph{x_coords}}{Numeric. Refers to the cell's x-coordinates.}
#'   \item{\emph{y_coords}}{Numeric. Refers to the cell's y-coordinates.}
#'   \item{\emph{dfo}}{Numeric. The distances from the first position tracked. (Distance from origin).}
#'   \item{\emph{dfpl}}{Numeric. The distances from the last point.}
#'   \item{\emph{speed}}{Numeric. Refers to the instantaneuous speed.}
#'   \item{\emph{afo}}{Numeric. The angle from the position of origin.}
#'   \item{\emph{aflp}}{Numeric. The angle from the last position.}
#'   \item{\emph{frame_num}}{Numeric. The frame number the observation refers to.}
#'   \item{\emph{frame_time}}{Numeric. The frame number multiplied with the interval between two frames}
#'   \item{\emph{frame_itvl}}{Character. The frame time combined with the intervals unit}
#'   }
#'
#' @return
#' @export
#'

check_track_df <- function(track_df){}


#' @title Check well plate data.frame (shiny)
#'
#' @description Makes sure that the well plate data.frame's information 
#' status is sufficient to proceed with data loading. 
#'
#' @return A named list containing the argument input for \code{checkpoint()}.

check_wp_df_shiny <- function(wp_df){
  
  result_list <- list()
  
  # check conditions for unknown
  if(!base::any(stringr::str_detect(string = wp_df$cl_condition, pattern = "unknown"))){
    
    result_list$evaluate <- TRUE
    
  } else if(base::any(wp_df$condition == "unknown")) {
    
    result_list$evaluate <- FALSE
    result_list$case_false <- "There are still unknown conditions left. Please add respective information or enable 'Discard unknown'."
    
  } else if(base::any(wp_df$cell_line == "unknown")){
    
    result_list$evaluate <- FALSE
    result_list$case_false <- "There are still unknown cell lines left. Please add respective information or enable 'Discard unknown"
  }
  
  return(result_list)
  
}

#' @title Detect double directories
#'
#' @param all_wp_lists A list of well plate lists.
#'
#' @return A character value that contains either \emph{'unique'} or the 
#' directories that are not unique. 

check_wp_directories <- function(all_wp_lists){
  
  all_wp_lists <- 
    purrr::discard(.x = all_wp_lists, 
                   .p = ~ base::is.null(.x[["directory"]]))
  
  count_dirs <- 
    purrr::map_chr(.x = all_wp_lists, "directory") %>% 
    base::table() %>% 
    base::as.data.frame() %>% 
    magrittr::set_colnames(value = c("dir", "count"))
  
  if(base::any(count_dirs$count != 1)){
    
    return_value <- 
      dplyr::filter(.data = count_dirs, count != 1) %>% 
      dplyr::pull(var = "dir") %>% 
      stringr::str_c(collapse = "', '")
    
  } else {
    
    return_value <- "unique"
    
  }
  
  base::return(return_value)
  
}


#' @title Check well plate name
#' 
#' @inherit check_object params
#' @param well_plate Character value. The name of the well plate of interest. Valid inputs can be obtained 
#' via \code{getWellPlateNames()}.
#' 
check_wp_name <- function(object, well_plate){
  
  wp_names <- getWellPlateNames(object)
  
  if(base::all(well_plate == "")){
    
    well_plate <- wp_names[1]
    
    if(base::length(wp_names) > 1){
      
      base::message(glue::glue("Defaulting to first well plate found: '{wp_names[1]}'."))
      
    }
    
  } else {
    
    well_plate <- confuns::check_vector(input = well_plate, 
                                        against = wp_names, 
                                        verbose = TRUE, 
                                        ref.input = "input for argument 'well_plate'",
                                        ref.against = "valid well plate names")
    
  }
  
  base::return(well_plate)
  
}




