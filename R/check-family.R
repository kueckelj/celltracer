

#' @title Check content availability 
#'
#' @param evaluate The test that must be TRUE. 
#' @inherit argument_dummy params
#' @param ref_input glue reference
#' @param ref_fun glue reference
#'
check_availability <- function(evaluate, phase, ref_input, ref_fun){
  
  ce <- rlang::caller_env()
  
  object <- base::parse(text = "object") %>% base::eval(envir = ce)
  
  if(!base::isTRUE(evaluate)){
    
    msg <- glue::glue("Could not find {ref_input}{ref_phase}. You might have to use function '{ref_fun}' first.",
                      ref_phase = hlpr_glue_phase(object, phase, FALSE))
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  
}

#' @title Check object input
#' 
#' @description Makes sure that object input is of class cypro and 
#' that it contains the relevant information needed for the function.
#' @param object A valid cypro object. 
check_object <- function(object, experiment = NULL, set_up_req = "process_data", exp_type_req = NULL, module_req = NULL){
  
  input_class <- base::class(object)
  input_attribute <- base::attr(input_class, "package")
  
  if(!input_class == object_class & input_attribute == "cypro"){
    
    msg <- "Input for argument 'object' must be of class 'cto' from package 'cypro'."
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  if(!base::isTRUE(object@set_up$progress[[set_up_req]])){
    
    missing_fun <- set_up_funs[[set_up_req]]
    
    msg <- glue::glue("Seems like you've missed some object processing steps. Make sure to run '{missing_fun}' first and then try again.")
    
    base::stop(msg)
    
  }
  
  if(base::is.character(exp_type_req)){
    
    if(exp_type_req == "time_lapse"){
      
      if(!isTimeLapseExp(object)){
        
        msg <- "This function requires a time lapse experiment set up."
        
        base::stop(msg)
        
      }
      
    }
    
  }
  
  
  if(base::is.character(module_req)){
    
    if(!isUsable(object, module = module_req)){
      
      msg <- glue::glue("This functions requires specific module usability. Module: '{module_req}'")
      
      base::stop(msg)
      
    }
    
  }
  
  base::invisible(TRUE)
  
}


#' @title Check phase input 
#' 
#' @inherit check_object params
#' @param phase Character value. The ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
#' referring to the phase of interest. If set to NULL takes the phase denoted as default with \code{adjustDefault()}.
#' 
#' Ignored if the experiment design contains only one phase. 
#' 
#' @param max_phase Numeric value or NULL. If numeric it regulates the maximal number of phases allowed. 
#' 
check_phase <- function(object, phase, max_phases = NULL){
  
  if(multiplePhases(object)){
    
    if(base::is.numeric(phase)){
      
      lp <- base::length(getPhases(object))
      
      confuns::is_vec(phase, mode = "numeric", max.length = lp)
      
      if(base::max(phase) > lp){
        
        base::stop(glue::glue("Input for argument 'phase' must not exceed the number of phases of the experiment which is {lp}."))
        
      }
      
      phase <- base::names(object@set_up$phases)[phase]
      
    }
    
    
    if(base::is.numeric(max_phases)){
      
      if(max_phases == 1){
        
        confuns::is_vec(x = phase, mode = "character", of.length = max_phases)
        
      } else {
        
        confuns::is_vec(x = phase, mode = "character", max.length = max_phases)
        
      }
      
    }
    
    if(base::all(phase == "all") & base::is.null(max_phases)){
      
      phase <- getPhases(object)
      
    } else if(base::is.numeric(max_phases) && base::length(phase) > max_phases){
      
      msg <- glue::glue("Length of input for argument phase must be equal to or lower than {max_phases}.")
      
      confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
      
    } else if(base::all(phase == "all") && base::is.numeric(max_phases) && base::length(getPhases(object) > max_phases)){
      
      msg <- glue::glue("Length of input for argument phase must be equal to or lower than {max_phases}. All phases sum up to {base::length(getPhases(object))}.")
      
      confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
      
    } else {
      
      confuns::check_one_of(
        input = phase, 
        against = getPhases(object)
      )
      
    }
    
  }
  
  base::return(phase)
  
}


#' @title Check that phase is specified manually
check_phase_manually <- function(object, phase = NULL){
  
  if(multiplePhases(object)){
    
    if(base::is.null(phase)){
      
      base::stop("Creating a subset of your data impacts downstream analysis. Please specify argument 'phase' manually to ensure that you are referring to the right one.")
      
    }
    
  }
  
}

#' @title Detect missing variables
#' 
#' @description Returns \code{variable} if it was not detected in the column names 
#' of the specified data.frame. To be used in \code{purrr::map()} with argument \code{.x}
#' set to \code{original_ct_variables}
#' @param variable Character value. The variable to be checked.
#' @param df A read in cell track data.frame.
#'

check_df_variables <- function(variable, df){
  
  cnames <- base::colnames(df)
  
  if(variable %in% cnames){
    
    NULL
    
  } else if(base::any(stringr::str_detect(cnames, pattern = variable))){
    
    NULL
    
  } else {
    
    base::return(variable)
    
  }
  
}


#' @title Detect protected variables
#' @description Makes sure that renaming variables does not conflict with 
#' protected variable names,
check_renamed_variables <- function(cnames){
  
  found_pos <- base::which(protected_vars %in% cnames)
  
  found_vars <- protected_vars[found_pos]
  
  if(base::length(found_vars) >= 1){
    
    msg <- 
      glue::glue(
        "'{ref_found_vars} {ref1} invalid new {ref2}.", 
        ref_found_vars = glue::glue_collapse(x = found_vars, sep = "', '", last = "' and '"), 
        ref1 = confuns::adapt_reference(found_vars, "is an", "are"), 
        ref2 = confuns::adapt_reference(found_vars, "name", "names")
      )
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  } 
  
  
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
    result_list$case_false <- "There are still missing or incomplete wells left. Please add respective information or enable 'Empty Wells'."
    
  } else if(base::any(wp_df$cell_line == "unknown")){
    
    result_list$evaluate <- FALSE
    result_list$case_false <- "There are still missing or incomplete wells left. Please add respective information or enable 'Empty Wells"
    
  }
  
  return(result_list)
  
}

#' @title Detect double directories
#'
#' @param well_plate_list A list of well plate lists.
#'
#' @return A character value that contains either \emph{'unique'} or the 
#' directories that are not unique. 

check_wp_directories <- function(well_plate_list){
  
  well_plate_list <- 
    purrr::discard(.x = well_plate_list, 
                   .p = ~ base::is.null(.x[["directory"]]))
  
  count_dirs <- 
    purrr::map_chr(.x = well_plate_list, "directory") %>% 
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




