#' @title Shiny feedback messages
#'
#' @description Wrapper around \code{shiny::req()} and \code{shiny::showNotification()}.
#' Prevents application from crashing and displays guiding message about what the user
#' is supposed to do in order to continue without this message to appear.
#'
#' @param evaluate A vector of logical tests to be evaluated.
#' @param case_false A character string indicating the message to be displayed if one element of
#' \code{evaluate} turns out to be FALSE. Needs to be in \code{base::names(\code{error/warning_notifiations})}.
#' @param error_notifications A named list of character strings.
#' @param warning_notifications A named list of character strings.
#' @param duration The duration the message is displayed.
#' @param stop_process,stop_app Logical. What is supposed to happen if one element of \code{evaluate}
#' turns out to be FALSE.
#'
#' @return A shiny notification.
#'
checkpoint <- function(evaluate = TRUE,
                       case_false = NULL,
                       error_notifications = list(
                         
                         # experiment set up
                         "no_well_plate_chosen" = "Please choose a well plate first.",
                         "invalid_image_number" = "The number of images per well must not be 0.",
                         "invalid_wp_name" = "Invalid or already existing well plate name.",
                         "missing_software" = "Please choose the software from which the files derived.", 
                         "no_well_plates_added" = "No well plates have been added yet.",
                         
                         # load data
                         "no_set_up_saved" = "No experiment set up has been saved yet.",
                         "well_plates_not_ready" = "There are still well plates left that do not match the requirements to be loaded.",
                         "no_data_read_in" = "No files have been loaded yet.",
                         "no_cells_remaining" = "The filter criteria discard all cells. At least one cell must remain.", 
                         "errors_left" = "Trying to load some files resulted in errors. To proceed without them enable 'Ignore Errors'. If you want the files to be included fix the error and load the files again.",
                         
                         # quality check 
                         "no_qc_data" = "Data has to undergo quality check first before the cell tracer object can be returned.",
                         
                         # descriptive / categorical statistics
                         "no_features_selected" = "Please selecte at least one feature."
                         
                       ),
                       warning_notifications = list(),
                       duration = 4,
                       stop_process = TRUE,
                       stop_app = FALSE){
  
  ##-- check if truthy for all elements
  results <- shiny::isTruthy(evaluate)
  
  if(any(results == F)){##-- at least one of the elements is not truthy
    
    if(!is.null(case_false) & case_false %in% names(warning_notifications)){
      
      ##-- show notification
      shiny::showNotification(ui = warning_notifications[[case_false]], duration = duration, closeButton = T, type = "warning")
      
    } else if(!is.null(case_false) & case_false %in% names(error_notifications)){
      
      ##-- show notification
      shiny::showNotification(ui = error_notifications[[case_false]], duration = duration, closeButton = T, type = "error")
      
      ##-- stop computation and or stop app?
      if(isFALSE(stop_app) & isTRUE(stop_process)){
        
        shiny::req(evaluate)
        
      } else if(isTRUE(stop_app)) {
        
        shiny::stopApp()
        
      }
      
    }
    
  }
  
}
