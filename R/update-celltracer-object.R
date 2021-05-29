


#' @title Keep cypro object up to data
#' 
#' @description Ensures that the cypro object is constructed according
#' to the latest version of the package. 
#'
#' @inherit argument_dummy
#'
#' @return A cypro object whose architecture is up to date with the latest version.
#' @export
#'
updateCyproObject <- function(object){
  
  stopifnot(base::class(object) == "cto")
  
  # include old Cypro objects
  all_slot_names <- methods::slotNames(object)
  
  content <- 
    purrr::map(all_slot_names, .f =purrr::safely(~methods::slot(object, name = .x))) %>% 
    purrr::set_names(nm = all_slot_names)
  
  success <- 
    purrr::keep(.x = content, .p = ~ base::is.null(.x[["error"]]))
  
  success_names <- base::names(success)
  
  errors <- 
    purrr::keep(.x = content, .p = ~ !base::is.null(.x[["error"]]))
  
  error_names <- base::names(errors)
  
  if(!shiny::isTruthy(error_names)){
    
    version <- object@version
    
  } else if(base::all(new_slots %in% error_names)){
    
    version <- "old"
    
  } else {
    
    version <- "alpha-development"
    
  }
  
  # updating 
  new_object <- methods::new(Class = "cto")
  
  if(base::identical(version, current_version)){
    
    stop("Updating not possible. Input object already has been updated to the newest version.")
    
  } else if(version == "old"){
    
    stop("Updating not possible. Please initiate the cypro object again.")
    
  } else if(version == "alpha-development"){
    
    for(slot in success_names){
      
      methods::slot(new_object, name = slot) <- 
        methods::slot(object, name = slot)
      
      new_object@version <- current_version
      
    }
    
  }
  
  # return
  base::return(new_object)
  
}