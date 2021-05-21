

mergeCelltraceObjects <- function(..., verbose = TRUE){
  
  
  object_list <- list(...)
  
  # keep only celltracer objects 
  object_list <- purrr::keep(.x = object_list, .p = ~ methods::is(.x, "celltracer"))
  
  # name list by object name 
  object_names <- purrr::map(.x = object_list, .f = ~ .x@name)
  
  object_list <- purrr::set_names(x = object_list, nm = object_names)
  
  check_object_compatibility()
  
  
  
}



check_object_compatibility <- function(object_list){
  
  exp_types <- purrr::map_chr(.x = object_list, .f = ~ object@set_up$experiment_type)

  # check if equal experiment types
  all_equal <- base::all(exp_types == exp_types[1])
    
  if(!base::isTRUE(all_equal)){
    
    base::stop("Experiment types must be equal.")
    
  }
  
  
  # check if equal set up 
  if(exp_types[1] == "time_lapse"){
    
    frame_nom <- purrr::map_dbl(.x = object_list, .f = ~ object@set_up$nom)
    
    all_equal <- base::all()
    
    
  }
  
  
  
}