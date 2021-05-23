#' @title Save Celltracer Object
#'
#' @inherit check_object params
#'
#' @export

saveCelltracerObject <- function(object, verbose = TRUE){
  
  dir <- object@information$directory_cto

  confuns::give_feedback(
    msg = glue::glue("Saving celltracer object under '{dir}'."), 
    verbose = verbose
  )
    
  base::saveRDS(object, file = dir)

  confuns::give_feedback(msg = "Done.", verbose = verbose)
    
}