#' @title Save cypro object
#'
#' @inherit check_object params
#'
#' @export

saveCelltracerObject <- function(object, verbose = TRUE){
  
  dir <- object@information$directory_cto

  confuns::give_feedback(
    msg = glue::glue("Saving cypro object under '{dir}'."), 
    verbose = verbose
  )
    
  base::saveRDS(object, file = dir)

  confuns::give_feedback(msg = "Done.", verbose = verbose)
    
}