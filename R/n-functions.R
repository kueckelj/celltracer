

#' @title Number of miscellaneous content
#' 
#' @description These functions return a numeric value describing the number
#' of \code{n*()} the object contains. (E.g. \code{nCells()} returns the total 
#' number of cells about which the object contains data, \code{nConditions()} 
#' returns the total number of conditions, etc. )
#'
#' @inherit argument_dummy params
#'
#' @return A numeric value. 
#' @export
#'

nCells <- function(object){
  
  check_object(object)
  
  getStatsDf(object) %>% 
    base::nrow()
  
}

#' @rdname nCells
#' @export
nCellLines <- function(object){
  
  check_object(object)
  
  getCellLines(object) %>% 
    base::length()
  
}

#' @rdname nCells
#' @export
nConditions <- function(object, phase = NULL){
  
  check_object(object)
  
  getConditions(object) %>% 
    base::length()
  
}

