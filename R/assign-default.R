#' @title Assigns default to parent function
#'
#' @inherit check_object params
#'
assign_default <- function(object){
  
  ce <- rlang::caller_env()
  
  default_args <- base::names(object@default)
  
  cfn <- rlang::caller_fn()
  
  # get arguments froms calling function
  cargs <- rlang::fn_fmls_names(fn = cfn)
  
  # keep arguments from calling function
  default_args <- cargs[cargs %in% default_args]
  
  # assign default argument values if input was set to NULL
  for(arg in default_args){
    
    arg_value <-
      base::parse(text = arg) %>%
      base::eval(envir = ce)
    
    if(base::is.null(arg_value)){
      
      arg_value <- object@default[[arg]]
      
      if(!base::is.null(arg_value)){
        
        base::assign(
          x = arg,
          value = arg_value,
          envir = ce
        )
        
      }
      
    }
    
  }
  
}