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





#' @title Adjust object based default
#' 
#' @description The default input for many recurring arguments is carried with 
#' the celltracer object. Use this function to savely adjust the default 
#' according to your preference. 
#'
#' @inherit argument_dummy params 
#' @param ... The argument names with their new default value specified with the 
#' following syntax 'argument_name' = 'argument_default'.
#' 
#' Use \code{adjustableDefaultInstructions()} to obtain all argument names 
#' whose default input you can manipulate this way. 
#'
#' @inherit update_family return
#' @export
#'
#' @examples
#' 
#'  object <- adjustDefaultInstructions(object, pt_clrp = "jco", pt_alpha = 0.6, method_dist = "spearman")
#' 
#' 
adjustDefaultInstructions <- function(object, ...){
  
  input <- confuns::keep_named(c(...))
  
  confuns::check_one_of(
    input = base::names(input), 
    against = adjustableDefaultInstructions(), 
    ref.input = "arguments to adjust", 
    fdb.opt = 2, 
    ref.opt = "arguments that can be adjusted"
  )
  
  for(i in base::seq_along(input)){
    
    slot <- base::names(input[i])
    
    if(slot %in% default_character_values){
      
      confuns::is_value(x = input[i], mode = "character", ref = slot)
      
    } else if(slot %in% default_logical_values){
      
      confuns::is_value(x = input[i], mode = "logical", ref = slot)
      
    } else if(slot %in% defalt_numeric_values){
      
      confuns::is_value(x = input[i], mode = "numeric", ref = slot)
      
    } 
    
    object@default[[slot]] <- base::unname(input[i])
    
  }
  
  base::return(object)
  
}