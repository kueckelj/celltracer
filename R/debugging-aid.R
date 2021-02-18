
debugging <- function(input){
  
  base::tryCatch(
    
    if(base::isTRUE(debug)){
      
      print(input)
      
    }
    
  , error = function(errro){
    
    # nothing happens
    
  }
  
  )
  
}