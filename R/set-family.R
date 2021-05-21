

#' @title Set clustering object
#' 
#' @description Sets a convenient object and empties its data slot. 
#'
#' @inherit argument_dummy params 
#'
setClusterConv <- function(object, cluster_object, method, phase, variable_set){
  
  cluster_object@data <- matrix()
  
  if(multiplePhases(object)){
    
    object@analysis$clustering[[method]][[variable_set]][[phase]] <- 
      cluster_object
    
  } else {
    
    object@analysis$clusering[[method]][[variable_set]] <- 
      cluster_object
    
  }
  
  base::return(object)
  
}

#' @rdname setClusterConv
#' @export
setDimRedConv <- function(object, dim_red_object, method, phase, variable_set){
  
  dim_red_object@data <- matrix()
  
  if(multiplePhases(object)){
    
    object@analysis$dim_red[[method]][[variable_set]][[phase]] <- 
      dim_red_object
    
  } else {
    
    object@analysis$dim_red[[method]][[variable_set]] <- 
      dim_red_object
    
  }
  
  base::return(object)
  
}




#' @title Set data data.frames
#' 
setGroupingDf <- function(object, grouping_df, phase){
  
  warning("setGroupingDf() is deprecated in favor of setCellDf()")
  
  object@data$grouping[[phase]] <- grouping_df
  
  base::return(object)
  
}


#' @title Set cell data.frame
#' 
setCellDf <- function(object, slot, df, phase){
  
  if(multiplePhases(object)){
    
    object@cdata[[slot]][[phase]] <- df
    
  } else {
    
    object@cdata[[slot]] <- df
    
  }
  
  base::return(object)
  
}





