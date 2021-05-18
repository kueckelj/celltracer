

#' @title Set clustering object
#' 
#' @description Sets a cluster_conv object and empties its data slot. 
#'
#' @inherit argument_dummy params 
#'
setClusterConv <- function(object, cluster_object, method, phase, variable_set){
  
  cluster_object@data <- matrix()
  
  object@analysis$clustering[[method]][[phase]][[variable_set]] <- 
    cluster_object
  
  base::return(object)
  
}









#' @title Set data data.frames
#' 
setGroupingDf <- function(object, grouping_df, phase){
  
  object@data$grouping[[phase]] <- grouping_df
  
  base::return(object)
  
}