

# Documentations dummies --------------------------------------------------

#' dummy 
#' @return An updated cell tracer object that contains the data added.
add_family <- function(){}



# Functions ---------------------------------------------------------------

#' @title Add discrete/categorical variables
#' 
#' @description These functions allow to join new discrete/categorical variables that can be referred 
#' to via the \code{across}-argument of many functions.
#'
#' @inherit check_object params 
#' @inherit cluster_df params
#' @inherit input_df params
#' @inherit phase_single params
#' @param cluster_name,variable_name Character value. The name of the variable that is to be joined.
#' 
#' @inherit add_family return
#'
#' @export

addClusterVariable <- function(object,
                               cluster_df,
                               cluster_name,
                               phase = "first_tmt", 
                               overwrite = FALSE, 
                               verbose = TRUE){
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  new_cluster_df <- 
    dplyr::select(cluster_df, dplyr::all_of(x = c("cell_id", cluster_name)))
                  
  old_cluster_df <- getClusterData(object, phase = phase)
  
  if(cluster_name %in% base::colnames(old_cluster_df)){
    
    if(base::isTRUE(overwrite)){
      
      base::message(glue::glue("Overwriting already existing cluster variable {cluster_name}."))
      
      old_cluster_df[[cluster_name]] <- NULL
      
    } else {
      
      base::stop(glue::glue("Cluster variable {cluster_name} already exists. Set argument 'overwrite' to TRUE in order to continue."))
      
    }
    
  }
  
  updated_cluster_df <- 
    dplyr::left_join(
      x = old_cluster_df, 
      y = new_cluster_df, 
      by = "cell_id"
    )
  
  #!!! add dplyr::rename() to equip the added variable with a
  
  object@data$cluster[[phase]] <- updated_cluster_df
  
  base::return(object)
  
}

#' @rdname addClusterVariable
#' @export
addGroupingVariable <- function(object,
                                input_df,
                                variable_name,
                                phase = "first_tmt"){
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  old_meta_df <- getMeta(object)
  new_meta_df <- dplyr::select(input_df, dplyr::all_of(x = c("cell_id", variable_name)))
  
  updated_meta_df <- 
    dplyr::left_join(
      x = old_meta_df, 
      y = new_meta_df, 
      by = "cell_id"
    )
  
  #!!! add dplyr::rename() to equip the added variable with a
  
  object@data$meta <- updated_meta_df
  
  base::return(object)
  
}



#' @title Add dimensional reduction data
#' 
#' @description This function allows to add dimensional reduction data to the 
#' object that can be referred to e.g. in \code{plotDimRed()}.
#'
#' @inherit check_object params
#' @inherit dim_red_df params
#' @inherit dim_red_method params
#' 
#' @inherit add_family return
#' 
#' @export

addDimRedData <- function(object, dim_red_df, dim_red_method){
  
  object@data$dim_red[[name]] <- data
  
}