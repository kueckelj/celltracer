

#' @title Filter track data.frame
#' 
#' @description Filters a track data.frame according to the parameters 
#' set.
#'
#' @inherit check_track_df params
#' @param n_cells Numeric value. The number of cells the resulting data.frame
#' contains. 
#' @param condition  Character vector. The conditions to be kept. 
#' @param cell_line Character vector. The cell lines to be kept. 
#'
#' @return A filtered track data.frame 

filterTracks <- function(track_df,
                         n_cells = 100,
                         condition = NULL,
                         cell_line = NULL){
  
  if(!base::is.null(condition)){
    
    track_df <-
      dplyr::filter(track_df, condition %in% {{condition}})
    
  }
  
  if(!base::is.null(cell_line)){
    
    track_df <- 
      dplyr::filter(track_df, cell_line %in% {{cell_line}})
    
  }
  
  cell_ids <- base::unique(track_df$cell_id)
  
  sample_ids <- base::sample(x = cell_ids, size = n_cells)
  
  result_df <- 
    dplyr::filter(track_df, cell_id %in% sample_ids)
  
  base::return(result_df)
  
}