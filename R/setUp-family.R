#' @title Well Plate Data.frame 
#' 
#' @description Sets up a data.frame in which each observation refers
#' to a well.
#'
#' @param type Character value. One of \emph{'2x3 (6)', '3x4 (12)', '4x6 (24)', '6x8 (48)', '8x12 (96)'}
#'
#' @export
#'

setUpWellPlateDf <- function(type = "8x12 (96)", phases = NULL){
  
  # row- and column number of current well plate
  well_plate_used <- 
    dplyr::filter(well_plate_info, type == {{type}})
  
  # data.frame (obs => well)
  well_plate_df_new <- 
    tidyr::expand_grid(row_num = 1:well_plate_used$rows, 
                       col_num = 1:well_plate_used$cols) %>%
    dplyr::group_by(row_num, col_num) %>% 
    dplyr::mutate(
      row_letter = base::LETTERS[row_num],
      well = stringr::str_c(row_letter, col_num, sep = ""), 
      group = "well_plate",
      information_status = base::factor(x = "Missing",
                                        levels = c("Complete", "Incomplete", "Missing")),
      cell_line = "unknown",
      condition = "unknown", 
      cl_condition = "unknown & unknown", 
      type = {{type}}
    )
  
  if(!base::is.null(phases)){
    
    phases_names <- 
      english::ordinal(x = base::seq_along(phases)) %>%
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Phase:", sep = " ")
    
    well_plate_df_new$condition_df <- 
      purrr::map(.x = base::seq_along(well_plate_df_new$well), 
                 .f = function(x){
                   
                   base::matrix(ncol = base::length(phases_names), nrow = 1) %>% 
                   base::as.data.frame() %>% 
                   magrittr::set_colnames(value = phases_names)
                   
                 })
    
  }
  
  base::return(well_plate_df_new)
  
}
