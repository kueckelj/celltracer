#' @title Well Plate Data.frame 
#' 
#' @description Sets up a data.frame in which each observation refers
#' to a well.
#'
#' @param type Character value. One of \emph{'2x3 (6)', '3x4 (12)', '4x6 (24)', '6x8 (48)', '6x12 (96)'}
#'
#' @export
#'

setUpWellPlateDf <- function(type){
  
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
  
  base::return(well_plate_df_new)
  
}
