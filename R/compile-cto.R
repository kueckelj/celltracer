#' @title Compiles a cell tracer object
#'
#' @param ct_list 
#'
#' @return A cell tracer object. 
#'

compileCto <- function(ct_list){
  
  set_up <-
    purrr::discard(ct_list$set_up, .p = ~ base::is.list(.x))
  
  cto <- methods::new(Class = "cto")
  

# Slot: Data --------------------------------------------------------------

  track_df <- 
    dplyr::mutate(.data = ct_list$track_df, 
                  frame_num = frame, 
                  frame_time = frame * set_up$itvl,
                  frame_itvl = stringr::str_c(frame_time, set_up$itvl_u, sep = " "),
                  frame = NULL) %>% 
    dplyr::filter(frame_num <= set_up$nom)
  
  if(set_up$tmt_start %in% c("From beginning", "No treatment")){
    
    stat_df <-
      dplyr::mutate(.data = track_df, tmt = "no_tmt") %>% 
      compute_stat()
    
  } else {
    
    stat_df <- 
      purrr::map(.x = c("before", "afterwards"), 
                 .f = ~ dplyr::filter(.data = track_df, tmt == {{.x}}) %>%
                        compute_stat() %>% 
                        dplyr::mutate(tmt = {{.x}})
                 ) %>% 
      purrr::map_df(.f = ~ base::return(.x))
    
  }
  
  cto@data <- list("track" = track_df, 
                   "stat" = stat_df)
  
  # -----

# Other slots -------------------------------------------------------------
  
  cto@set_up <- set_up

  cto@wp_info <-
    purrr::map(.x = ct_list$wp_info, "wp_df_eval")
  
  cto@storage_info <- 
    purrr::map(.x = ct_list$wp_info, ~ .x[c("directory", "valid_directories", "missing_files")])
  
  cto@background_info <- list()
  
  # -----

  
  base::return(cto)
  
  
}

