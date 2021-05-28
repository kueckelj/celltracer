# NOT Exported ------------------------------------------------------------

# cdata set up

set_up_cdata_meta <- function(object, verbose = TRUE){
  
  confuns::give_feedback(msg = "Creating cell meta data.", verbose = verbose)
  
  data_slot <- base::ifelse(test = isTimeLapseExp(object), yes = "tracks", no = "stats")
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    object@cdata$meta <- 
      purrr::map(.x = all_phases,
                 .f = function(phase){
                   
                   object@cdata[[data_slot]][[phase]] %>% 
                     dplyr::select(cell_id,cell_line, condition) %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(
                       cell_line = base::as.factor(cell_line), 
                       condition = base::as.factor(condition)
                      )
                   
                   
                 }) %>% 
      purrr::set_names(nm = all_phases)
    
  } else {
    
    object@cdata$meta <-
      object@cdata[[data_slot]][[1]] %>% 
      dplyr::select(cell_id, cell_line, condition) %>% 
      dplyr::distinct()
    
  }
  
  base::return(object)
  
}

set_up_cdata_cluster <- function(object, verbose){
  
  confuns::give_feedback(msg = "Creating cell cluster data.", verbose = verbose)
  
  data_slot <- base::ifelse(test = isTimeLapseExp(object), yes = "tracks", no = "stats")
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    object@cdata$cluster <- 
      purrr::map(.x = all_phases,
                 .f = function(phase){
                   
                   dplyr::select(object@cdata[[data_slot]][[phase]], cell_id) %>% 
                     dplyr::distinct()
                   
                   
                 }) %>% 
      purrr::set_names(nm = all_phases)
    
  } else {
    
    object@cdata$cluster <- 
      dplyr::select(object@cdata[[data_slot]][[1]], cell_id) %>% 
      dplyr::distinct()
    
  }
  
  base::return(object)
  
}

set_up_cdata_tracks_and_stats <- function(object, verbose = TRUE){
  
  confuns::give_feedback(msg = "Creating track- & stat data.", verbose = verbose)
  
  # if time lapse experiment way of processing depends on phase set up
  if(isTimeLapseExp(object)){
    
    if(multiplePhases(object)){
      # multiple phases => named list of data.frames
      
      # process tracks
      object@cdata$tracks <- 
        purrr::map2(
          .x = object@cdata$tracks,
          .y = getPhases(object),
          object = object,
          verbose = verbose,
          .f = hlpr_process_tracks
          ) %>% 
        purrr::set_names(nm = getPhases(object))
      
      # compute statistics 
      object@cdata$stats <- 
        purrr::map2(
          .x = object@cdata$tracks,
          .y = getPhases(object),
          object = object,
          verbose = verbose,
          .f = compute_cell_stats
          ) %>% 
        purrr::set_names(nm = getPhases(object))
      
    } else {
      
      # one phase => single data.frame 
      
      # process tracks
      object@cdata$tracks <- 
        purrr::map_df(
          .x = object@cdata$tracks,
          phase = NULL,
          object = object,
          verbose = verbose,
          .f = hlpr_process_tracks
        )
      
      # compute statistics 
      object@cdata$stats <- 
        compute_cell_stats(
          df = object@cdata$tracks,
          phase = NULL,
          verbose = verbose, 
          object = object
          )
      
    }
    
    # if not time lapse only one way (not much processing necessary)
  } else {
    
    # data stored in slot stats as list of one slot "only"
    df <- 
      object@cdata$stats$only %>% 
      dplyr::select(cell_id, where(base::is.numeric))
    
    # convert to data.frame
    object@cdata$stats <- df
    
    cnames <- base::colnames(df)
    
    # if available shift cell location info to track data 
    if(base::all(c("x_coords", "y_coords") %in% cnames)){
      
      object@cdata$tracks <- 
        dplyr::select(df, cell_id, x_coords, y_coords) %>% 
        dplyr::mutate(frame_num = 1)
      
    } else {
      
      object@cdata$tracks <- data.frame()
      
    }
    
    if("x_coords" %in% cnames){
      
      object@cdata$stats$x_coords <- NULL
      
    }
    
    if("y_coords" %in% cnames){
      
      object@cdata$stats$y_coords <- NULL
      
    }
    
    
  }
  
  base::return(object)
  
}

# vdata set up 

#' @title Computes variable summaries
#' 
#' @inherit argument_dummy  
#'
#' @inherit updated_object return
#' @export
#'
set_up_vdata <- function(object, verbose = TRUE){
  
  confuns::give_feedback(msg = "Computing variable statistics and summary.", verbose = verbose)
  
  vdata <- list()
  
  if(multiplePhases(object)){
    
    vdata$summary <- 
      purrr::map(.x = getPhases(object), 
                 .f = function(phase){
                   
                   getStatsDf(object, phase = phase) %>% 
                     dplyr::select_if(base::is.numeric) %>% 
                     base::as.matrix() %>% 
                     psych::describe(IQR = TRUE) %>% 
                     base::as.data.frame() %>% 
                     tibble::rownames_to_column(var = "variable") %>% 
                     tibble::as_tibble() 
                   
                 }) %>% 
      purrr::set_names(nm = getPhases(object))
    
  } else {
    
    # over all phases
    stats_mtr <- 
      getStatsDf(object) %>% 
      dplyr::select_if(.predicate = base::is.numeric) %>% 
      base::as.matrix() 
    
    vdata$summary <- 
      psych::describe(stats_mtr, IQR = TRUE) %>% 
      base::as.data.frame() %>% 
      tibble::rownames_to_column(var = "variable") %>% 
      tibble::as_tibble() 
    
  }
  
  object@vdata <- vdata
  
  base::return(object)
  
}


# wpdata 
set_up_cdata_well_plate <- function(object, verbose = TRUE){
  
  data_slot <- base::ifelse(test = isTimeLapseExp(object), yes = "tracks", no = "stats")
  
  object@cdata$well_plate <- 
    dplyr::select(object@cdata[[data_slot]][[1]], dplyr::all_of(x = c("cell_id", well_plate_vars))) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      dplyr::across(.cols = dplyr::starts_with("well"), .fns = base::as.factor)
    )
  
  base::return(object)
  
}



# EXPORTED ----------------------------------------------------------------

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
