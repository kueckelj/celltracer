#' @title Assemble a directory
#' 
#' @description Assembles a single character string direction from the 
#' output of \code{shinyFiles::shinyDirButton()}.
#'
#' @param input_list A named list: output of \code{shinyFiles::shinyDirButton()} 
#'
#' @return A character string. 

hlpr_assemble_directory <- function(input_list){
  
  root <-
    stringr::str_remove(input_list$root, pattern = "\\(") %>% 
    stringr::str_remove(pattern = "\\)")
  
  path <-
    purrr::map_chr(.x = input_list$path, ~ base::return(.x)) %>% 
    purrr::discard(.p = ~ .x == "") %>% 
    stringr::str_c(collapse = "/") %>% 
    stringr::str_c(root, ., sep = "/")
  
  base::return(path)
  
}


#' @title Assemble a cell id 
#' 
#' @description Assembles a complete unique cell id including well plate, 
#' well image heritage.
#'
#' @inherit check_track_df params


hlpr_assemble_track_df <- function(track_df, wp_data, wp_index, wp_name){
  
  well_info_df <- wp_data$wp_df[,c("well", "cell_line", "condition", "cl_condition")]
  
  result_df <- 
    dplyr::mutate(.data = track_df,
        cell_id = stringr::str_c("CID", cell_id,"WI", well_image,"WP", wp_index,sep = "_"),
        well = stringr::str_extract(string = well_image, pattern = well_regex), 
        well_plate_index = stringr::str_c("WP", wp_index, sep = "_"), 
        well_plate_name = {{wp_name}}
    ) %>% 
    dplyr::left_join(x = ., y = well_info_df, by = "well")
  
  
  base::return(result_df)
  
}


#' @title Clarifying caption
#'
#' @description Returns a \code{ggplot2::labs()}-add on in which the 
#' caption clarifies whether the plot refers to the times pan before the treatment, 
#' after the treatment or to the entire experiment. 
#' 
#' If there was not treatment or the cells were treated right from the beginning 
#' no caption is returned.
#'
#' @inherit check_object params
#' @inherit phase_all params   
#'
#' @return

hlpr_caption_add_on <- function(object, phase){
  
  if(time_displaced_tmt(object)){
    
    if(phase == "entire"){
      
      add_on <- 
        ggplot2::labs(caption = "Before & after treatment")
      
    } else if(phase == "first_tmt") {
      
      add_on <- 
        ggplot2::labs(caption = "After treatment") 
      
    } else if(phase == "before_tmt"){
      
      add_on <- 
        ggplot2::labs(caption = "Before treatment") 
      
    }
    
  } else {
    
    add_on <- NULL
    
  }
  
  base::return(list(add_on))
  
}



#' @title Process Track Data.frame
#' 
#' @description Filters the whole track data frame into it's phase-subparts
#' and recomputes the variable \emph{dfo (= Distance from origin)} if the 
#' filtered part does not belong to the phase \code{'before_tmt'}.
#'  
#' To be used as input for \code{.f} in \code{purrr::map()}
#' inside the function \code{compileCto()}.
#'
#' @inherit phase_single params  
#' @inherit check_track_df params

hlpr_create_track_list <- function(phase, track_df){
  
  filtered_df <- 
    dplyr::filter(.data = track_df, tmt == {{phase}}) %>% 
    dplyr::select(-tmt)
  
  if(phase != "before_tmt"){
    
    dplyr::group_by(.data = filtered_df, cell_id) %>% 
      dplyr::mutate(dfo = compute_distances_from_origin(x_coords, y_coords)) %>% 
      base::return()
    
  } else {
    
    base::return(filtered_df)
    
  }
  
  
}



#' @title Create Cell Meta Data
#'
#' @description Filters meta variables from track data. To be used in purrr::map2().
hlpr_create_meta_data <- function(df, phase, verbose){
  
  dplyr::select(df, dplyr::all_of(x = meta_variables)) %>% 
    dplyr::mutate(phase = {{phase}}) %>% 
    dplyr::distinct()
  
}


#' @title ggplot2 add on helpers
#' 
#' @description Functions that either return an empty list 
#' or the respective ggplot add-on. 

hlpr_coords_flip_add_on <- function(flip_coords){
  
  if(base::isTRUE(flip_coords)){
    
    ggplot2::coord_flip()
    
  } else {
    
    list()
    
  }
  
}


#' @rdname hlpr_coords_flip_add_on
hlpr_plot_well_plate_fill <- function(input){
  
  if(input == "ambiguity"){
    
    ggplot2::scale_fill_manual(values = ambiguity_colors, drop = FALSE)
    
  } else {
    
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = "milo")
    
  }
  
}




#' @title Make pretty column names 
#' 
#' @description Helper around the problem with concise vs. pretty columnnames 
#'
#' @param df A data.frame that might contain variables for which prettier versions exist.
#' @param value A name for which a prettier version might exist. 
#' @param vec A vector of variable names for which prettier versions might exist. 

hlpr_pretty_colnames <- function(df){
  
  cnames <- base::colnames(df) 
  
  arg_list <- 
    purrr::keep(.x = pretty_names_list, .p = ~ .x %in% cnames) %>% 
    purrr::prepend(x = ., values = list(".data" = df))
  
  rlang::call2(.fn = "rename", .ns = "dplyr", !!!arg_list) %>% 
    base::eval()
  
}

#' @rdname hlpr_pretty_colnames
hlpr_pretty_value <- function(value){
  
  confuns::is_value(value, mode = "character", ref = "value")
  
  if(value %in% pretty_names_vec){
    
    value <- 
      base::names(pretty_names_vec)[pretty_names_vec == value]
    
  }
  
}

#' @rdname hlpr_pretty_colnames
hlpr_pretty_vec <- function(vec){
  
  purrr::map_chr(.x = vec, .f = hlpr_pretty_value)
  
}


#' @title Rename cell tracker columns
#'
#' @inherit check_track_df params 
#'
#' @return A renamed data.frame. 

hlpr_rename_track_df_cols <- function(track_df,
                                        software = "cell_tracker",
                                        denoted_columns = NULL,
                                        additional_columns = NULL){
  
  if(software == "cell_tracker"){
    
    df <- 
      dplyr::select(.data = track_df, 
                    cell_id = `Cell ID`, 
                    x_coords = `x-coordinate [pixel]`,
                    y_coords = `y-coordinate [pixel]`, 
                    frame = `Frame number`,
                    dfo = tidyselect::starts_with("Distance from origin "), 
                    dflp = tidyselect::starts_with("Distance from last point "),
                    speed = tidyselect::starts_with("Instantaneous speed "), 
                    afo = tidyselect::starts_with("Angle from origin "), 
                    aflp = tidyselect::starts_with("Angle from last point ")
      )
    
  } else if(software == "cell_profiler"){
    
    df <- 
      dplyr::select(track_df, dplyr::all_of(x = c(base::unname(denoted_columns), additional_columns))) %>% 
      dplyr::rename(!!denoted_columns)
    
  }
  
  base::return(df)

  
}


#' @title Work around 
#' 
#' @description Awkward solution to the problem that
#' input$change_order_order (of shinyjqui::orderInput()) somehow changes it's class 

hlpr_order_input <- function(order_input){
  
  if(base::is.data.frame(order_input)){
    
    order <- order_input$text
    
  } else if(base::is.character(order_input)){
    
    order <- order_input
    
  }
  
  base::return(order)
  
}



#' @title Split subset input 
#' @description Splits input in a named list.  
hlpr_split_subset <- function(subset_input){
  
  return_list <- list()
  
  return_list$discard <- stringr::str_subset(subset_input, pattern = "^-")
  
  return_list$keep <-  stringr::str_subset(subset_input, pattern = "^-", negate = TRUE)
  
  base::return(return_list)
  
}

#' @rdname hlpr_split_subset
hlpr_select <- function(df, variables_subset){
  
  if(base::is.character(variables_subset)){
    
    vars <- hlpr_split_subset(subset_input = variables_subset)
    
    if(base::length(vars$discard) >= 1){
      
      df <- dplyr::select(df, -dplyr::all_of(x = vars$discard))
      
    }
    
    if(base::length(vars$keep) >= 1){
      
      df <- dplyr::select(df, dplyr::all_of(x = vars$keep))
      
    }
    
  }
  
  base::return(df)
  
}

#' @title Return directory of well plate
#' 
#' @description A set of functions that extract information 
#' from well plate data lists.
#'
#' @param wp_list A well plate list.

hlpr_wp_directories <- function(wp_list){
  
  dir <- wp_list$directory
  
  if(base::is.null(dir)){dir <- "No directory assigned"}
  
  base::return(dir)
  
}


#' @rdname hlpr_wp_directories
hlpr_wp_file_number_f <- function(wp_list){
  
  all_valid_dirs <- wp_list[["valid_directories"]]
  
  stringr::str_extract(string = all_valid_dirs, pattern = file_regex) %>% 
  dplyr::n_distinct()
  
  }

#' @rdname hlpr_wp_directories
hlpr_wp_file_number <- purrr::possibly(.f = hlpr_wp_file_number_f, otherwise = 0, quiet = TRUE)

#' @rdname hlpr_wp_directories
hlpr_wp_ambiguous_number_f <- function(wp_list){dplyr::n_distinct(wp_list[["ambiguous_directories"]])}

#' @rdname hlpr_wp_directories
hlpr_wp_ambiguous_number <- purrr::possibly(.f = hlpr_wp_ambiguous_number_f, otherwise = 0, quiet = TRUE)

#' @rdname hlpr_wp_directories
hlpr_wp_exp_file_number <- function(wp_list){ 
  
  n_wells <- 
    dplyr::filter(wp_list$wp_df, information_status == "Complete") %>% 
    base::nrow()
  
  n_ipw <- wp_list$wp_df$ipw %>% base::unique()
  
  base::return(n_wells * n_ipw)
    
}



#' @title Where do I need this? 

hlpr_select_stats <- function(object, phase = "first_tmt", var_classes, ...){
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  variables_to_select <- 
    purrr::map(.x = var_classes, 
               .f = ~ getVariableNames(
                 object = object, 
                 phase = phase, 
                 var_classes = .x, 
                 flatten = TRUE, ...
               )) %>% 
    purrr::flatten_chr()
  
  stat_df <- 
    dplyr::select(
      getStats(object, phase = phase), 
      dplyr::all_of(x = c("cell_id", variables_to_select))
    )
  
  base::return(stat_df)
  
}

#' Subset the across-variables
#'
#' @description Checks across and across_subset input and if at least one
#' of the across_subset values exists filters the data accordingly.
#'
#' @param data A data.frame that contains the variable specified in \code{across}.
#' @param across Character value. Denotes the discrete variable in the data.frame
#' across which the variables of interest are to be analyzed or displayed. Valid input 
#' options can be obtained via \code{getAcrossOptions()}.
#' @param across_subset Character vector. The groups of interest that the \code{across}-
#' variable contains. Valid input options can be obtained via \code{getVariableValues()}.
#'
#' @return A filtered data.frame, informative messages or an error.
#' @export
#'

hlpr_subset_across <- function(data, across, across_subset){
  
  if(base::is.null(across_subset)){
    
    base::return(data)
    
  } else {
    
    #data[[across]] <- confuns::unfactor(data[[across]])
    
    if(base::is.factor(data[[across]])){
      
      against_input <- base::levels(data[[across]])
      
    } else {
      
      against_input <- base::unique(data[[across]])
      
    }
    
    data <- dplyr::filter(.data = data, !!rlang::sym(across) %in% {{across_subset}})
    
    if(base::is.factor(data[[across]])){
      
      data[[across]] <- 
        base::factor(x = data[[across]], levels = across_subset)
      
    }
    
    base::return(data)
    
  }
  
}




