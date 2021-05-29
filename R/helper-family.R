#' @title Adds data to cluster object 
#'
#' @inherit argument_dummy params
#'
#' @export
#'
hlpr_add_data_to_cluster_object <- function(object, cluster_object, with_data, phase){
  
  if(base::isFALSE(with_data)){
    
    base::return(cluster_object)
    
  } else {
    
    variables <- cluster_object@variables
    
    data_mtr <-
      getStatsDf(object, phase = phase) %>% 
      dplyr::select(cell_id, dplyr::all_of(x = variables)) %>% 
      tibble::column_to_rownames(var = "cell_id") %>% 
      base::as.matrix()
    
    if(base::isTRUE(cluster_object@scale)){
      
      data_mtr <- base::scale(data_mtr)
      
    }
    
    cluster_object@data <- data_mtr
    
  }
  
  base::return(cluster_object)
  
}

#' @title Add variable set suffix
#' 
#' @description Adds variable set suffix to cluster names. 
#' 
hlpr_add_vset_suffix <- function(cluster_df, variable_set){
  
  tibble::column_to_rownames(cluster_df, var = "cell_id") %>% 
  dplyr::rename_with(.fn = ~ stringr::str_c(.x, "_(", variable_set, ")", sep = "")) %>% 
  tibble::rownames_to_column(var = "cell_id")
  
}



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


#' @title Joins cell data with well plate meta data 
#' 
#' @description Assembles a complete unique cell id including well plate, 
#' well image heritage and joins cell information with well plate meta data. 
#'
#' @inherit check_track_df params


hlpr_assemble_df <- function(track_df, wp_data, wp_index, wp_name){
  
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
#'
#' @return

hlpr_caption_add_on <- function(object, phase){
  
  if(multiplePhases(object)){
    
    phases <-
      purrr::map_chr(phase, .f = confuns::make_capital_letters) %>% 
      glue::glue_collapse(sep = ", ", last = " & ")
    
    add_on <- ggplot2::labs(caption = stringr::str_c(phases, " Phase", sep = ""))
    
  } else {
    
    add_on <- NULL
    
  }
  
  base::return(list(add_on))
  
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


#' @title Process Track Data.frame
#' 
#' @description Filters the whole track data frame into it's phase-subparts
#' and recomputes the variable \emph{dfo (= Distance from origin)} if the 
#' filtered part does not belong to the phase \code{'before_tmt'}.
#'  
#' To be used as input for \code{.f} in \code{purrr::map()}
#' inside the function \code{compileCto()}.
#'

hlpr_create_track_list <- function(phase, track_df){ # deprecated
  
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
    dplyr::distinct()
  
}


#' @title Adjust phase input for feedback messages 
hlpr_glue_phase <- function(object, phase, empty_space = TRUE, string = "for"){ # do not change argument order!!!
  
  all_phases <- getPhases(object)
  
  # if only one phase exist input for argument phase is irrelevant
  if(base::length(all_phases) == 1){
    
    if(base::isTRUE(empty_space)){
    
      string <- " "  
      
    } else {
      
      string <- ""
      
    }
    
  } else {
    
    if(base::isTRUE(empty_space)){
      
      string <- glue::glue(" {string} {phase} phase ")
      
    } else {
      
      string <- glue::glue(" {string} {phase} phase")
      
    }
    
    
  }
  
  base::return(string)
  
}

#' @title Assess number of missing values
#' 
#' @description Returns the maximum count of missing values per cell id 
#' across all variables
#'
hlpr_max_track_na_count <- function(object){
  
  df <- object@information$track_na_count
  
  max_nas <- 
    base::apply(X = dplyr::select(df, -cell_id), 
                MARGIN = 1, 
                FUN = base::max)
  
  res_df <- 
    dplyr::mutate(df, max_nas = {{max_nas}}) %>% 
    dplyr::select(cell_id, max_nas)
  
  base::return(res_df)
  
}


#' @title Merge conditions
hlpr_merge_conditions <- function(track_df, phase, across, verbose = TRUE){
  
  if(base::length(phase) > 1 & across %in% c("condition")){
    
    confuns::give_feedback(
      msg = glue::glue("Merging conditions over {base::length(phase)} phases by cell ID."),
      verbose = verbose,
      with.time = FALSE
      )
    
    track_df <- 
      dplyr::group_by(track_df, cell_id) %>% 
      dplyr::mutate(
        condition = hlpr_merge_condition_by_id(condition)
      ) %>% 
      dplyr::ungroup()
    
  }
  
  base::return(track_df)
  
}

#' @rdname hlpr_merge_conditions
hlpr_merge_condition_by_id <- function(condition){
  
  n_obs <- base::length(condition)
  
  all_conditions <- base::unique(base::as.character(condition))
  
  merged_conditions <- 
    purrr::map2_chr(.x = all_conditions, .y = base::seq_along(all_conditions),
                    .f = ~ stringr::str_c(.y, .x, sep = ". ")) %>% 
    stringr::str_c(collapse = " -> ")
  
  res <- base::rep(merged_conditions, n_obs)
  
  base::return(res)
  
  
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

hlpr_rename_df_cols <- function(df,
                                denoted_columns = NULL,
                                additional_columns = NULL){
  
  df <- 
      dplyr::select(df, dplyr::all_of(x = c(base::unname(denoted_columns), additional_columns))) %>% 
      dplyr::rename(!!denoted_columns)
    
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


#' @title Processing helper
#' 
#' @description To be used as mapped function in purrr::map_*().
#' Only used if experiment type is 'time_lapse'. Processes the track data.frame 
#' with regards to column names as well as module based computation if 
#' the needed variables are present.
#' 
#' - completes the track data.frame in cases of uncomplete tracks
#' - creates frame related columns 
#' - filters observations for frames within denoted timespan
#' - if migration module usable: 
#'   - computes distance from origin
#'   - computes distance from last point
#'   - computes speed 
 
hlpr_process_tracks <- function(df, phase, object, verbose){
  
  # ensure that phase is deault with as a character
  phase <- check_phase(object, phase = phase)
  
  itvl <- object@set_up$itvl
  itvl_u <- object@set_up$itvl_u
  exp_type <- object@set_up$experiment_type
  
  # ensure that the data.frame is complete regarding the observations 
  all_cell_ids <- base::unique(df$cell_id)
  
  frame_range <- base::range(df$frame)
  
  all_frames <- frame_range[1]:frame_range[2]
  
  df <- dplyr::ungroup(df)
  
  # split dfs to avoid NAs in grouping variables
  group_df <- 
    dplyr::select(df, cell_id, dplyr::starts_with("well"), where(base::is.factor))
  
  numeric_df <- dplyr::select(df, cell_id, where(base::is.numeric))
  
  complete_num_df <- 
    tidyr::expand_grid(cell_id = {{all_cell_ids}}, frame = {{all_frames}}) %>% 
    dplyr::left_join(x = ., y = numeric_df, by = c("cell_id", "frame"))
  
  complete_df <- 
    dplyr::left_join(x = complete_num_df, y = group_df, by = "cell_id") %>% 
    dplyr::distinct() %>% # temporary solution to weird multiplying of observations
    dplyr::arrange(cell_id) %>% 
    dplyr::select(cell_id, where(base::is.numeric)) # discard non numeric variables
  
  # allow computation of later phases to refer to the last position of the previous phase
  # ignored if only one phase exist as phase in this case can only be "first"
  if(phase != "first"){
    
    n_phase <- base::which(getPhases(object) == phase) - 1
    
    prev_track_df <- 
      object@cdata$tracks[[n_phase]] %>% 
      dplyr::group_by(cell_id) %>% 
      dplyr::slice_max(frame) %>% # slice by frame as frame_num is not part of object data yet
      dplyr::select(cell_id, where(base::is.numeric))
    
    complete_df <- 
      dplyr::bind_rows(complete_df, prev_track_df) %>% 
      dplyr::group_by(cell_id) %>% 
      dplyr::arrange(frame, .by_group = TRUE)
    
    prev_last_frame <- base::max(prev_track_df$frame)
    
  }
  
  mutated_df <- 
    dplyr::mutate(.data = complete_df, 
      frame_num = frame,
      frame_time = frame * itvl,
      frame_itvl = stringr::str_c(frame_time, itvl_u, sep = " "),
      frame = NULL
      ) %>% 
    # in case of multiple phases this only affects the data.frame of 
    # the last phase as previous phases have lower max(frame_num)
    dplyr::filter(frame_num <= object@set_up$nom)
  
  if(isUsable(object, module = "migration")){
    
    msg <-
      glue::glue(
        "Computing variables for migration module{ref_phase}.",
        ref_phase = hlpr_glue_phase(object, phase, FALSE)
        )
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    mutated_df <- 
      dplyr::group_by(.data = mutated_df, cell_id) %>% 
      dplyr::mutate(
        dfo = compute_distances_from_origin(x_coords, y_coords),
        dflp = compute_distances_from_last_point(x_coords, y_coords), 
        speed = dflp / object@set_up$itvl
      )
    
    # delete the added frame row
    if(phase != "first"){
      
      mutated_df <-
        dplyr::filter(mutated_df, frame_num > prev_last_frame)
      
    }
    
    track_df <-
      dplyr::select(.data = mutated_df,
        cell_id, x_coords, y_coords, speed, dfo, dflp,
        dplyr::starts_with(match = "frame"), 
        dplyr::everything() 
      ) %>% 
      dplyr::select(-dplyr::starts_with(match = "well"))
    
  } else {
    
    track_df <- mutated_df
    
  }
  
  track_df_numeric <- 
    dplyr::select(track_df, cell_id, dplyr::starts_with("frame"), where(base::is.numeric)) %>% 
    dplyr::ungroup()
  
  base::return(track_df_numeric)
  
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


#' @title Add vertical phase separating line to lineplot
hlpr_phase_vertical_line <- function(object, phase, display_vline, vline_clr, vline_type){
  
  if(multiplePhases(object) & base::length(phase) >= 2){
    
    frames_df <-
      purrr::imap_dfr(
        .x = object@set_up$phases[phase], 
        .f = function(phase_start, p){
          
          if(p != "first"){
            
            pos <- base::which(object@set_up$measurement_string == phase_start)
            
            data.frame(frame = pos, phase = p)
            
          } 
          
          
        })
    
    add_on <-
      ggplot2::geom_vline(
        data = frames_df,
        mapping = ggplot2::aes(xintercept = frame),
        linetype = vline_type, 
        color = vline_clr
      )
    
  } else {
    
    add_on <- list()
    
  }
  
  base::return(add_on)
  
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




