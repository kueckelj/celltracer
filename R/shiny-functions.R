
#' @title Add helping text 
#'
add_helper <- function(shiny_tag, content, title = "What do I have to do here?", type = "inline", size = "s", ...){
  
  
  res <- 
    shinyhelper::helper(shiny_tag = shiny_tag, 
                        content = content, 
                        title = title, 
                        size = size,
                        type = type, 
                        ...)
  
  base::return(res)
  
}


#' @title Evaluate the availability of files 
#' 
#' @description This functions takes a list as input that contains a well-plate data.frame (slot 
#' must be named \emph{wp_df}) and a directory. The directory leads to the folder in which to look for
#' .csv and .xls files named according to the well-image they belong. The availability of files is compared 
#' to what is expected based on the well-plate data.frame (wells, number of images per well etc.) 
#'
#' @param wp_list A list that contains a well-plate data.frame and a directory leading to the folder in which 
#' the respective files are stored. 
#' @param recursive Logical value. Given to argument \code{recursive} of \code{base::list.files()}.
#' @param keep_filetype Character value. Determines the filetype to be ignored if for one well-image 
#' a .csv and a .xls file is found. Either \emph{'xls$'} or \emph{'csv$'}.
#'
#' @return The same list with additional slots containing a the well-plate data.frame joined 
#' with the evaluation variables as well as a vector containing the well-image files that are missing,
#' a vector with file directories that are ambiguous and a vector with the valid directories from which
#' to load the data. 
#' 

evaluate_file_availability_shiny <- function(wp_list, recursive = TRUE, keep_filetype = "csv$"){
  
  directory <- wp_list$directory
  wp_df <- wp_list$wp_df
  relevant_wp_df <- dplyr::filter(.data = wp_df, information_status == "Complete")
  
  ignore_filetypes <- filetypes[filetypes != keep_filetype]
  
  ipw <- wp_df$ipw %>% base::unique()
  
  # filter all subsequent directories for the 'file_regex' pattern
  wp_directories <-
    base::list.files(path = directory, recursive = recursive, full.names = TRUE) %>% 
    stringr::str_subset(pattern = file_regex)
  
  if(base::isTRUE(debug_ct)){print(wp_directories)}
  
  # get directories that are to be discarded
  well_image_vec <-
    stringr::str_extract(string = wp_directories, pattern = file_regex) %>% 
    stringr::str_extract(pattern = well_image_regex)
  
  well_image_vec_unique <- base::unique(well_image_vec)
  
  well_image_df <- 
    base::data.frame("well_image" = well_image_vec, stringsAsFactors = FALSE)
  
  # count well images, extract ambiguous ones and create the respective 
  ambiguous_files <- 
    dplyr::group_by(.data = well_image_df, well_image) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::pull(var = "well_image")
  
  if(base::length(ambiguous_files) != 0){
    
    ambiguous_files <-
      purrr::map(.x = ignore_filetypes,
                 .f = ~ stringr::str_c(ambiguous_files, .x, sep = ".")) %>% 
      purrr::flatten_chr()
    
  }
  
  #  extract irrelevant, dismissed wells and create the respective regex
  dismissed_wells <- 
    dplyr::filter(.data = wp_df, information_status != "Complete") %>% 
    dplyr::pull(var = "well")
  
  if(base::length(dismissed_wells) != 0){
    
    dismissed_wells <- stringr::str_c(dismissed_wells, "\\d{1}\\.(csv|xls|xlsx)$", sep = "_")
    
  }
  
  # create a regex that discards invalid, dismissed or irrelevant directories 
  discard_pattern <- stringr::str_c(c(ambiguous_files, dismissed_wells), collapse = "|")
  
  # apply discard pattern if there are files to be discarded
  if(discard_pattern != ""){
    
    invalid_directories <- stringr::str_detect(string = wp_directories, pattern = discard_pattern)
    
    filtered_directories <- wp_directories[!invalid_directories]
    
  } else if(discard_pattern == "") {
    
    filtered_directories <- wp_directories
    
  }
  
  # filter valid directories for directories that fall in the range of well images
  # with complete information status
  wells <- 
    base::unique(relevant_wp_df$well) %>% 
    stringr::str_c(collapse = "|") %>% 
    stringr::str_c("(", ., ")")
  
  relevant_pattern <- 
    stringr::str_c(1:ipw, collapse = "|") %>% 
    stringr::str_c("(", ., ")") %>% 
    stringr::str_c(wells, "_", ., "\\.(csv|xls|xlsx)$")
  
  valid_directories <- stringr::str_subset(string = filtered_directories, pattern = relevant_pattern)
  
  # obtain file availability in a tidy data.frame
  file_availability <- 
    base::data.frame(
      files = stringr::str_extract(valid_directories, file_regex), 
      stringsAsFactors = FALSE
    ) %>%
    tibble::as_tibble() %>% 
    tidyr::separate(col = files, into = c("well", "image", "file_type"), sep = "_|\\.") %>% 
    tidyr::unite(col = "well_image", well, image, sep = "_", remove = FALSE) %>% 
    dplyr::group_by(well) %>% 
    dplyr::mutate(well_files = dplyr::n()) %>% 
    dplyr::group_by(well_image) %>% 
    dplyr::mutate(well_image_files = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(well, well_files, well_image_files)
  
  missing_files <- 
    dplyr::pull(.data = relevant_wp_df, var = "well") %>% 
    tidyr::expand_grid(well = ., image = 1:ipw) %>% 
    dplyr::mutate(
      well_image = stringr::str_c(well, image, sep = "_"), 
      missing = !well_image %in% {{well_image_vec_unique}}
    ) %>% 
    dplyr::filter(missing) %>% 
    dplyr::pull(var = "well_image")
  
  # evaluate file availability in a joined data.frame  
  eval_df <- 
    dplyr::left_join(x = wp_df, y = file_availability, by = "well") %>% 
    tidyr::replace_na(replace = list(well_files = 0, well_image_files = 0)) %>% 
    dplyr::group_by(well) %>% 
    dplyr::mutate(
      ambiguous = dplyr::if_else(base::any(well_image_files > 1), true = TRUE, false = FALSE),
      availability_status = dplyr::case_when(
        information_status != "Complete" ~ "Dismissed", 
        ambiguous ~ "Ambiguous",
        well_files == 0 ~ "Missing", 
        well_files < ipw ~ "Incomplete", 
        well_files >= ipw ~ "Complete"
      ), 
      availability_status = base::factor(x = availability_status, levels = c("Complete", "Incomplete", "Missing", "Ambiguous", "Dismissed"))
    ) %>% 
    dplyr::distinct()
  
  if(base::any(eval_df$availability_status == "Ambiguous")){
    
    ambiguous_patterns <- 
      stringr::str_extract(string = valid_directories, pattern = file_regex) %>% 
      base::data.frame("files" = ., stringsAsFactors = FALSE) %>% 
      dplyr::group_by(files) %>% 
      dplyr::summarise(count = dplyr::n()) %>% 
      dplyr::filter(count > 1) %>% 
      dplyr::pull(var = "files") %>% 
      stringr::str_c(., "$", sep = "") %>% 
      stringr::str_c(collapse = "|")
    
    ambiguous_directories <- 
      stringr::str_subset(string = valid_directories, pattern = ambiguous_patterns) %>% 
      stringr::str_remove(pattern = directory) %>% 
      stringr::str_c("~", ., sep = "")
    
    ambiguous_well_images <- 
      stringr::str_extract(string = ambiguous_directories, pattern = well_image_regex)
    
    ambiguous_list <-
      base::vector(mode = "list", length = dplyr::n_distinct(ambiguous_well_images)) %>% 
      magrittr::set_names(value = base::unique(ambiguous_well_images))
    
    for(i in base::seq_along(ambiguous_directories)){
      
      aw <- ambiguous_well_images[i]
      
      if(stringr::str_detect(ambiguous_directories[i], pattern = aw)){
        
        ambiguous_list[[aw]] <- base::append(x = ambiguous_list[[aw]], 
                                             value = ambiguous_directories[i])
        
      } 
      
    }
    
    rows <- base::names(ambiguous_list)
    cols <- purrr::map_int(.x = ambiguous_list, .f = base::length) %>% base::max()
    
    mtr <- base::matrix(data = NA, nrow = base::length(rows), ncol = cols)
    
    base::rownames(mtr) <- rows
    base::colnames(mtr) <- stringr::str_c("Amb. Directory", 1:cols, sep = " ")
    
    for(i in base::seq_along(ambiguous_list)){
      
      well_image <- ambiguous_list[[i]]
      
      for(d in base::seq_along(well_image)){
        
        mtr[i,d] <- well_image[d]
        
      }
      
    }
    
    ambiguous_df <- 
      base::as.data.frame(mtr) %>% 
      tibble::rownames_to_column(var = "Well-Image")
    
  } else {
    
    ambiguous_df <- base::data.frame()
    
  }
  
  wp_list$wp_df_eval <- eval_df
  wp_list$valid_directories <- valid_directories
  wp_list$missing_files <- missing_files
  wp_list$ambiguous_directories <- ambiguous_df
  
  base::return(wp_list)
  
}





#' @title Display loading status
#' 
#' @description Creates a data.frame that displays information 
#' about the file availability evaluation undergone for every well
#' plate.
#'
#' @param well_plate_list A list of well plate lists. 
#' 
#' @usage In shiny-module \code{moduleLoadData}.
#'

loading_status_table_shiny <- function(well_plate_list){
  
  well_plates <- base::names(well_plate_list)
  
  directories <- purrr::map_chr(.x = well_plate_list, .f = hlpr_wp_directories)
  
  num_files <- purrr::map_int(.x = well_plate_list, hlpr_wp_file_number)
  
  num_ambiguous <- purrr::map_int(.x = well_plate_list, .f = hlpr_wp_ambiguous_number)
  
  num_files_expected <- purrr::map_int(.x = well_plate_list, hlpr_wp_exp_file_number)
  
  status_df <- 
    base::data.frame(
      "wp" = well_plates, 
      "asd" = directories, 
      "nof" = num_files, 
      "enof" = num_files_expected,
      "noaf" = num_ambiguous
    ) %>% 
    dplyr::mutate(
      "ready" = dplyr::case_when(
        asd == "No directory assigned" ~ "No", 
        nof == 0 ~ "No", 
        noaf != 0 ~ "No", 
        TRUE ~ "Yes"
      )
    ) %>% 
    dplyr::rename(
      "Well Plate" = wp,
      "Assigned Directory" = asd,
      "Number of Files" = nof,
      "Expected Number of Files" = enof,
      "Number of Ambiguous Directories" = noaf, 
      "Ready to load" = ready
    )
  
}


#' @title Reads cell track data
#' 
#' @description A function to be used for argument \code{.f} of \code{purrr::map_df}.
#' Calls the respective reading function depending on the directory's ending. 
#'
#' @param directory Character value. The directory from which to read the data.frame.
#' of \code{.x} contains.
#' @param progress A shiny - R6 progress object. 
#' @param progress_n Numeric value. Indicates the iterating progress.
#'
#' @return The read in data.frame with three additional informative columns: \emph{well_image, condition}
#' and \emph{cell_line}.

read_data_files_shiny <- function(directory, progress_n, progress, object){
  
  progress$set(value = progress_n)
  
  if(stringr::str_detect(string = directory, pattern = "csv$")){
    
    df <- 
      
      base::suppressMessages({
        
        base::suppressWarnings({
          
          readr::read_csv(file = directory)
          
        })
        
      })
      
    
  } else if(stringr::str_detect(string = directory, pattern = "xls$")){
    
    df <- readxl::read_xls(path = directory)
    
  } else if(stringr::str_detect(string = directory, pattern = "xlsx$")){
    
    df <- readxl::read_xlsx(path = directory)
    
  }
  
  df[base::is.na(df)] <- 0
  
  well_image <-
    stringr::str_extract(string = directory, pattern = file_regex) %>% 
    stringr::str_extract(pattern = well_image_regex)
  
  denoted_columns_list <- object@set_up$example$denoted_columns
    
  denoted_columns <- 
    c(x_coords = denoted_columns_list$x_coords, 
      y_coords = denoted_columns_list$y_coords, 
      frame = denoted_columns_list$frame, # NULL (ignored) if non time lapse experiment
      cell_id = denoted_columns_list$cell_id) %>% 
    purrr::discard(.p = base::is.null)
    
  additional_columns <- denoted_columns_list$additional
    
  variable_names <- c(base::unname(denoted_columns), additional_columns)
  
  # do the columns of the read in data.frame deviate from the denoted ones?
  missing_columns <- 
    purrr::map(.x = variable_names, 
               .f = check_df_variables,
               df = df) %>%
    purrr::discard(.p = base::is.null) %>% 
    purrr::flatten_chr()
  
  n_missing_cols <- base::length(missing_columns)
  
  # if so give feedback 
  if(n_missing_cols > 0){
    
    ref <- base::ifelse(n_missing_cols > 1, "columns", "column")
    ref_cols <- stringr::str_c(missing_columns, collapse = "', '")
    
    base::stop(glue::glue("Missing {ref}: '{ref_cols}'"))
    
  }
  
  df_return <-
    hlpr_rename_df_cols(
      df = df,
      denoted_columns = denoted_columns,
      additional_columns = additional_columns
      ) %>% 
    dplyr::mutate(well_image = {{well_image}}) 
  
  list_return <- 
    list("df" = df_return, 
         "well_image" = well_image, 
         "directory" = directory)
  
  base::return(list_return)
  
}


#' @title Read in cell track data
#' 
#' @description Reads in excel- and csv-files of all valid 
#' directories while displaying the progress via a progress bar.
#'
#' @param wp_list A well plate list.
#' @param wp_name The name of the respective well plate.
#'
#' @return A nested list obtained by \code{purrr::safely()} with 
#' successfully loaded files and failed ones including the error message.
#' 
#' @export

load_data_files_shiny <- function(wp_list, wp_name, session, object){
  
  directories <- wp_list$valid_directories
  
  well_image_files <- stringr::str_extract(string = directories, pattern = file_regex)
  
  num_files <- base::length(directories)
  
  # set up progress bar
  progress <- shiny::Progress$new(session, min = 1, max = num_files)
  base::on.exit(progress$close())
  
  progress$set(message = glue::glue("Reading data for well plate '{wp_name}' :"), 
               detail = glue::glue("Total of {num_files} files."))
  
  data_list <- 
    purrr::map2(.x = directories, 
                .y = base::seq_along(directories), # input for progress_n
                .f = purrr::safely(read_data_files_shiny),
                object = object, 
                progress = progress) %>% 
    purrr::set_names(nm = well_image_files)
  

  
  # sort successfull and failed loading 
  track_data_list <- 
    list("successful" = purrr::keep(.x = data_list, .p = ~ base::is.null(.x[["error"]])),
         "failed" = purrr::keep(.x = data_list, .p = ~ !base::is.null(.x[["error"]])))
  
  base::return(track_data_list)
  
}



#' @title Integrate cell stat tables
#' 
#' @description Subsets the successfully loaded files 
#'
#' @param track_data_list The output of \code{load_track_files_shiny()}.
#'
#' @return A list of one slot 'only' with the stat data.frame
#
assemble_stat_list_shiny <- function(stat_data_list, well_plate_list, object){
  
  df_list <- 
    purrr::map(.x = stat_data_list, ~.x[["successful"]]) %>% 
    purrr::map(.x = ., .f = ~ purrr::map(.x = .x, "result")) %>% 
    purrr::map(.x = ., .f = ~ purrr::map_df(.x = .x, "df"))
  
  all_data_list <- 
    list(df_list,
         well_plate_list,
         base::seq_along(df_list), 
         base::names(df_list)
    )
  
  # join well plate information and create final cell id
  df <- 
    purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_df)
  
  cell_lines <- df$cell_line %>% base::unique() %>% base::sort()
  conditions <- df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- df$cl_condition %>% base::unique() %>% base::sort()
  
  final_df <-
    dplyr::mutate(.data = df, 
                  cell_line = base::factor(x = cell_line, levels = cell_lines), 
                  condition = base::factor(x = condition, levels = conditions), 
                  cl_condition = base::factor(x = cl_condition, levels = cl_conditions)
    )
  
  stat_list <- base::list("only" = final_df)
  
  base::return(stat_list)
  
}

#' @title Integrate cell track tables
#' 
#' @description Subsets the successfully loaded files. In case of timelapse 
#' experiments with multiple phases it sorts the data into multiple 
#' data.frames and returns a list of multiple data.frames named according 
#' to the ordinal number of the phase ("first", "second" etc.). In case of
#' non timelapse experimernts the list contains only one data.frame in a 
#' slot named "first".
#'
#' @param track_data_list The output of \code{load_track_files_shiny()}.
#'
#' @return A list of data.frames named by 

assemble_track_list_shiny <- function(track_data_list, well_plate_list, object){
  
  df_list <- 
    purrr::map(.x = track_data_list, ~.x[["successful"]]) %>% 
    purrr::map(.x = ., .f = ~ purrr::map(.x = .x, "result")) %>% 
    purrr::map(.x = ., .f = ~ purrr::map_df(.x = .x, "df"))
  
  all_data_list <- 
    list(df_list, well_plate_list, base::seq_along(df_list), base::names(df_list))
  
  df <- 
    purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_df)
  
  cell_lines <- df$cell_line %>% base::unique() %>% base::sort()
  conditions <- df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- df$cl_condition %>% base::unique() %>% base::sort()
  
  final_df <-
    dplyr::mutate(.data = df, 
      cell_line = base::factor(x = cell_line, levels = cell_lines),
      condition = base::factor(x = condition, levels = conditions),
      cl_condition = base::factor(x = cl_condition, levels = cl_conditions)
    )
  
  phases <- object@set_up$phases
  
  measurements <- object@set_up$measurement_string
  
  conditions <- 
    stringi::stri_split(str = base::unique(final_df$condition), regex = "->")
  
  phase_starts <-
    purrr::map_dbl(.x = phases, .f = ~ base::which(x = measurements == .x))
  
  # if more than one phase exists return list of data.frames named by phase
  if(base::length(phase_starts) > 1){
    
    phase_endings <- 
      phase_starts[2:base::length(phase_starts)] %>% 
      base::append(x = ., values = base::max(final_df$frame)) %>% 
      purrr::set_names(nm = base::names(phases))
    
    phase_info <-
      purrr::pmap(
        .l = list(x = phase_starts, y = phase_endings, z = base::seq_along(phases)),
        .f = function(x, y, z){ list("start" = x, "end" = y, "index" = z)}
        ) %>% 
      purrr::set_names(nm = base::names(phases))
    
    track_list <- 
      purrr::map(.x = phase_info, 
                 .f = function(phase){
                   
                   phase_index <- phase$index
                   phase_start <- phase$start
                   phase_end <- phase$end
                   
                   phase_pattern <- stringr::str_c("^", phase_index, "\\.",sep = "")
                   
                   df <-
                     dplyr::filter(final_df, frame >= {{phase_start}} & frame < {{phase_end}})
                   
                   condition_split <- 
                     stringr::str_split_fixed(df$condition, pattern = " -> ", n = base::length(phase_endings)) 
                   
                   condition_vec <- 
                     stringr::str_remove_all(string = base::as.character(condition_split[,phase_index]), pattern = phase_pattern)
                   
                   df$condition <- condition_vec
                   df$cl_condition <- stringr::str_c(df$cell_line, df$condition, sep = " & ")
                   df$phase <- english::ordinal(phase_index)
                   
                   base::return(df)
                   
                 }) %>% 
      magrittr::set_names(value = base::names(phase_endings))
    
  } else { # if only one phase return list of one data.frame named "first"
    
    final_df$phase = "first"
    
    track_list <- base::list("first" = final_df)
    
  }
  
  base::return(track_list)
  
}




#' @title Show shiny - notifications
#'
#' @param in_shiny Logical value. 
#' @param ui Given to \code{shiny::showNotification()}.
#' @param type Given to \code{shiny::showNotification()}.
#' @param ... More arguments given \code{shiny::showNotification()}.
#'
#' @return A shiny notification.

shiny_fdb <- function(in_shiny, ui, type = "message", ...){
  
  if(base::isTRUE(in_shiny)){
    
    shiny::showNotification(ui = ui, type = type, ...)
    
  }
  
}


#' @title Summarize tracking quality
#'
#' @param track_df A track data.frame

quality_check_summary_shiny <- function(track_df){
  
  dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::summarise(
      last_meas = base::max(frame), 
      first_meas = base::min(frame), 
      total_meas = dplyr::n(), 
      skipped_meas = base::length(first_meas:last_meas) - total_meas
    )
  
}


#' @title Visualize the well plate
#'
#' @param wp_df A well-plate data.frame.
#' @param selected_wells_df A subsetted well-plate data.frame or NULL.
#' @param aes_fill Character value. Variable of \code{wp_df} to map on the 
#' fill-aesthetic.
#' @param aes_color Character value. Variable of \code{wp_df} to map on the 
#' color-aesthetic.
#' @param pt_size Numeric value. Size of points that represent the wells. 
#' @param pt_stroke Numeric value. Size of border that indicates what has been 
#' mapped on the color-aesthetic.
#' @param border Numeric value. Distance between the outer wells and the plate's
#' margin.
#'
#' @inherit ggplot_return return
#'

plot_well_plate_shiny <- function(wp_df,
                                  selected_wells_df = NULL,
                                  aes_fill,
                                  fill_values = NULL, 
                                  fill_guide = FALSE,
                                  aes_color,
                                  color_values,
                                  pt_size = 13.5,
                                  pt_stroke = 2,
                                  border = 0.75){
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(base::is.data.frame(selected_wells_df)){
    
    geom_point_add_on <-  
      ggplot2::geom_point( data = selected_wells_df, fill = "red",
                           size = pt_size, shape = 21, alpha = 1,
                           stroke = pt_stroke ) 
    
  } else {
    
    geom_point_add_on <- NULL
    
  }
  
  if(base::is.null(fill_values)){
    
    fill_add_on <- confuns::scale_color_add_on(aes = "fill",
                                variable = wp_df[[aes_fill]],
                                clrp.adjust = c("unknown" = "lightgrey", "unknown & unknown" = "lightgrey"),
                                clrp = "milo", 
                                guide = FALSE) 

  } else {
    
    fill_add_on <- ggplot2::scale_fill_manual(values = fill_values, drop = FALSE, guide = FALSE) 
    
  }
  
  if(base::isTRUE(fill_guide)){
    
    fill_guide <- ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
    
  } 
  
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    ggplot2::geom_point(
      data = wp_df, 
      mapping = ggplot2::aes(fill = .data[[aes_fill]], color = .data[[aes_color]]),
      size = pt_size, shape = 21, alpha = 0.9, stroke = pt_stroke, 
    ) + 
    geom_point_add_on +
    ggplot2::geom_text(mapping = ggplot2::aes(label = well)) +
    ggforce::geom_mark_rect(
      mapping = ggplot2::aes(x = col_num, y = row_num, color = group), 
      color = "black", size = 1, expand = ggplot2::unit(15, "mm")
    ) +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    #ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    confuns::scale_color_add_on(aes = "color", variable = wp_df[[aes_color]], clrp.adjust = color_values, clrp = "milo") + 
    fill_add_on +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21)), 
      fill = fill_guide
    )
  
  
}

#' @title Quality check histogram
#'
#' @param track_summary_df Output of \code{quality_check_summary()}
#' @param aes_x Character value. 
#' @param aes_fill Character value.
#' @param lab_x Character value.
#' @param lab_fill Character value.

plot_qc_histogram_shiny <- function(track_summary_df, 
                                 aes_x = "skipped_meas", 
                                 lab_x = "Measurements",
                                 legend_position = "none"){
  
  labels_breaks <-
    dplyr::pull(track_summary_df, var = {{aes_x}}) %>% 
    base::unique()
  
  ggplot2::ggplot(data = track_summary_df, mapping = ggplot2::aes(x = .data[[aes_x]])) + 
    ggplot2::geom_histogram(position = "stack", color = "black", binwidth = 1) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey"), 
      axis.text.x = ggplot2::element_text(vjust = 5), 
      plot.title = ggplot2::element_text(face = "bold", size = 15), 
      legend.position = "none"
    ) + 
    ggplot2::scale_x_continuous(labels = labels_breaks, breaks = labels_breaks) +
    ggplot2::labs(x = lab_x, y = NULL)
  
}


#' @title Quality check barplot
#'
#' @param df A data.frame 

plot_qc_barplot_shiny <- function(df, aes_x, aes_fill, bar_position){
  
  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = forcats::fct_infreq(.data[[aes_x]]))) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[aes_fill]]),
                      color = "black", position = bar_position) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  subtitle = stringr::str_c("n = ", base::nrow(df)))
  
}
