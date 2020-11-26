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

evaluate_file_availability <- function(wp_list, recursive = TRUE, keep_filetype = "csv$"){
  
  directory <- wp_list$directory
  wp_df <- wp_list$wp_df
  relevant_wp_df <- dplyr::filter(.data = wp_df, information_status == "Complete")

  ignore_filetypes <- filetypes[filetypes != keep_filetype]
  
  ipw <- wp_df$ipw %>% base::unique()
  
  # filter all subsequent directories for the 'file_regex' pattern
  wp_directories <-
    base::list.files(path = directory, recursive = recursive, full.names = TRUE) %>% 
    stringr::str_subset(pattern = file_regex)

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



