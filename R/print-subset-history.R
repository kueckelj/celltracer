

#' @title Print subset information 
#' 
#' @description If the object is the results of one or more 
#' subsetting processes you can print the respective information 
#' into the console to keep track of the objects origin. 
#'
#' @inherit argument_dummy params
#'
#' @return A printed message via \code{writeLines()}.
#' @export
#'
printSubsetHistory <- function(object){
  
  check_object(object)
  
  subset_info <- object@information$subset
  
  if(base::is.null(subset_info)){
    
    base::writeLines("Provided celltracer object has not been subsetted yet.")
    
  } else if(confuns::is_list(subset_info)){
    
    output_list <- 
      purrr::imap_chr(
        .x = subset_info, 
        .f = function(info, slot_name){
          
          fn_name <- stringr::str_c("subset_history_", info$by)
          
          text_out <- 
            confuns::call_flexibly(
              fn = fn_name, 
              fn.ns = "celltracer", 
              default = list(info = info, slot_name = slot_name)
            )
          
          return(text_out)
          
        }
      )
    
    base::writeLines(output_list)
    
  }
  
}



# basic helper ------------------------------------------------------------

make_cutting_line <- function(){
  
  base::rep("-", 50) %>% 
    stringr::str_c(collapse = "") %>% 
    stringr::str_c("\n", ., "\n")
  
}

make_header <- function(info, slot_name){
  
  headline <- 
    confuns::make_capital_letters(slot_name) %>% 
    stringr::str_c(., "Subsetting:\n", sep = " ")
  
  if(slot_name != "first"){
  
    cutting_line <- make_cutting_line()

    
  } else {
    
    cutting_line <- ""
    
  }
  
  by <- stringr::str_c("By: ", confuns::make_capital_letters(info$by))
  
  
  res <- 
    stringr::str_c(
      cutting_line, headline, by, sep = "\n"
    )
  
  base::return(res)
  
}

make_footer <- function(info, slot_name){
  
  n_remaining <- stringr::str_c("Number remaining: ", info$n_remaining)
  
  parent <- stringr::str_c("Parent object: ", info$parent_object)
  
  new <- stringr::str_c("New object: ", info$new_object)
  
  res <- stringr::str_c(parent, new, n_remaining, sep = "\n")
  
  base::return(res)
  
}



# specific helper ---------------------------------------------------------

subset_history_cell_id <- function(info, slot_name){
  
  header <- make_header(info, slot_name)
  
  footer <- make_footer(info, slot_name)
  
  text <- stringr::str_c(header, footer, sep = "\n")
  
  base::return(text)
  
}

subset_history_cell_lines <- function(info, slot_name){
  
  header <- make_header(info, slot_name)
  
  footer <- make_footer(info, slot_name)
  
  body <-
    stringr::str_c("Kept: '", glue::glue_collapse(info$cell_lines, sep = "', '", last = "' and '"), "'") 
  
  text <- 
    stringr::str_c(header, body, footer, sep = "\n")
  
  base::return(text)
  
  
}

subset_history_conditions <- function(info, slot_name){
  
  header <- make_header(info, slot_name)
  
  footer <- make_footer(info, slot_name)
  
  phase <- stringr::str_c("Phase: ", info$phase)
  
  conditions <-
    stringr::str_c("Kept: '", glue::glue_collapse(info$conditions, sep = "', '", last = "' and '"), "'") 
  
  text <- 
    stringr::str_c(header, phase, conditions,footer, sep = "\n")
  
  base::return(text)
  
}

subset_history_cluster <- function(info, slot_name){
  
  header <- make_header(info, slot_name)
  
  footer <- make_footer(info, slot_name)
  
  phase <- stringr::str_c("Phase: ", info$phase)
  
  cluster_var <- 
    stringr::str_c("Cluster Name: '", info$cluster_variable, "'")
  
  cluster <-
    stringr::str_c("Kept: '", glue::glue_collapse(info$cluster, sep = "', '", last = "' and '"), "'") 
  
  text <- 
    stringr::str_c(header, phase, cluster_var, cluster, footer, sep = "\n")
  
  base::return(text)
  
}

subset_history_filter <- function(info, slot_name){
  
  header <- make_header(info, slot_name)
  
  footer <- make_footer(info, slot_name)
  
  phase <- stringr::str_c("Phase: ", info$phase)
  
  requirements <- 
    purrr::map2_chr(.x = info$requirements, 
                    .y = base::seq_along(info$requirements),
                    last_req = base::length(info$requirements),
                    .f = function(req, index, last_req){
                      
                      call_obj <- rlang::quo_get_expr(req)
                      
                      call_string <- bbmle::call.to.char(x = call_obj)
                      
                      new_line <- base::ifelse(index < last_req, yes = "\n", no = "")
                      
                      stringr::str_c(" ",index, ". ", call_string, new_line)
                      
                    })
  
  text <- 
    stringr::str_c(header, phase, "Requirements:", requirements, footer, sep = "\n")
  
  base::return(text)
  
}


