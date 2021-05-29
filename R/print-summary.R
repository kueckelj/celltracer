

#' @title Print object summary 
#' 
#' @description Prints an overview of the objects content into the console.
#'
#' @inherit argument_dummy params
#'
#' @return A printed message via \code{writeLines()}.
#' @export
#'
printSummary <- function(object){
  
  check_object(object)
  
  summary_list <- list()
  
  summary_list$class <- "An object of class 'cypro'.\n\n"
  
  summary_list$object_name <-stringr::str_c("Name: ", object@name)
  
  summary_list$exp_type <- stringr::str_c("\nType: ", pretty_exp_types[object@set_up$experiment_type])
  
  summary_list$n_cells <- stringr::str_c("\nNumber of Cells: ", nCells(object))

# conditions --------------------------------------------------------------
  
  if(multiplePhases(object)){
    
    summary_list$conditions <- 
      purrr::map_chr(
        .x = getPhases(object), 
        .f = function(phase){
          
          all_conditions <- getConditions(object, phase = phase)
          
          text <- 
            stringr::str_c(
              "\n ", confuns::make_capital_letters(phase), " phase: '", 
              glue::glue_collapse(all_conditions, sep = "', '", last = "' and '"), 
              "'"
            )
          
          base::return(text)
          
        }
      ) %>% 
      stringr::str_c(collapse = "") %>% 
      stringr::str_c("\nConditions:", .)
    
    
  } else {
    
    summary_list$conditions <- 
      getConditions(object) %>% 
      glue::glue_collapse(sep = "', '", last = "' and '") %>% 
      stringr::str_c("\nConditions: ", "'", . , "'") 
    
  }
  
  

# cell lines  -------------------------------------------------------------

  summary_list$cell_lines <- 
    getCellLines(object) %>% 
    glue::glue_collapse(sep = "', '", last = "' and '") %>% 
    stringr::str_c("\nCell Lines: ", "'", . , "'")   
  

# well plates -------------------------------------------------------------

  summary_list$well_plates <- 
    getWellPlateNames(object) %>% 
    glue::glue_collapse(sep = "', '", last = "' and '") %>% 
    stringr::str_c("\nWell Plates: ", "'", . , "'")   

  
# variable sets -----------------------------------------------------------
  
  if(base::identical(object@variable_sets, base::list())){
    
    summary_list$variable_sets <- "\nNo variables sets have been defined yet."
    
  } else {
    
    summary_list$variables_sets <- 
      getVariableSetNames(object) %>% 
      glue::glue_collapse(sep = "', '", last = "' and '") %>% 
      stringr::str_c("\nVariable Sets: '", ., "'") 
    
  }
  
# print output ------------------------------------------------------------
  
  purrr::flatten_chr(summary_list) %>% 
    stringr::str_c(collapse = "") %>% 
    base::writeLines()
  
}

#' @rdname printSummary
#' @export
printAnalysisSummary <- function(object, slots = c("dim_red", "clustering")){
  
  make_cutting_line() %>% 
    base::writeLines()
  
  confuns::check_one_of(
    input = slots, 
    against = base::names(analysis_methods)
  )
  
  if(base::identical(object@analysis, base::list())){
    
    base::writeLines("No analysis has been conducted yet.")
    
  } else {
    
    base::writeLines("Conducted analysis:\n\n")
    
    for(slot in slots){
      
      print_analysis_info_dfs(object = object, slot = slot)
      
    }
    
  }
  
}





# helper ------------------------------------------------------------------

print_analysis_info_dfs <- function(object, slot){
  
  vset_names <- getVariableSetNames(object)
  
  if(base::is.character(vset_names)){
    
    variable_sets <- 
      glue::glue_collapse(vset_names, sep = "', '", last = "' and '") %>% 
      stringr::str_c("\nVariable Sets: ", "'", . , "'") 
    
    n_vsets <- base::length(vset_names)
    
    methods <- analysis_methods[[slot]]
    
    n_methods <- base::length(methods)
    
    vset_dim_red <- 
      base::matrix(data = "", nrow = n_methods, ncol = n_vsets) %>% 
      base::as.data.frame() %>% 
      magrittr::set_colnames(value = vset_names) %>% 
      magrittr::set_rownames(value = methods)
    
    if(multiplePhases(object)){
      
      output <- 
        purrr::map(
          .x = getPhases(object), 
          .f = function(phase){
            
            results_exist <- 
              purrr::map(.x = methods, .f = ~ get_analysis_names(object, slot = slot, method = .x, phase = phase)) %>% 
              purrr::set_names(methods)
            
            for(vset in vset_names){
              
              for(method in methods){
                
                if(vset %in% results_exist[[method]]){
                  
                  vset_dim_red[method, vset] <- "Yes"
                  
                } else {
                  
                  vset_dim_red[method, vset] <- "No"
                  
                }
                
              }
              
            }
            
            output <- tibble::rownames_to_column(vset_dim_red, var = slot)
            
            base::return(output)
            
          }
        ) %>% 
        purrr::set_names(nm = getPhases(object)) 
      
      for(phase in base::names(output)){
        
        spacer <- base::ifelse(phase == "first", yes = "", no = "\n\n")
        
        base::writeLines(stringr::str_c(spacer, confuns::make_capital_letters(phase), " phase:"))
        
        base::print(knitr::kable(x = output[[phase]]))
        
      }
      
    } else {
      
      results_exist <- 
        purrr::map(.x = methods, .f = ~ get_analysis_names(object, slot = slot, method = .x)) %>% 
        purrr::set_names(methods)
      
      for(vset in vset_names){
        
        for(method in methods){
          
          if(vset %in% results_exist[[method]]){
            
            vset_dim_red[method, vset] <- "Yes"
            
          } else {
            
            vset_dim_red[method, vset] <- "No"
            
          }
          
        }
        
      }
      
      output <- tibble::rownames_to_column(vset_dim_red, var = slot)
      
      base::print(knitr::kable(x = output))
      
    }
    
  }
  
  make_cutting_line() %>% 
    stringr::str_c("\n\n", .) %>% 
    base::writeLines()
  
}

get_analysis_names <- function(object, slot, method, phase = NULL, if_null = ""){
  
  assign_default(object)
  
  if(base::is.null(object@analysis[[slot]])){
    
    out <- if_null
    
  } else if(multiplePhases(object)){
    
    out <- 
      object@analysis[[slot]][[method]] %>% 
      purrr::discard(.x = ., .p = ~ base::is.null(.x[[phase]])) %>% 
      base::names()
    
  } else {
    
    out <- base::names(object@analysis[[slot]][[method]])
    
  }
  
  if(base::is.null(out)){ out <- if_null }
  
  base::return(out)
  
}





