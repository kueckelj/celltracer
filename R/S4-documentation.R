


# celltracer-object -------------------------------------------------------

cto <- setClass(Class = "cto", 
                slots = c(
                  analysis = "list",
                  compatibility = "list",
                  cdata = "list",
                  vdata = "list",
                  default = "list",
                  information = "list",
                  name = "character",
                  set_up = "list",
                  well_plates = "list", 
                  variable_sets = "list",
                  version = "list"
                )
)

# show method
setMethod(f = "show", signature = "cto", definition = function(object){
            
            check_object(object)
            
            summary_list <- list()
            
            summary_list$class <- "An object of class 'cto' - celltracer object.\n\n"
            
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
            
          })
