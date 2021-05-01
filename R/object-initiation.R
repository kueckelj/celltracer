#' Title
#'
#' @return
#' @export
#'
designExperiment <- function(){
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleExperimentDesignUI(id = "ed"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cto")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ed_results <-
          moduleExperimentDesignServer(id = "ed", usage = "in_function")
        
        
        output$return_cto <- shiny::renderUI({
          
          ed_list <- shiny::reactiveValuesToList(ed_results)
          
          if(base::isTRUE(ed_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(inputId = "return_cto",
                                   label = "Return Celltracer Object", 
                                   color = color, 
                                   style = "gradient")
          
        })
        
        oe <- shiny::observeEvent(input$return_cto, {
          
          ed_list <- shiny::reactiveValuesToList(ed_results)
          
          check <- base::tryCatch({
            
            base::class(ed_list$object) == "cto"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cto")
          
          ct_object <- ed_list$object
          
          ct_object@set_up$progress$experiment_design <- TRUE
          
          shiny::stopApp(returnValue = ct_object)
          
        })
        
      }
    )
  )
  
}


#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
loadData <- function(object){
  
  check_object(object, set_up_req = "experiment_design")
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleLoadDataUI(id = "ld"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cto")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ld_results <-
          moduleLoadDataServer(id = "ld", object = object)
        
        output$return_cto <- shiny::renderUI({
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          if(base::isTRUE(ld_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(inputId = "return_cto",
                                   label = "Return Celltracer Object", 
                                   color = color, 
                                   style = "gradient")
          
        })
        
        
        oe <- shiny::observeEvent(input$return_cto, {
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          check <- base::tryCatch({
            
            base::class(ld_list$object) == "cto"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cto2")
          
          ct_object <- ld_list$object
          
          ct_object@set_up$progress$load_data <- TRUE
          
          shiny::stopApp(returnValue = ct_object)
          
        })
        
      }
    )
  )
  
}


#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
checkDataQuality <- function(object){
  
  check_object(object, set_up_req = "load_data")
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleQualityCheckUI(id = "qc"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cto")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        qc_results <-
          moduleQualityCheckServer(id = "qc", object = object)
        
        output$return_cto <- shiny::renderUI({
          
          qc_list <- shiny::reactiveValuesToList(qc_results)
          
          if(base::isTRUE(qc_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(inputId = "return_cto",
                                   label = "Return Celltracer Object", 
                                   color = color, 
                                   style = "gradient")
          
        })
        
        oe <- shiny::observeEvent(input$return_cto, {
          
          qc_list <- shiny::reactiveValuesToList(qc_results)
          
          check <- base::tryCatch({
            
            base::class(qc_list$object) == "cto"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cto2")
          
          ct_object <- qc_list$object
          
          ct_object@set_up$progress$quality_check <- TRUE
          
          shiny::stopApp(returnValue = ct_object)
          
        })
        
      }
    )
  )
  
  
  
  
}




#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
processData <- function(object, verbose = TRUE){
  
  check_object(object, set_up_req = "check_quality")
  
  # post process track list
  confuns::give_feedback(msg = "Processing tracking data.", verbose = verbose)
  
  object@data$tracks <- 
    purrr::map2(.x = object@data$tracks,
                .y = getPhases(object),
                itvl = object@set_up$itvl,
                itvl_u = object@set_up$itvl_u,
                software = object@set_up$software,
                verbose = verbose,
                .f = function(df, phase, itvl, itvl_u, software, verbose){
                 
                 mutated_df <- 
                   dplyr::mutate(
                     .data = df, 
                     frame_num = frame,
                     frame_time = frame * itvl,
                     frame_itvl = stringr::str_c(frame_time, itvl_u, sep = " "),
                     frame = NULL) %>% 
                   dplyr::filter(frame_num <= object@set_up$nom)
                 
                 if(software$name == "cell_profiler" && base::isTRUE(software$tracking)){
                   
                   confuns::give_feedback(msg = glue::glue("Computing migration data from {phase} phase."), verbose = verbose)
                   
                   mutated_df <- 
                     dplyr::group_by(.data = mutated_df, cell_id) %>% 
                     dplyr::mutate(
                       dfo = compute_distances_from_origin(x_coords, y_coords),
                       dflp = compute_distances_from_last_point(x_coords, y_coords), 
                       speed = dflp / object@set_up$itvl
                     )
                   
                   final_df <-
                     dplyr::select(.data = mutated_df,
                                   cell_id, x_coords, y_coords, speed, dfo, dflp,
                                   dplyr::starts_with(match = "frame"), 
                                   dplyr::everything() 
                     ) %>% 
                     dplyr::select(-dplyr::all_of(meta_variables))
                   
                 } else {
                   
                   final_df <- mutated_df
                   
                 }
                 
                 base::return(final_df)
                 
    }) %>% 
    purrr::set_names(nm = getPhases(object))
  
  # compute statistics 
  object@data$stats <- 
    purrr::map2(.x = object@data$tracks,
                .y = getPhases(object),
                software = object@set_up$software,
                .f = compute_cell_stats,
                verbose = verbose
                ) %>% 
    purrr::set_names(nm = getPhases(object))
  
  
  # create meta data 
  confuns::give_feedback(msg = "Creating cell meta data.", verbose = verbose)
  
  object@data$meta <- 
    purrr::map2(.x = object@data$tracks, 
                .y = getPhases(object), 
                .f = hlpr_create_meta_data, 
                verbose = verbose) %>% 
    purrr::set_names(nm = getPhases(object))
  
  # create cluster data
  object@data$cluster <- 
    purrr::map2(.x = object@data$meta, 
                .y = getPhases(object), 
                .f = function(df, phase){
                  
                  dplyr::select(df, cell_id) %>% 
                    dplyr::mutate(phase = {{phase}})
                  
                }) %>% 
    purrr::set_names(nm = getPhases(object))
  
  object@set_up$progress$process_data <- TRUE
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


