
#' @title Create a Cell Tracer Object
#' 
#' @description This function allows for interactive creation of a 
#' cell-tracer-object. 
#'
#' @return A cell tracer object.
#' @export
#'

initiateCTO <- function(){

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
                                      moduleLoadDataUI(id = "ld"),
                                      moduleQualityCheckUI(id = "qc"),
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shinyWidgets::actionBttn(inputId = "return_cto",
                                                                               label = "Return Cell-Tracer-Object", 
                                                                               color = "success", 
                                                                               style = "gradient")
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
        
        experiment_design <-
          moduleExperimentDesignServer(id = "ed")
        
        data <-
          moduleLoadDataServer(id = "ld", ed_input = experiment_design)
        
        quality_check <-
          moduleQualityCheckServer(id = "qc", ld_input = data)
        
        # TEST OE-BUTTON #
        oe <- shiny::observeEvent(input$return_cto, {
          
          checkpoint(
            evaluate = !base::is.null(quality_check()$data), 
            case_false = "no_qc_data"
          )
          
          cto_list <- list(
            set_up = experiment_design()$set_up, # experiment info (list)
            track_df = quality_check()$data, # filtered tracks (data.frame)
            filter = quality_check()$filter,
            wp_info = data()$all_wp_lists # well plate info (list)
          )
          
          assign("ct_list", value = cto_list, .GlobalEnv)
          
          cto <- compileCto(ct_list = ct_list,
                            in_shiny = TRUE, 
                            compute_umap = FALSE)
          
          shiny::stopApp(returnValue = cto)
          
        })
        
      }
    )
  )
  
}


#' @title Compile the cell tracer object

compileCto <- function(ct_list, in_shiny = FALSE, compute_umap = FALSE){
  
  cto <- methods::new(Class = "cto")
  
  # Other slots -------------------------------------------------------------
  
  cto@set_up <-
    purrr::discard(ct_list$set_up, .p = ~ base::is.list(.x))
  
  cto@set_up$qualtiy_check <- ct_list$set_up$filter
  
  cto@cluster_info <- list()
  
  cto@wp_info <-
    purrr::map(.x = ct_list$wp_info, "wp_df_eval")
  
  cto@storage_info <- 
    purrr::map(.x = ct_list$wp_info, ~ .x[storage_slots])
  
  cto@background_info <- list()
  
  # -----
  
  # Slot: Data --------------------------------------------------------------
  
  shiny_fdb(in_shiny, ui = "Computing cell statistics.", type = "message")
  
  track_df <- 
    dplyr::mutate(.data = ct_list$track_df, 
                  frame_num = frame, 
                  frame_time = frame * cto@set_up$itvl,
                  frame_itvl = stringr::str_c(frame_time, cto@set_up$itvl_u, sep = " "),
                  frame = NULL) %>% 
    dplyr::filter(frame_num <= cto@set_up$nom)
  
  if(cto@set_up$tmt_start %in% c("From beginning", "No treatment")){

    stat_list <- list("entire" = compute_stat(track_df))
    track_list <- list("entire" = track_df)
    
  } else {
   
    tmt_start <- 
      stringr::str_remove(string = cto@set_up$tmt_start, pattern = cto@set_up$itvl_u) %>% 
      base::as.numeric()
    
    track_df <- 
      dplyr::mutate(track_df, tmt = dplyr::if_else(frame_time <= {{tmt_start}}, "before_tmt", "first_tmt"))
    
    # create data lists
    track_list <- 
      purrr::map(
        .x = c("before_tmt", "first_tmt"), 
        .f = ~ hlpr_create_track_list(phase = .x, track_df = track_df)
      ) %>% 
      purrr::set_names(nm = c("before_tmt", "first_tmt"))
    
    stat_list <- purrr::map(.x = track_list, .f = compute_stat)

  }
  
  meta_df <- 
    purrr::map_df(.x = stat_list, .f = ~ .x) %>% 
    dplyr::select(dplyr::all_of(x = meta_variables)) %>% 
    dplyr::distinct()

  cto@data <- 
    list(
      "tracks" = purrr::map(.x = track_list,
                            .f = ~ dplyr::select(.x, -dplyr::all_of(base::colnames(meta_df)), cell_id)), 
      "stats" = purrr::map(.x = stat_list,
                           .f = ~ dplyr::select(.x,  -dplyr::all_of(base::colnames(meta_df)), cell_id)), 
      "meta" = dplyr::mutate(meta_df, dplyr::across(-cell_id, .fns = base::as.factor)), 
      "cluster" = purrr::map(.x = stat_list, 
                             .f = ~ dplyr::select(.x, cell_id))
    )
  
  if(base::isTRUE(compute_umap)){
    
    shiny_fdb(in_shiny, ui = "Computing UMAP data.", type = "message")
    
    cto <- computeUmap(cto = cto)
    
  } else {
    
    shiny_fdb(in_shiny, ui = "Skip computing UMAP data.", type = "message")
    
  }

  
  # -----
  
  shiny_fdb(in_shiny, ui = "Done.", type = "message")
  
  base::return(cto)
  
  
}

