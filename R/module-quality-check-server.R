#' @title Server logic: Quality Check
#' 
#' @description Server logic to be used as input for \code{module}-argument
#' of function \code{shiny::moduleServer()}.
#' 
#' @param id Namespace ID
#' @param ld_input A reactive and named list. See value of \code{moduleLoadDataServer()}.
#'
#' @return A named list:

moduleQualityCheckServer <- function(id, object){
  
  shiny::moduleServer(
    id = id,
    module = function(input, output, session){
      

# Reactive values ---------------------------------------------------------
      
      phase_max_frames <- shiny::reactiveVal(value = purrr::map(.x = object@cdata$tracks, .f = ~ base::max(.x[["frame"]])))
      
      track_df <- shiny::reactiveVal(value = purrr::map_df(.x = object@cdata$tracks, .f = ~ .x) %>% dplyr::ungroup())
      
      filter <- shiny::reactiveValues(
        
        "total_meas_values"= base::character(1),
        "skipped_meas_values"= base::character(1),
        "first_meas_values"= base::character(1),
        "last_meas_values"= base::character(1),
        "total_meas_opt"= base::character(1),
        "skipped_meas_opt"= base::character(1),
        "first_meas_opt"= base::character(1),
        "last_meas_opt"= base::character(1)
        
      )
      
      qc_list <- shiny::reactiveValues(
        
        info_list = list(), 
        data_list = list()
        
      )
      
      # -----
      

# Render UIs --------------------------------------------------------------
      
      output$skipped_meas_opt <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::length(filter_skipped_meas()) != 0, 
            message = "Include values interactive."
          )
        )
        
        ns <- session$ns
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("skipped_meas_opt"),
          label = NULL,
          choices = c("Include brushed area" = "include", "Exclude brushed area" = "exclude"),
          inline = TRUE
          )
        
      })
      
      output$first_meas_opt <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::length(filter_first_meas()) != 0, 
            message = "Include values interactive."
          )
        )
        
        ns <- session$ns
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("first_meas_opt"),
          label = NULL,
          choices = c("Include brushed area" = "include", "Exclude brushed area" = "exclude"),
          inline = TRUE
        )
        
      })
      
      output$last_meas_opt <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::length(filter_last_meas()) != 0, 
            message = "Include values interactive."
          )
        )
        
        ns <- session$ns
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("last_meas_opt"),
          label = NULL,
          choices = c("Include brushed area" = "include", "Exclude brushed area" = "exclude"),
          inline = TRUE
        )
        
      })
      
      output$total_meas_opt <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::length(filter_total_meas()) != 0, 
            message = "Include values interactive."
          )
        )
        
        ns <- session$ns
        
        shinyWidgets::prettyRadioButtons(
          inputId = ns("total_meas_opt"),
          label = NULL,
          choices = c("Include brushed area" = "include", "Exclude brushed area" = "exclude"),
          inline = TRUE
        )
        
      })
      
      
      # -----

# Observe events ----------------------------------------------------------
      
      oe <- shiny::observeEvent(input$qc_save_and_proceed, {
        
        checkpoint(
          evaluate = base::is.data.frame(track_df()) && base::nrow(track_df()) > 0, 
          case_false = "no_data_read_in"
        )
        
        checkpoint(
          evaluate = base::length(remaining_cell_ids()) > 0, 
          case_false = "no_cells_remaining"
        )
        
        object@cdata$tracks <- 
          purrr::map(.x = base::names(object@set_up$phases), .f = ~ dplyr::filter(remaining_cells_df(), phase == {{.x}})) %>% 
          purrr::set_names(nm = base::names(object@set_up$phases))
        
        qc_list$object <- object
        
        qc_list$proceed <- input$qc_save_and_proceed
        
        shiny_fdb(in_shiny = TRUE, ui = glue::glue("Results have been saved.  Click on 'Return Cypro Object' and proceed with processData()."))
        
        
      })
      
      # ----- 
      
      

# Reactive expressions ----------------------------------------------------

      # data quality data.frame 
      
      track_summary_df <- shiny::reactive({
        
        shiny::validate(
          shiny::need(
            expr = base::nrow(track_df()) > 0, 
            message = "No files have been loaded yet."
          )
        )
        
        quality_check_summary_shiny(track_df = track_df())
        
      })
      
      
      # interactively set filter reqiurements ---
      
      filter_skipped_meas <- shiny::reactive({
        
        shiny::req(input[["brush_skipped_meas"]])
        
        shiny::brushedPoints(track_summary_df(), input[["brush_skipped_meas"]], xvar = "skipped_meas")
        
      })
      
      filter_first_meas <- shiny::reactive({
        
        shiny::req(input[["brush_first_meas"]])
        
        shiny::brushedPoints(track_summary_df(), input[["brush_first_meas"]], xvar = "first_meas")
        
      })
      
      filter_last_meas <- shiny::reactive({
        
        shiny::req(input[["brush_last_meas"]])
        
        shiny::brushedPoints(track_summary_df(), input[["brush_last_meas"]], xvar = "last_meas")
        
      })
      
      filter_total_meas <- shiny::reactive({
        
        shiny::req(input[["brush_total_meas"]])
        
        shiny::brushedPoints(track_summary_df(), input[["brush_total_meas"]], xvar = "total_meas")
        
      })
      
      # ---
      
      # remaining cell ids ---
      
      remaining_cell_ids <- shiny::eventReactive(input$apply_filter,{
        
        ###---  1.) prepare key objects
        check <- list()
        check[["num_filter_applied"]] <- 4
        
        df <- track_df()
        
        ###---  2.) apply filter in a inclusive or exclusive way if filter results exist (length > 0)
        for(i in 1:4){
          
          criterion <- imp_filter_criteria[i]
          
          filter_results <- shiny::brushedPoints(df = track_summary_df(),
                                                 brush = input[[stringr::str_c("brush_", criterion)]],
                                                 xvar = criterion)
          
          ##-- 2.2 make sure the respective filter criterion was applied 
          if(!base::is.null(input[[stringr::str_c(criterion,"_opt")]]) & base::nrow(filter_results) != 0){
            
            ##-- 2.1) call the respective reactive {filter} expression and obtain the data frame
            #- check how to apply the filter (include cell ids vs exclude cell ids)
            if(input[[stringr::str_c(criterion,"_opt")]] == "include"){
              
              df <- dplyr::filter(.data = df, cell_id %in% filter_results$cell_id)
              
              #- store the values the filter allowed
              filter[[stringr::str_c(criterion, "_opt")]] <- "(Included)"
              
              res <- filter_results[,criterion] %>% base::range() %>% base::unique()
              
              filter[[stringr::str_c(criterion, "_values")]]  <-
                base::ifelse(test = base::length(res) == 1,
                             yes = base::as.character(res),
                             no = stringr::str_c(res[1], res[2], sep = " to "))
              
              
            } else if(input[[stringr::str_c(criterion,"_opt")]] == "exclude"){
              
              df <- dplyr::filter(.data = df, !cell_id %in% filter_results$cell_id)
              
              #- store the values the filter allowed
              filter[[stringr::str_c(criterion, "_opt")]] <- "(Excluded)"
              
              res <- filter_results[,criterion] %>% base::range() %>% base::unique()
              
              filter[[stringr::str_c(criterion, "_values")]]  <-
                base::ifelse(test = base::length(res) == 1,
                             yes = base::as.character(res),
                             no = stringr::str_c(res[1], res[2], sep = " to ")) 
              
            }
            
            ##-- 2.2 if the filter was not applied:
          } else {
            
            filter[[stringr::str_c(criterion, "_opt")]] <- "Not applied"
            filter[[stringr::str_c(criterion, "_values")]] <- "Not applied"
            
            check[["num_filter_applied"]] <- (check[["num_filter_applied"]] - 1)
            
          }
          
        }
        
        ###--- 5.) return vector
        base::return(df$cell_id)
        
      })
      
      remaining_cells_plot <- shiny::reactive({
        
        shiny::validate(
          shiny::need(
            expr = base::nrow(track_df()) > 0, 
            message = "No files have been loaded yet."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = base::length(remaining_cell_ids()) > 0, 
            message = "The filter criteria discard all cells. At least one cell must remain."
          )
        )
        
        filtered_df <- 
          dplyr::filter(.data = track_df(), cell_id %in% remaining_cell_ids()) %>% 
          dplyr::select(cell_id, cell_line) %>% 
          dplyr::distinct()

        plot_qc_barplot_shiny(df = filtered_df, 
                              aes_x = "cell_line", 
                              aes_fill = "cell_line", 
                              bar_position = "stack") + 
          confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = "milo")
        
      })
      
      remaining_cells_df <- shiny::reactive({
        
        track_df() %>% 
          dplyr::filter(cell_id %in% remaining_cell_ids())
        
      })
      
      # ---
      
      
      
      
      # -----
      
      

# Plot outputs ------------------------------------------------------------
      
      output$total_meas <- shiny::renderPlot({
        
        shiny::validate(
          shiny::need(
            expr = base::is.data.frame(track_summary_df()),
            message = "No files have been loaded yet."
          )
        )
        
        plot_qc_histogram_shiny(
          track_summary_df = track_summary_df(), 
          aes_x = "total_meas", 
          lab_x = "Measurements"
        )
        
      })
      
      output$skipped_meas <- shiny::renderPlot({
        
        shiny::validate(
          shiny::need(
            expr = base::is.data.frame(track_summary_df()),
            message = "No files have been loaded yet."
          )
        )
        
        plot_qc_histogram_shiny(
          track_summary_df = track_summary_df(), 
          aes_x = "skipped_meas", 
          lab_x = "Measurements",
          legend_position = c(0.85, 0.75)
        )
        
      })
      
      output$first_meas <- shiny::renderPlot({
        
        shiny::validate(
          shiny::need(
            expr = base::is.data.frame(track_summary_df()),
            message = "No files have been loaded yet."
          )
        )
        
        plot_qc_histogram_shiny(
          track_summary_df = track_summary_df(), 
          aes_x = "first_meas", 
          lab_x = "nth Measurement"
        )
        
      })
      
      output$last_meas <- shiny::renderPlot({
        
        shiny::validate(
          shiny::need(
            expr = base::is.data.frame(track_summary_df()),
            message = "No files have been loaded yet."
          )
        )
        
        plot_qc_histogram_shiny(
          track_summary_df = track_summary_df(), 
          aes_x = "last_meas", 
          lab_x = "nth Measurement"
        )
        
      })
      
      output$remaining_cells_plot <- shiny::renderPlot({
        
        remaining_cells_plot()
        
      })
      
  # -----
  

# Return value ------------------------------------------------------------

  # currently not in use !!! ------- start
      
  return_value <- shiny::reactive({ 
    
    filter_fdb <- 
      purrr::map(.x = base::names(filter), ~ base::return(filter[[.x]])) %>% 
      purrr::set_names(nm = base::names(filter))
    
    rv <- 
    list(info_list = qc_list$info_list, 
         data = qc_list$data, 
         filter = filter_fdb
         )
    
    assign(x = "rv_quality_check", value = rv, .GlobalEnv)
    
    return(rv)
    
    })
      
  # currently not in use !!! ------- start
  
  base::return(qc_list)
  
  })
  
}
