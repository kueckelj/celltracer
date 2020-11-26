moduleAllTracksUI <- function(id, module_width, module_headline){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::column(width = module_width, 
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(width = 12, 
                                    shiny::h4(shiny::strong(module_headline)), 
                                    shiny::plotOutput(outputId = ns("module_plot")), 
                                    shiny::HTML("<br>"), 
                                    plot_and_save(ns = ns),
                                    shiny::HTML("<br>"), 
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("change_order")))
                                    ),
                                    shiny::HTML("<br><br>"), 
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("phase_cluster"))),
                                      hs(4,shiny::uiOutput(outputId = ns("across"))),
                                      hs(4,shiny::uiOutput(outputId = ns("across_subset")))
                                    ), 
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("phase"))),
                                      hs(4,n_cells_numeric_input(ns = ns)),
                                      hs(4,shiny::uiOutput(outputId = ns("frame_time_subset")))
                                      
                                    ), 
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("color_to"))),
                                      hs(4,shiny::selectInput(inputId = ns("linetype"),
                                                              label = "Linetype",
                                                              choices = pretty_linetypes, 
                                                              selected = "solid")
                                         ), 
                                      hs(4,shiny::sliderInput(inputId = ns("linesize"), label = "Linesize", 
                                                              value = 0.5, min = 0.25, max = 5, step = 0.01)
                                         )
                                    )
                      )
                    )
                  )
    )
  )
  
}


moduleAllTracksServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      # Reactive values ---------------------------------------------------------
      
      input_object <- shiny::reactive({object})
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      output$phase <- shiny::renderUI({
        
        ns <- session$ns
        
        validate_timedisplaced_tmt(input_object())
        
        phase_picker_input(ns = ns)
        
        
      })
      
      output$phase_cluster <- shiny::renderUI({
        
        ns <- session$ns
        
        validate_timedisplaced_tmt(input_object())
        
        phase_cluster_picker_input(ns = ns)
        
      })
      
      output$across <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$phase_cluster)
        
        across_groups <- 
          getVariableNames(object = input_object(), 
                           phase = input$phase_cluster, 
                           variable_classes = c("input", "cluster")
          )
        
        across_picker_input(ns = ns, choices = across_groups)
        
      })
      
      output$across_subset <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$phase_cluster)
        shiny::req(input$across)
        
        across_subset_options <- 
          getVariableValues(object = input_object(), 
                            phase = input$phase_cluster, 
                            variable_name = input$across)
        
        across_subset_picker_input(ns = ns, 
                                   choices = across_subset_options, 
                                   selected = across_subset_options)
        
      })
      
      output$color_to <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$phase_cluster)
        
        color_to_groups <- 
          getVariableNames(object = input_object(), 
                           phase = input$phase_cluster, 
                           variable_classes = c("input", "cluster"))
        
        color_to_picker_input(ns = ns, choices = color_to_groups)
        
      })
      
      output$frame_time_subset <- shiny::renderUI({
        
        ns <- session$ns
        
        whole_time <- 
          getFrameTimeSeq(object = input_object(), 
                          phase = input$phase)
        
        shiny::sliderInput(inputId = ns("frame_time_subset"),
                           label = "Time Regulator:", 
                           value = base::min(whole_time),
                           min = base::min(whole_time), 
                           max = base::max(whole_time), 
                           step = getInterval(object = input_object()),
                           post = stringr::str_c(" ", getIntervalUnit(object = input_object()))
                           )
        
      })
      
      output$change_order <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$across_subset)
        
        change_order_input(ns = ns, items = input$across_subset)
        
      })
      
      # -----
      
      
      
      # Reactive expressions ----------------------------------------------------
      
      #!!! awkward solution to the problem that input$change_order_order changes it's class somehow
      across_subset_ordered <- shiny::reactive({
        
        hlpr_order_input(input$change_order_order)
        
      })
      
      module_plot <- shiny::eventReactive(input$update_plot, {
        
        plotAllTracks(
          object = input_object(), 
          across = input$across, 
          across_subset = across_subset_ordered(),
          color_to = input$color_to, 
          phase = input$phase, 
          phase_cluster = input$phase_cluster,
          n_cells = input$n_cells, 
          linetype = input$linetype, 
          linesize = input$linesize, 
          time_subset = input$frame_time_subset
        )
        
      })
      
      
      # -----
      
      
      # Plot outputs ------------------------------------------------------------
      
      output$module_plot <- shiny::renderPlot({
        
        shiny::req(module_plot())
        
        module_plot()
        
      })
      
      output$save_as_pdf <- shiny::downloadHandler(
        filename = function(){
          base::paste("name", ".pdf", sep = "")
        }, 
        content = function(file){
          grDevices::pdf(file)
          plot(module_plot())
          grDevices::dev.off()
        }, 
        contentType = "application/pdf"
      )
      
      # -----
      
      
    }
  )
  
  
}