moduleVelocityHeatmapUI <- function(id, module_width, module_headline){
  
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
                                      hs(4,shiny::sliderInput(inputId = ns("smooth_span"), label = "Smooth Span",
                                                              value = 0.25, min = 0.1, max = 0.9))
                                    ), 
                                    shiny::fluidRow(
                                      hs(4,shiny::selectInput(inputId = ns("arrange_rows"), label = "Arrange Rows", 
                                                              choices = c("Maxima" = "maxima", "None" = "none"))),
                                      hs(4,shiny::numericInput(inputId = ns("n_cells"), label = "Number of Cells:", 
                                                               value = 100, min = 10, max = 1000, step = 1))
                                    )
                      )
                    )
                  )
    )
  )
  
}


moduleVelocityHeatmapServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      
      # Reactive values ---------------------------------------------------------
      
      input_object <- shiny::reactive({ object })
      
      # -----
      
      # Render UIs --------------------------------------------------------------

      
      output$phase_cluster <- shiny::renderUI({
        
        ns <- session$ns
        
        validate_timedisplaced_tmt(input_object())
        
        phase_cluster_picker_input(ns = ns)
        
      })
      
      output$across <- shiny::renderUI({
        
        ns <- session$ns
        
        
        across_groups <- 
          getVariableNames(object = input_object(), 
                           phase = input$phase_cluster, 
                           variable_classes = c("input", "cluster")
                           )
        
        across_picker_input(ns = ns, choices = across_groups)
        
      })
      
      output$across_subset <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$across)
        
        across_subset_options <- 
          getVariableValues(object = input_object(), 
                            phase = input$phase_cluster, 
                            variable_name = input$across)
        
        across_subset_picker_input(ns = ns, 
                                   choices = across_subset_options, 
                                   selected = across_subset_options)
        
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
        
        velocity_heatmap <- 
          plotVelocityHeatmap(
            object = input_object(), 
            across = input$across, 
            across_subset = across_subset_ordered(), 
            phase = "all", 
            phase_cluster = input$phase_cluster,
            n_cells = input$n_cells, 
            arrange_rows = input$arrange_rows, 
            smooth = TRUE, 
            smooth_span = input$smooth_span, 
            in_shiny = TRUE, 
            verbose = FALSE
          )
        
        base::return(velocity_heatmap)
        
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
      
      
    }
  )
  
  
}