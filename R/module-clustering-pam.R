moduleClusteringPamUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 3, 
                    blue_box(title = "Pam Clustering", width = 12,
                             shiny::column(width = 12, 
                                           shiny::h4(shiny::strong("Display Cluster:")),
                                           shiny::uiOutput(outputId = ns("phase")), 
                                           shiny::uiOutput(outputId = ns("cluster")), 
                                           shiny::actionButton(inputId = ns("update_plots"), label = "Update Plots"), 
                                           shiny::HTML("<br>")
                                           )
                             )
                    ), 
      shiny::column(width = 9, 
                    shiny::fluidRow(
                      shiny::column(width = 6,
                                    well_panel(
                                      shiny::h4(shiny::strong("Medoid Summary")),
                                      shiny::plotOutput(outputId = ns("plot_medoid_summary")), 
                                      shiny::fluidRow(
                                        moduleSaveAsPdfUI(id = ns("ms")),
                                      ),
                                      shiny::HTML("<br>"), 
                                      shiny::fluidRow(
                                        hs(3, shiny::uiOutput(outputId = ns("clrp_ms"))), 
                                        hs(6, shinyWidgets::checkboxGroupButtons(
                                          inputId = ns("options_ms"),
                                          label = "Additional Options:", 
                                          choices = c("Flip Coordinates" = "flip_coords"), 
                                          selected = "flip_coords")
                                        )
                                      )
                                    )
                      ),
                      shiny::column(width = 6,
                                    well_panel(
                                      shiny::h4(shiny::strong("Cluster Quality")),
                                      shiny::plotOutput(outputId = ns("plot_sil_width")), 
                                      shiny::fluidRow(
                                        moduleSaveAsPdfUI(id = ns("sw"))
                                      ),
                                      shiny::HTML("<br>"), 
                                      shiny::fluidRow(
                                        hs(3, shiny::uiOutput(outputId = ns("clrp_sw"))), 
                                        hs(6, shinyWidgets::checkboxGroupButtons(
                                          inputId = ns("options_sw"),
                                          label = "Additional Options:", 
                                          choices = c("Display average" = "display_average"), 
                                          selected = "display_average")
                                        )
                                      )
                                    )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(width = 6,
                                    well_panel(
                                      shiny::h4(shiny::strong("Cluster Summary")),
                                      shiny::plotOutput(outputId = ns("plot_cluster_summary")), 
                                      shiny::fluidRow(
                                        moduleSaveAsPdfUI(id = ns("cs"))
                                      ),
                                      shiny::HTML("<br>"), 
                                      shiny::fluidRow(
                                        hs(3,shiny::uiOutput(outputId = ns("clrp_cs"))), 
                                        hs(6, shinyWidgets::checkboxGroupButtons(
                                          inputId = ns("options_cs"),
                                          label = "Additional Options:", 
                                          choices = c("Flip Coordinates" = "flip_coords"), 
                                          selected = "flip_coords")
                                        )
                                      )
                                    )
                      ),
                      shiny::column(width = 6,
                                    well_panel(
                                      shiny::h4(shiny::strong("Cluster Count")),
                                      shiny::plotOutput(outputId = ns("plot_cluster_count")), 
                                      shiny::fluidRow(
                                        moduleSaveAsPdfUI(id = ns("cc"))
                                      ),
                                      shiny::HTML("<br>"), 
                                      shiny::fluidRow(
                                        hs(3, shiny::uiOutput(outputId = ns("clrp_cc")))
                                      )
                                    )
                      )
                    ))
      )
  )
  
}


moduleClusteringPamServer <- function(id, object){
  
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      # Reactive values ---------------------------------------------------------
      
      
      input_object <- shiny::reactive({ object })
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      # in control box
      output$phase <- shiny::renderUI({
        
        ns <- session$ns
        
        validate_timedisplaced_tmt(input_object())
        
        phase_picker_input(ns = ns, choices = pretty_phases[1:2])
        
      })
      
      output$cluster <- shiny::renderUI({
        
        ns <- session$ns
        
        if(time_displaced_tmt(input_object())){
          
          shiny::req(input$phase)
          
        }
        
        pam_choices <- 
          getClusterNames(input_object(), phase = input$phase, algorithm_subset = "pam") %>% 
          purrr::flatten() %>% 
          purrr::flatten_chr()
        
        shinyWidgets::pickerInput(
          inputId = ns("cluster"), 
          label = "Current Cluster:", 
          choices = pam_choices, 
          selected = pam_choices[1]
        )
        
      })
      
      output$numeric_variables <- shiny::renderUI({
        
        ns <- session$ns
        
        variables <-
          getNumericVariableNames(object = input_object())
        
        
        numeric_variables_picker_input(ns = ns, choices = variables)
        
        
      })
      
      
      # in well panels
      output$clrp_ms <- shiny::renderUI({
        
        ns <- session$ns 
        
        clrp_picker_input(ns = ns, id = "clrp_ms")
        
      })
      
      output$clrp_cs <- shiny::renderUI({
        
        ns <- session$ns 
        
        clrp_picker_input(ns = ns, id = "clrp_cs", selected = "jama")
        
      })
      
      output$clrp_cc <- shiny::renderUI({
        
        ns <- session$ns 
        
        clrp_picker_input(ns = ns, id = "clrp_cc", selected = "jama")
        
      })
      
      output$clrp_sw <- shiny::renderUI({
        
        ns <- session$ns 
        
        clrp_picker_input(ns = ns, id = "clrp_sw", selected = "jama")
        
      })
      
      # -----


      
      # Reactive expressions ----------------------------------------------------
      
      # argument inputs
      
      variables <- shiny::reactive({ input$numeric_variables })
      
      input_list <- shiny::eventReactive(input$update_plots, {
        
        list(k = stringr::str_extract(input$cluster, pattern = "\\d{1,2}$"), 
             phase = input$phase)
        
      })
      
      k <- shiny::reactive({
        
        input_list()$k
        
      })
      
      phase <- shiny::reactive({ 
        
        input_list()$phase
        
      })
      
      
      # plots 
      plot_medoid_summary <- shiny::reactive( {
        
        shiny::req(phase())
        
        plotPamMedoidSummary(
          object = input_object(),
          k = k(),#!!!,
          phase = shiny::isolate(phase()),
          clrp = input$clrp_ms,
          flip_coords = base::ifelse("flip_coords" %in% input$options_ms, TRUE, FALSE)
          )
        
      })
      
      plot_sil_width <- shiny::reactive( {
        
        shiny::req(phase())
        
        plotPamSilWidth(
          object = input_object(), 
          k = k(), 
          phase = shiny::isolate(phase()),
          clrp = input$clrp_sw, 
          display_average = base::ifelse("display_average" %in% input$options_sw, TRUE, FALSE)
        )
        
      })
      
      
      plot_cluster_summary <- shiny::reactive( {
        
        shiny::req(phase())
        
        plotPamClusterSummary(
          object = input_object(), 
          phase = shiny::isolate(phase()),
          k = k(), 
          clrp = input$clrp_cs, 
          flip_coords = base::ifelse("flip_coords" %in% input$options_cs, TRUE, FALSE)
        )
        
      })
      
      
      plot_cluster_count <- shiny::reactive({
        
        shiny::req(phase())
        
        plotPamClusterCount(
          object = input_object(), 
          phase = shiny::isolate(phase()),
          k = k(), 
          clrp = input$clrp_cc, 
          flip_coords = base::ifelse("flip_coords" %in% input$options_cc, TRUE, FALSE)
        )
        
      })
      
      # -----
      
      
      # Plot outputs ------------------------------------------------------------
      
      output$plot_medoid_summary <- shiny::renderPlot({
        
        shiny::req(plot_medoid_summary())
        
        plot_medoid_summary()
        
      })
      
      moduleSaveAsPdfServer(id = "ms", input_plot = plot_medoid_summary)
      
      
      output$plot_sil_width <- shiny::renderPlot({
        
        shiny::req(plot_sil_width())
        
        plot_sil_width()
        
      })
      
      moduleSaveAsPdfServer(id = "sw", input_plot = plot_sil_width)
      
      
      output$plot_cluster_summary <- shiny::renderPlot({
        
        shiny::req(plot_cluster_summary())
        
        plot_cluster_summary()
        
      })
      
      moduleSaveAsPdfServer(id = "cs", input_plot = plot_cluster_summary)
      
      
      output$plot_cluster_count <- shiny::renderPlot({
        
        shiny::req(plot_cluster_count())
        
        plot_cluster_count()
        
      })
      
      moduleSaveAsPdfServer(id = "cc", input_plot = plot_cluster_count)
      
      
      
      # -----

      

      
      
      
    }
  )
}