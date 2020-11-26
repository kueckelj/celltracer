moduleDescrStatPlotUI <- function(id, module_width, module_headline){
  
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
                                        hs(4,shiny::uiOutput(outputId = ns("across"))),
                                        hs(4,shiny::uiOutput(outputId = ns("across_subset"))),
                                        hs(4,shiny::uiOutput(outputId = ns("variables"))),
                                      ), 
                                      shiny::fluidRow(
                                        hs(4,shiny::uiOutput(outputId = ns("phase"))), 
                                        hs(4,shiny::uiOutput(outputId = ns("plot_type"))), 
                                        hs(4,shiny::uiOutput(outputId = ns("plot_adjustments")))
                                      ), 
                                      shiny::uiOutput(outputId = ns("statistics"))
                                      )
                      )
                    )
                    )
  )
  
}



moduleDescrStatPlotServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      

# Reactive values ---------------------------------------------------------

      input_object <- shiny::reactive({ object })
      
      # -----

# Render UIs --------------------------------------------------------------
      
      output$phase <- shiny::renderUI({
        
        ns <- session$ns
        
        validate_timedisplaced_tmt(input_object())
        
        phase_picker_input(ns = ns, choices = pretty_phases[1:2])
        
      })
      
      output$variables <- shiny::renderUI({
        
        ns <- session$ns
        
        num_variables <-
          getStats(object = input_object()) %>% 
          dplyr::select_if(.predicate = base::is.numeric) %>% 
          base::colnames()
        
        variables <- 
          base::append(
            x = pretty_stat_variables_list, 
            values = num_variables[!num_variables %in% pretty_stat_variables_vec]
          )
        
        shinyWidgets::pickerInput(inputId = ns("variables"), 
                                  label = "Include Variables:", 
                                  choices = variables, 
                                  selected = variables, 
                                  options = list(`actions-box`= TRUE),
                                  multiple = TRUE)
        
      })
      
      output$across <- shiny::renderUI({
        
        ns <- session$ns
        
        across_groups <- 
          getVariableNames(object = input_object(), 
                           phase = input$phase, 
                           variable_classes = c("input", "cluster"))
        
        shiny::req(across_groups)
        
        across_picker_input(ns = ns, choices = across_groups)
        
      })
      
      output$across_subset <- shiny::renderUI({
        
        ns <- session$ns

        shiny::req(input$across)
        
        across_subset_options <- 
          getVariableValues(
            object = input_object(), 
            phase = input$phase, 
            variable_name = input$across
          )
        
        across_subset_picker_input(ns = ns, 
                                   choices = across_subset_options, 
                                   selected = across_subset_options)
        
      })
      
      
      output$change_order <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$across_subset)
        
        change_order_input(ns = ns, items = input$across_subset)
        
      })
      
      output$plot_type <- shiny::renderUI({
        
        ns <- session$ns 
        
        shinyWidgets::pickerInput(inputId = ns("plot_type"),
                           label = "Plottype:", 
                           choices = pretty_plottypes, 
                           selected = "boxplot", 
                           multiple = FALSE)
        
      })
      
      
      # --- Statistical part
      
      output$statistics <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::validate(
          shiny::need(
            expr = input$plot_type %in% c("boxplot", "violin"), 
            message = "Choose Box- or Violinplot to perform statistical tests."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = base::length(input$across_subset) > 1, 
            message = "At least two groups are needed to perform statistical tests."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = base::length(input$variables) == 1, 
            message = "Exactly one variable is needed to perform statistical tests."
          )
        )
        
        shiny::tagList(
          shiny::fluidRow(
            hs(width = 8,
               shinyWidgets::radioGroupButtons(
                 inputId = ns("test_options_pairwise"),
                 label = "Pairwise Test:",
                 choices = pretty_stattests_pairwise, 
                 selected = "none",
                 justified = TRUE)
               ), 
            hs(width = 4, 
               shinyWidgets::pickerInput(
                 inputId = ns("ref_group"), 
                 label = "Reference Group:",
                 choices = input$across_subset, 
                 multiple = FALSE)
               )
          ), 
          shiny::fluidRow(
            hs(width = 8,
               shinyWidgets::radioGroupButtons(
                 inputId = ns("test_options_groupwise"), 
                 label = "Groupwise Test:",
                 choices = pretty_stattests_groupwise, 
                 selected = "none", 
                 justified = TRUE)
               )
          )
        )
        
        
      })
      
      # ---
      
      
      # -----
      
      

# Reactive expressions ----------------------------------------------------
      
      #!!! awkward solution to the problem that input$change_order_order changes it's class somehow
      across_subset_ordered <- shiny::reactive({
        
        hlpr_order_input(input$change_order_order)
        
      })
      
      module_plot <- shiny::eventReactive(input$update_plot, {
        
        plotDistribution(
          object = input_object(), 
          variables = input$variables, 
          across = input$across, 
          across_subset = across_subset_ordered(), 
          phase = input$phase,
          test_pairwise = input$test_options_pairwise, 
          test_groupwise = input$test_options_groupwise,
          ref_group = input$ref_group,
          plot_type = input$plot_type, 
          scales = "free", 
          verbose = FALSE
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
      
      
    }
  )
  
  
}