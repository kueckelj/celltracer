moduleCategStatPlotUI <- function(id, module_width, module_headline){
  
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
                                    shiny::HTML("<br><br>"), 
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("phase"))),
                                      hs(4,shiny::uiOutput(outputId = ns("features"))),
                                      hs(4,shiny::uiOutput(outputId = ns("feature_compare")))
                                    ), 
                                    shiny::fluidRow(
                                      hs(4,bar_position_picker_input(ns = ns)), 
                                      hs(4,shiny::uiOutput(outputId = ns("plot_adjustments")))
                                    )
                      )
                    )
                  )
    )
  )
  
}



moduleCategStatPlotServer <- function(id, object){
  
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

      output$features <- shiny::renderUI({
        
        ns <- session$ns
        
        features <- 
          getVariableNames(object = input_object(), 
                           phase = input$phase, 
                           variable_classes = c("input", "cluster"))
        
        shiny::req(features)
        
        shinyWidgets::pickerInput(inputId = ns("features"), 
                                  label = "Features:", 
                                  choices = features, 
                                  multiple = TRUE)
        
      })
      
      output$feature_compare <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(input$features)
        
        feature_compare <- 
          getVariableNames(object = input_object(), 
                           phase = input$phase, 
                           variable_classes = c("input", "cluster"))
        
        shinyWidgets::pickerInput(inputId = ns("feature_compare"), 
                                  label = "Feature to compare:", 
                                  choices = feature_compare, 
                                  selected = "condition",
                                  multiple = FALSE)
        
      })

      # -----
      
      
      
      # Reactive expressions ----------------------------------------------------
      
      
      module_plot <- shiny::eventReactive(input$update_plot, {
        
        checkpoint(evaluate = base::length(input$features) != 0, 
                   case_false = "no_features_selected")
        
        plotDistributionDiscrete(
          object = input_object(), 
          phase = input$phase, 
          features = input$features, 
          feature_compare = input$feature_compare, 
          clrp = "milo", 
          position = input$bar_position
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