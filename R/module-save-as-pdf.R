moduleSaveAsPdfUI <- function(id, module_width = 3){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    hs(width = module_width,
       shiny::h5(shiny::strong("Save:")), 
       shiny::downloadButton(outputId = ns("save_as_pdf"), label = "PDF")
       )
  )
  
  
}


moduleSaveAsPdfServer <- function(id, input_plot){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      plot_to_save <- shiny::reactive({ input_plot() })
      
      output$save_as_pdf <- shiny::downloadHandler(
        filename = function(){
          base::paste("name", ".pdf", sep = "")
        }, 
        content = function(file){
          grDevices::pdf(file)
          plot(plot_to_save())
          grDevices::dev.off()
        }, 
        contentType = "application/pdf"
      )
      
    }
  )
  
}