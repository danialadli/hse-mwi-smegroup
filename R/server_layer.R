# server_layer.R

#' Initialize Server Logic
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param state Reactive values object for session state
#' @param config App configuration
#' @export
initialize_server <- function(input, output, session, state, config) {
  
  # Example: Navigation Logic (connecting sidebar to main tabs)
  # This assumes your sidebar returns input$nav_selection
  # You might need to adjust based on how navlistPanel behaves
  
  # Example: File Upload Logic
  observeEvent(input$btn_upload, {
    req(input$file_upload)
    # Logic to use data_layer::process_data() would go here
    shinyjs::show("upload_success")
  })
  
  # Example: Rendering Table
  output$raw_data_table <- DT::renderDataTable({
    req(state$data)
    # Return first dataset found
    if(length(state$data) > 0) state$data[[1]]
  })
}
