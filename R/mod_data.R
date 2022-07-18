#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div
mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      fileInput(ns("donation_file"), "Upload a new donor file",
        multiple = FALSE,
        accept = c(
          "xlsx",
          ".xlsx"
        )
      )
    ),
    tags$br(),
    downloadButton(
      ns("downloadData"),
      label = "Download",
      icon = shiny::icon("download")
    ),
    
  #  shinyWidgets::prettySwitch(ns("show_data"), label = "Show Data", value = FALSE),
    tags$br(),
    #conditionalPanel(
     # "input.show_data == 'true'",
      #div(
      #  style = "overflow-x: auto",
        reactableOutput(ns("data"))
      #)
    #)
  )
}

#' data Server Functions
#'

mod_data_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      print(names(isolate(data())))
      # 
      # observe({
      #   # check current file
      #   # tryCatch(
      # 
      #   # )
      #   # if file_is_ok(input$donation_file$path)
      #   #
      #   req(input$donation_file)
      #   print("new donation file")
      # 
      #   new_data <- readxl::read_excel(input$donation_file$path)
      # 
      #   # add data to existing data
      #   overwrite <- dplyr::bind_rows(data(), new_data) %>%
      #     unique()
      # 
      #   data() %>% write.csv(file = "./data/metrics_data_backup.csv")
      #   overwrite %>% write.csv(file = "./data/metrics_data.csv")
      # })

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(data(), file)
        }
      )
      output$data <- renderReactable({
        req(input$donation_file)
        new_data <- suppressWarnings(readxl::read_excel(input$donation_file$datapath))
        
        if (!all(names(new_data) == names(data()))){
          
          showNotification(glue::glue("Expected columns: {names(data())}"))
        } else {
          overwrite_data <- vctrs::vec_rbind(data(), new_data, .ptype = data())
          write.csv(data(), "data/metrics_data_backup.csv")
          write.csv(overwrite_data, "data/metrics_data.csv")
        }
        
        reactable(new_data)
      })
    }
  )
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
