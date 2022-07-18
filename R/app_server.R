#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  rv_data <- reactive({
    validate(
      need(input$date_range[1] <= input$date_range[2], 'Make sure dates are correct.')
    )
    data %>% filter(
      Date >= as.Date(input$date_range[1]),  Date <= as.Date(input$date_range[2])
    )
  })
  
  compare_type <- reactive({
    print(isolate(rv_data())$Date %>% max())
    if (min(rv_data()$year, na.rm = T) == max(rv_data()$year)) "Month" else "Year"
  })

  mod_fundraising_server("fundraising_1", data = rv_data, compare_type)

  mod_data_server("data_1", data = data)
}
