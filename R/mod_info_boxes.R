#' info_boxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_boxes_ui <- function(id) {
  ns <- NS(id)
  tagList( wellPanel(
    fillRow(height = 50,
     
      #column(
      #  2,
        uiOutput(ns("total_donations")),
      #),
      #column(
      #  2,
        uiOutput(ns("avg_donation")),
      #),
     # column(
    #    2,
        uiOutput(ns("total_donors")),
     # ),
      #column(
      #  2,
        uiOutput(ns("new_donors")),
      #),
    #  column(
     #   2,
        uiOutput(ns("retention_rate"))
      #)
      )
    )
  )
}

make_description_block <- function(change, now, name = "Total Donations", pct=T,caret =T, currency=TRUE) {
  descriptionBlock(
    number = if (pct) scales::percent(change) else change,
    numberColor = if (change > 0) "success" else "danger",
    numberIcon = if (change > 0) icon("caret-up") else icon("caret-down"),
    header = if (currency )scales::dollar(now) else now,
    text = name,
    rightBorder = TRUE,
    marginBottom = FALSE
  )
}

#' info_boxes Server Functions
#'
#' @noRd
mod_info_boxes_server <- function(id, comp_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$total_donations <- renderUI({
      req(comp_data())
      make_description_block(comp_data()$total_donations_change, comp_data()$total_donations_now)
    })

    output$avg_donation <- renderUI({
      req(comp_data())
      make_description_block(comp_data()$avg_donation_change, comp_data()$avg_donation_now, name = "Avg Donation")
    })

    output$total_donors <- renderUI({
      req(comp_data())
      make_description_block(comp_data()$total_donors_change, comp_data()$total_donors_now, name = "Total Donors", currency = F)
    })

    output$new_donors <- renderUI({
      req(comp_data())
      make_description_block(comp_data()$new_donors_change, comp_data()$new_donors_now, name = "New Donors", currency = F)
    })
    
    output$retention_rate <- renderUI({
      req(comp_data())
      make_description_block(comp_data()$retention_rate, comp_data()$retention, name = "Retention Rate", currency = F)
    })

    
  })
}

## To be copied in the UI
# mod_info_boxes_ui("info_boxes_1")

## To be copied in the server
# mod_info_boxes_server("info_boxes_1")
