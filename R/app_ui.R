#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Define this page as a dashboard page to signal we're using the     dashboard page format
    dashboardPage(
      header = dashboardHeader(
        title = dashboardBrand(
          title = "Rokpa metrics",
          color = "primary",
          # href = "https://divadnojnarg.github.io/outstanding-shiny-ui/",
          image = "https://adminlte.io/themes/v3/dist/img/user2-160x160.jpg",
          opacity = 0.8
        )
      ),
      # Create our navigation menu that links to each of the tabs we defined
      sidebar = dashboardSidebar(
        sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          menuItem("Fundraising", tabName = "fundraising", icon = icon("dollar", verify_fa = F)),
          menuItem("Data", tabName = "data", icon = icon("database"))
        )
      ),
      controlbar = dashboardControlbar(
        width = "250",
        div(style="padding: 25px 10px 25px 10px",
            dateRangeInput("date_range", label = "Select Date Range of data", start="2018-01-01", end = as.character(Sys.Date()))
           
        #shinyWidgets::pickerInput("compare_type", label = "Compare to last", choices = c("Year", "Month"), selected = "Year", inline=T)
      )),
      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = dashboardBody(
        tabItems(
          tabItem("fundraising", mod_fundraising_ui("fundraising_1")),
          tabItem("data", mod_data_ui("data_1"))
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rokpadash"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
