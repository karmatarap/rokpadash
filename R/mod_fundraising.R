#' fundraising UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fundraising_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      mod_info_boxes_ui(ns("info_boxes_1")),
      tags$br(), tags$br(),
      fluidRow(
        column(
          8,
          box(
            width = 12,
            closable = F, 
            maximizable = T,
            div(style="display:inline-block",shinyWidgets::prettySwitch(ns("cum_switch"), label = "Cumulative")),
            div(style="display:inline-block", shinyWidgets::prettySwitch(ns("med_switch"), label = "Median")),
            echarts4r::echarts4rOutput(ns("donations_plot"))
          )
        ),
        column(4,
               box(width=12, closable = F,      maximizable = T,
                shinyWidgets::prettySwitch(ns("com_switch"), label = "Communications"),
                echarts4r::echarts4rOutput(ns("project_plot"))
               )
        )
      ),
   
      fluidRow(
        column(
          6,
          box( width=12,
               closable = F, maximizable = T,
               reactableOutput(ns("amount_table"))
               )
        ),
        column(
          6,
          box(
            width=12,
            closable = F, maximizable = T,
            shinyWidgets::prettySwitch(ns("inst_switch"), label = "Institutions"),
            reactableOutput(ns("donors_table"))
          )
        )
      )
    )
  )
}

#' fundraising Server Functions
#'
#' @noRd
mod_fundraising_server <- function(id, data = NULL, compare_type = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pct_change <- function(curr, prev) {
      ifelse(prev == 0, 0, (curr - prev) / prev)
    }

    comp_data <- reactive({
      today <- Sys.Date()
      this.year <- lubridate::year(today)
      this.month <- lubridate::month(today)

      subsetted <- data() %>%
        mutate(
          comp_type = compare_type(),
          timepoint = case_when(
            comp_type == "Month" & year == this.year & month == (this.month - 1) ~ "now",
            comp_type == "Month" & year == this.year & month == (this.month - 2) ~ "prev",
            comp_type == "Year" & year == this.year ~ "now",
            comp_type == "Year" & year == (this.year - 1) ~ "prev",
            TRUE ~ ""
          )
        ) %>%
        filter((timepoint != "")) 
      
     
      prev_donors <- subsetted %>% filter(timepoint == "prev") %>% pull(Spender.ID)
      new_donors <- subsetted %>% filter(timepoint == "now") %>% pull(Spender.ID)
      retention <-  length(intersect(prev_donors, new_donors))
      
      subsetted %>% 
        group_by(timepoint) %>%
        summarise(
          total_donations = sum(Amount_CHF, na.rm = T),
          total_donors = n_distinct(Spender.ID),
          avg_donation = mean(Amount_CHF, na.rm = T),
          new_donors = sum(new_donor, na.rm = T)
        ) %>%
        tidyr::pivot_wider(names_from = timepoint, 
                           values_from = c(total_donations, total_donors, avg_donation, new_donors)) %>%
        mutate(
          total_donations_change = pct_change(total_donations_now, total_donations_prev),
          total_donors_change = pct_change(total_donors_now, total_donors_prev),
          avg_donation_change = pct_change(avg_donation_now, avg_donation_prev),
          new_donors_change = pct_change(new_donors_now, new_donors_prev),
          retention = retention,
          retention_rate =  retention / total_donors_prev 
        )
      
      
      
      
    })


    mod_info_boxes_server("info_boxes_1", comp_data)



    output$donations_plot <- echarts4r::renderEcharts4r({
      
      # for comparing months this year
      

      if (compare_type() == "Year"){
        tmp <-  data() %>%
          group_by(year, month, month_name)
        
        if (input$med_switch){
          tmp <- tmp %>% 
            summarize(donations = round(median(Amount_in_CHF), 2))
        } else {
          tmp <- tmp %>% 
            summarize(donations = round(sum(Amount_in_CHF), 2))
        }
        tmp <- tmp %>% 
          group_by(year) %>%
          mutate(cum_donations = round(cumsum(donations), 2)) %>%
          e_charts(month_name)
  
      } else {
        tmp <-  data() %>%
          group_by(day_of_month, month_name)
          if (input$med_switch){
            tmp <- tmp %>% 
              summarize(donations = round(median(Amount_in_CHF), 2))
          } else {
            tmp <- tmp %>% 
              summarize(donations = round(sum(Amount_in_CHF), 2))
          }
        tmp <- tmp %>% group_by(month_name) %>%
          mutate(cum_donations = round(cumsum(donations), 2)) %>%
          e_charts(day_of_month)
      }

      p <- tmp
      if (input$cum_switch) {
        p <- p %>% 
          e_line(cum_donations) %>% 
          e_title(paste("Cumulative Donations by",compare_type())) 
      } else {
        p <- p %>%
          e_line(donations)%>% 
        e_title(paste(ifelse(input$med_switch, "Median", "Total"),"Donations by",compare_type())) 
      }
      

      p %>% 
        e_datazoom(
          type = "slider", 
          toolbox = FALSE,
          bottom = -5
        ) %>% 
        e_legend(right = 0) %>% 
        e_tooltip(
          formatter = e_tooltip_item_formatter("currency")
        )
    })
    
    
    output$project_plot <- echarts4r::renderEcharts4r({
      
      if (input$com_switch){
        data() %>% 
          filter(!is.na(Sponsor..Versand_RTimes)) %>% 
          group_by(Sponsor..Versand_RTimes) %>% 
          summarise(total= round(sum(Amount_in_CHF, na.rm = T)))%>% 
          arrange((total))  %>%  
          e_charts(Sponsor..Versand_RTimes) %>% 
          e_pie(total) %>% 
          #e_bar(total)  %>% 
          #e_flip_coords() %>% 
          e_legend(show = F) %>% 
          e_tooltip(
            
          )%>% 
          e_title("Funding by preferred communication type") %>% 
          e_grid(left = 90)
      } else {
        data() %>% 
          mutate(Zweck = coalesce(Zweck, "General")) %>% 
          group_by(Zweck) %>% 
        summarise(total= round(sum(Amount_in_CHF, na.rm = T))) %>% 
        arrange((total)) %>% tail(12) %>%  
        e_charts(Zweck) %>% 
       # e_bar(total)  %>% 
      #  e_flip_coords() %>% 
          e_pie(total) %>% 
        e_legend(show = F) %>% 
        e_tooltip() %>% 
            e_title("Top 12 most funded projects")
      }

    })
    
    output$donors_table <- renderReactable({
      tmp <- data() 
      if (input$inst_switch){
        tmp <- tmp %>% filter(Sponsor..Spenderkategorie != "Privatperson")
      } else {
        tmp <- tmp %>% filter(Sponsor..Spenderkategorie == "Privatperson")
      }
      tmp %>% group_by(`Sponsor..Vor_und_Nachname`) %>% 
        summarise(Total = sum(Amount_in_CHF))%>% 
        ungroup() %>% 
        arrange(desc(Total)) %>% 
        head(20) %>% 
        mutate(Total = scales::dollar(round(Total))) %>% 
        rename(Sponsor = `Sponsor..Vor_und_Nachname`) %>% 
        reactable(defaultPageSize = 5)
    })
    
    output$amount_table <- renderReactable({
      data() %>% 
        group_by(`Spender.ID`) %>% 
        filter(!is.na(Amount_CHF)) %>% 
        mutate(x = sum(Amount_in_CHF), 
               Category=cut(x, 
                            breaks=c(-Inf, 50, 100, 500, 1000, 5000, 10000, Inf), 
                            labels=c("less than 50", "50 to 100","100 to 500","500 to 1000", "1000 to 5000", "5000 to 10,000", "over 10,000"))) %>% 
        group_by(Category) %>% 
        #count() %>% 
        summarise(Donations = n()) %>% 
        reactable(  columns = list(
          Category = colDef(footer = "Total"),
          Donations = colDef(footer = function(values) scales::comma(sum(values)))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold")))
    })
  })
}

## To be copied in the UI
# mod_fundraising_ui("fundraising_1")

## To be copied in the server
# mod_fundraising_server("fundraising_1")
