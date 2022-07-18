library(shiny)
library(bs4Dash)
# library(shinydashboard)
library(reactable)
library(waiter)
library(echarts4r)
library(dplyr)

thematic::thematic_shiny()


data <- read.csv("./data/metrics_abbr.csv") %>% 
  dplyr::select(Date, Zweck, 
                Spender.ID,Sponsor..Vor_und_Nachname,
                Amount_in_CHF, Sponsor..Spenderkategorie, 
                Sponsor..Land,Sponsor..Versand_RTimes) %>% #write.csv("./data/metrics_abbr.csv")
  dplyr::mutate(Amount_CHF = as.numeric(Amount_in_CHF),
                Date = as.Date(Date),
                month = lubridate::month(Date), 
                month_name = months(Date, abbreviate =T),
                month_name=forcats::fct_reorder(month_name,month), 
                year = (lubridate::year(Date)),
                day_of_year = lubridate::yday(Date),
                day_of_month =lubridate::mday(Date)) %>% 
  dplyr::arrange(Spender.ID, Date) %>% 
  group_by(Spender.ID) %>% 
  mutate(new_donor = row_number()==1) %>% 
  ungroup() 

