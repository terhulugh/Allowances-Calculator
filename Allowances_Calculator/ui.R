# Load the necessary packages
library(shiny)
library(shinyjs)
suppressWarnings(library(shinythemes))
suppressWarnings(library(officer))
library(rmarkdown)
suppressWarnings(library(dplyr))
library(tibble)
suppressWarnings(library(scales))
suppressWarnings(library(writexl))
suppressWarnings(library(countrycode))

rank_name <- c("Minister/SGF/HOS", "Permanent Secretary/Chief Executive/Board Members", "Grade Level 17(Director)", "Grade Level 16(Deputy Director)",
               "Grade Level 15(Assistant Director)", "Grade Level 14(Chief)", "Grade Level 13(Assistant Chief)", "Grade Level 12(Principal)",
               "Grade Level 10(Senior)", "Grade Level 9(Officer I)", "Grade Level 8(Officer II)", "Grade Level 7", "Grade Level 6",
               "Grade Level 5", "Grade Level 4", "Grade Level 3", "Grade Level 2", "Grade Level 1")

airport_cities <- c("Abuja", "Lagos", "PortHarcourt", "Kano", "Akure", "Asaba", "Benin", "BirninKebbi", "Calabar", "Enugu", "Gombe", "Ibadan",
                    "Ilorin", "Jos", "Kaduna", "Katsina", "Maiduguri", "Makurdi", "Minna",
                    "Owerri", "Sokoto", "Uyo", "Warri", "Yola")

state.capital <- c("Abakaliki", "Abeokuta", "Abuja", "AdoEkiti", "Akure", "Asaba", "Awka", "Bauchi", "BeninCity",
                   "BirninKebbi", "Calabar", "Damaturu", "Dutse", "Enugu", "Gombe", "Gusau", "Ibadan", "Ikeja", 
                   "Ilorin", "Jalingo", "Jos", "Kaduna", "Kano", "Katsina", "Lafia", "Lokoja", "Maiduguri", "Makurdi",
                   "Minna", "Osogbo", "Owerri", "PortHarcourt", "Sokoto", "Umuahia", "Uyo", "Yenagoa", "Yola")

country_name <- countrycode::codelist$country.name.en

supp_category <- c("boarding & lodging", "lodging and cash", "lodging only", "cash only")


ui <- fluidPage(
        useShinyjs(),  # Initialize shinyjs
        theme = shinytheme("cerulean"),
        
        tags$head(
                tags$style(HTML("
      .nav-dropdown {
        cursor: pointer;
        padding: 10px;
        background-color: #337ab7;
        color: white;
        text-align: left;
      }
      .nav-dropdown:hover {
        background-color: #286090;
      }
      .nav-tabs {
        display: none;
        margin-top: 10px;
      }
    "))
        ),
        
        titlePanel("Nigerian Public Services Allowances Calculator"),
        
        sidebarLayout(
                sidebarPanel(
                        div(class = "nav-dropdown", "Allowance Types", onclick = "shinyjs.toggle('navlistPanel')"),
                        div(id = "navlistPanel", class = "nav-tabs",
                            navlistPanel(
                                    tabPanel("Air Travel Allowance", value = "air_travel"),
                                    tabPanel("Road Travel Allowance", value = "road_travel"),
                                    tabPanel("Estacode Allowance", value = "estacode"),
                                    tabPanel("Estacode Supplementation Allowance", value = "estacode_supp"),
                                    tabPanel("Warm Clothing Allowance", value = "warm_clothing"),
                                    id = "navlistPanelItems", selected = "air_travel"
                            )
                        )
                ),
                
                mainPanel(
                        conditionalPanel(
                                condition = "input.navlistPanelItems == 'air_travel'",
                                fluidPage(
                                        titlePanel("Calculate Air Travel Allowance"),
                                        sidebarLayout(
                                                sidebarPanel(
                                                        textInput("name_air", "Name of Staff"),
                                                        selectInput("rank_air", "Rank", choices = c("", rank_name), selected = ""),
                                                        selectInput("travel_from_air", "Traveling from", choices = c("", airport_cities), selected = ""),
                                                        selectInput("travel_to_air", "Traveling to", choices = c("", airport_cities), selected = ""),
                                                        numericInput("num_days_air", "Number of Days:", value = "", min = 1),
                                                        numericInput("air_ticket_value", "Air Ticket Value:", value = ""),
                                                        numericInput("airport_taxi_value","Airport Taxi Value", value = ""),
                                                        actionButton("calculate_air", "Calculate Allowance"),
                                                        actionButton("reset_air", "Reset"),
                                                        hr(),
                                                        downloadButton("downloadWord_air", "Download Word"),
                                                        downloadButton("downloadCSV_air", "Download CSV")
                                                ),
                                                mainPanel(
                                                        h4("Air Travel Allowance Breakdown:"),
                                                        tableOutput("air_allowance_table")
                                                )
                                        )
                                )
                        ),
                        conditionalPanel(
                                condition = "input.navlistPanelItems == 'road_travel'",
                                fluidPage(
                                        titlePanel("Calculate Road Travel Allowance"),
                                        sidebarLayout(
                                                sidebarPanel(
                                                        textInput("name_road", "Name of Staff"),
                                                        selectInput("rank_road", "Rank", choices = c("", rank_name), selected = ""),
                                                        selectInput("travel_from_road", "Traveling from", choices = c("", state.capital), selected = ""),
                                                        selectInput("travel_to_road", "Traveling to", choices = c("", state.capital), selected = ""),
                                                        numericInput("num_days_road", "Number of Days:", value = "", min = 1),
                                                        actionButton("calculate_road", "Calculate Allowance"),
                                                        actionButton("reset_road", "Reset"),
                                                        hr(),
                                                        downloadButton("downloadWord_road", "Download Word"),
                                                        downloadButton("downloadCSV_road", "Download CSV")
                                                ),
                                                mainPanel(
                                                        h4("Road Travel Allowance Breakdown:"),
                                                        tableOutput("road_allowance_table")
                                                )
                                        )
                                )
                        ),
                        conditionalPanel(
                                condition = "input.navlistPanelItems == 'estacode'",
                                fluidPage(
                                        titlePanel("Calculate Estacode Allowance"),
                                        sidebarLayout(
                                                sidebarPanel(
                                                        textInput("name_estacode", "Name of Staff"),
                                                        selectInput("rank_estacode", "Rank", choices = c("", rank_name), selected = ""),
                                                        selectInput("travel_from_estacode", "Traveling from", country_name, selected = "Nigeria"),
                                                        selectInput("travel_to_estacode", "Traveling to", choices = c("", country_name), selected = ""),
                                                        numericInput("num_days_estacode", "Number of Days:", value = "", min = 1),
                                                        numericInput("exchange_rate", "Exchange rate(₦/$)", value = ""),
                                                        actionButton("calculate_estacode", "Calculate Allowance"),
                                                        actionButton("reset_estacode", "Reset"),
                                                        hr(),
                                                        downloadButton("downloadWord_estacode", "Download Word"),
                                                        downloadButton("downloadCSV_estacode", "Download CSV")
                                                ),
                                                mainPanel(
                                                        h4("Estacode Allowance Breakdown:"),
                                                        tableOutput("estacode_allowance_table")
                                                )
                                        )
                                )
                        ),
                        conditionalPanel(
                                condition = "input.navlistPanelItems == 'estacode_supp'",
                                fluidPage(
                                        titlePanel("Calculate Estacode Supplementation Allowance"),
                                        sidebarLayout(
                                                sidebarPanel(
                                                        textInput("name_estacode_supp", "Name of Staff"),
                                                        selectInput("rank_estacode_supp", "Rank", choices = c("", rank_name), selected = ""),
                                                        selectInput("travel_from_estacode_supp", "Traveling from", country_name, selected = "Nigeria"),
                                                        selectInput("travel_to_estacode_supp", "Traveling to", choices = c("", country_name), selected = ""),
                                                        selectInput("estacode_supplement_category", "Estacode Supplement Category", supp_category),
                                                        numericInput("num_days_estacode_supp", "Number of Days:", value = "", min = 1),
                                                        numericInput("cash_received", "Cash Received($)", value = ""),
                                                        numericInput("exchange_rate_estacode_supp", "Exchange rate(₦/$)", value = ""),
                                                        actionButton("calculate__estacode_supp", "Calculate Allowance"),
                                                        actionButton("reset__estacode_supp", "Reset"),
                                                        hr(),
                                                        downloadButton("downloadWord__estacode_supp", "Download Word"),
                                                        downloadButton("downloadCSV__estacode_supp", "Download CSV")
                                                ),
                                                mainPanel(
                                                        h4("Estacode Supplementation Allowance Breakdown:"),
                                                        tableOutput("estacode_supp_allowance_table")
                                                )
                                        )
                                )
                        ),
                        conditionalPanel(
                                condition = "input.navlistPanelItems == 'warm_clothing'",
                                fluidPage(
                                        titlePanel("Calculate Warm Clothing Allowance"),
                                        sidebarLayout(
                                                sidebarPanel(
                                                        textInput("name_warm_clothing", "Name of Staff"),
                                                        selectInput("rank_warm_clothing", "Rank", choices = c("", rank_name), selected = ""),
                                                        selectInput("travel_from_warm_clothing", "Traveling from", country_name, selected = "Nigeria"),
                                                        selectInput("travel_to_warm_clothing", "Traveling to", choices = c("", country_name), selected = ""),
                                                        numericInput("exchange_rate_warm_clothing", "Exchange rate(₦/$)", value = ""),
                                                        actionButton("calculate_warm_clothing", "Calculate Allowance"),
                                                        actionButton("reset_warm_clothing", "Reset"),
                                                        hr(),
                                                        downloadButton("downloadWord_warm_clothing", "Download Word"),
                                                        downloadButton("downloadCSV_warm_clothing", "Download CSV")
                                                ),
                                                mainPanel(
                                                        h4("Warm Clothing Allowance Breakdown:"),
                                                        tableOutput("warm_clothing_allowance_table")
                                                )
                                        )
                                )
                        )
                )
        )
)
