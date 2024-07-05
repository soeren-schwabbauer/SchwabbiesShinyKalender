library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(googlesheets4)
library(lubridate)
library(dplyr)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "./.secrets"
)

# load pw
source("./master.R")

# Source all files that contribute to the live app -----------------------------
all_files <- list.files("./", full.names = TRUE)
app_files <- all_files[grep("prepare_|datatable_", all_files, ignore.case = TRUE)]
for (filename in app_files) try(source(filename))

# load feiertage -------------------------------------------------------------
feiertage_df <<- readr::read_rds("./feiertage.rds")

ui <- fluidPage(
  
  # Application title
  titlePanel("Schwabbies Kalender"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
      uiOutput("login_ui"),
      uiOutput("main_ui")
  )
)

server <- function(input, output, session) {
  
  # Reactive polling to check for updates in Google Sheets
  data_poller <- reactivePoll(10000, session,
                              checkFunc = function() {
                                googledrive::drive_get("termine")$drive_resource[[1]]$modifiedTime
                              },
                              valueFunc = function() {
                                read_sheet(tabelle_id())
                              }
  )
  
  # Tabellen ID ----------------------------------------------------------------
  tabelle_id <- reactive({googledrive::drive_get("termine")$id})

  # Store login state
  logged_in <- reactiveVal(FALSE)
  
  # Password check
  observeEvent(input$pw_confirm, {
    if (input$pw_text == pw) {  # Replace with your actual password
      logged_in(TRUE)
    } else {
      showModal(modalDialog(
        title = "Login Failed",
        "Incorrect password. Please try again.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Login UI
  output$login_ui <- renderUI({
    if (!logged_in()) {
      wellPanel(
        textInput("pw_text", "Enter Login Password"),
        actionButton("pw_confirm", "Login", icon = icon("sign-in-alt"))
      )
    }
  })
  
  # Main UI
  output$main_ui <- renderUI({
    if (logged_in()) {
      tagList(
        fluidPage(
            column(2, selectInput("edit_type", "Typ", choices = c("Privat", "Uni", "FMA", "Geburtstage", "Karo"))),
            column(2, dateInput("edit_date", "Datum", value = Sys.Date())),
            column(3, textInput("edit_termin", "Termin", placeholder = "Uhrzeit & Beschreibung")),
            column(2, actionButton("add", "Hinzufügen", icon = icon("add"))),
            column(2, actionButton("remove", "Löschen", icon = icon("trash")))
          
        ),
        fluidPage(
          withSpinner(DTOutput("calender"))
        )
      )
    }
  })
  
  # Create new event
  observeEvent(input$add, {
    if (logged_in()) {
      new_data <- data.frame(Datum = as.Date(input$edit_date),
                             Termin = input$edit_termin,
                             Typ = input$edit_type)
      sheet_append(data = new_data, ss = tabelle_id())
      # clear inputs
      updateTextInput(session, "edit_termin", value = "")
    }
  })
  
  # Remove event
  observeEvent(input$remove, {
    if (logged_in()) {
      # load existing data
      termine_edit <- read_sheet(tabelle_id()) 
      
      # filter input data from existing data
      termine_edited <- termine_edit %>%
        mutate(Datum = as.Date(Datum, format = "%Y-%m-%d")) %>%
        filter(!(Datum  == as.Date(input$edit_date, format = "%Y-%m-%d") & 
                   Typ    == input$edit_type &
                   Termin == input$edit_termin))
      
      # write sheet again
      write_sheet(termine_edited, ss = tabelle_id(), sheet = "Termine")
      
      # clear inputs
      updateTextInput(session, "edit_termin", value = "")
    }
  })
  
  # Output sheet
  output$calender <- renderDataTable({
    # Use the data from reactive polling
    termine <- data_poller()
    
    # prepare calender to dataframe
    kalender <- prepare_kalender(termine, feiertage_df)
    
    # make kalender a datatable
    kalender <- datatable_kalender(kalender)
    
    return(kalender)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
