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
    
    
    fluidPage(
      wellPanel(
        column(3, selectInput("edit_type", NULL, choices = c("Privat", "Uni", "FMA", "Stmyr", "Karo", "Geburtstage"))),
        column(3, dateInput("edit_date", NULL, value = Sys.Date())),
        column(3, textInput("edit_termin", NULL, placeholder = "Uhrzeit & Beschreibung")),
        actionButton("add", "Hinzufügen"),
        actionButton("remove", "Löschen")
      )
    ),
    
    fluidPage(
      withSpinner(DTOutput("calender"))
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive trigger to update DataTable
  trigger <- reactiveVal(0)
  
  # Tabellen ID ----------------------------------------------------------------
  tabelle_id <- reactive({googledrive::drive_get("termine")$id})
  
  # creat new event ------------------------------------------------------------
  observeEvent(input$add, {
    
    new_data <- data.frame(Datum = as.Date(input$edit_date),
                           Termin = input$edit_termin,
                           Typ = input$edit_type)
    sheet_append(data = new_data, ss = tabelle_id())
    # clear inputs
    updateTextInput(session, "edit_termin", value = "")
    
    # Update the trigger to refresh the DataTable
    trigger(trigger() + 1)
  })
  
  # remove event ---------------------------------------------------------------
  observeEvent(input$remove, {
    
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
    
    # Update the trigger to refresh the DataTable
    trigger(trigger() + 1)
  })
  
  # output sheet --------------------------------------------------------------
  output$calender <- renderDataTable({
    
    # Reactively trigger the DataTable update
    trigger()
    
    # load a kalender sheets from docs
    termine <- read_sheet(tabelle_id())
    
    # prepare calender to dataframe
    kalender <- prepare_kalender(termine, feiertage_df)
    
    # make kalender a datatable
    kalender <- datatable_kalender(kalender)
    
    return(kalender)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
