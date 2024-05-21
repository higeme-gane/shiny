# Load necessary libraries
library(shiny)
library(tidyverse)
library(stringi)

# Define Shiny UI
ui <- fluidPage(
  titlePanel("Search by Hospital Name"),
  sidebarLayout(
    sidebarPanel(
      textInput("search_input", "Enter hospital name:", ""),
      actionButton("search_button", "Search"),
      selectInput("hospital_select", "Select a hospital:", choices = NULL),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      tableOutput("search_output")
    )
  )
)

# Define Shiny server logic
server <- function(input, output, session) {
  df <- read_csv("https://www.dropbox.com/scl/fi/461fd9mjxag2jxinf3blx/df_hosp_master_2021.csv?rlkey=ujpmfuj3cxok8v0qjccgxi8yp&raw=1")
  df_each_hosp <- reactiveVal(read_csv("https://www.dropbox.com/scl/fi/t7iyv9371scu4r7gkd6b7/df_long_inc_kekkaku_2013_2021.csv?rlkey=jn41n4sbxeg09x0bzhcplqzyx&raw=1"))
  selected_hospital <- reactiveVal()
  
  observeEvent(input$search_button, {
    # Get search term
    search_term <- input$search_input
    
    # Find matching hospital names
    matching_hospitals <- df[stri_detect_fixed(df$hosp_name_2021,
                                               search_term),]
    
    # Update the hospital selection input with the matching hospital names
    updateSelectInput(session, "hospital_select",
                      choices = matching_hospitals$hosp_name_2021)
  })
  
  observeEvent(input$hospital_select, {
    # Get selected hospital name
    selected_hospital(input$hospital_select)
    
    # Find kokuji_2021 for the selected hospital
    kokuji_master <- df[df$hosp_name_2021 == selected_hospital(), "kokuji_2021"]
    
    # Filter the data using kokuji_num
    df_each_hosp_temp <- df_each_hosp() %>%
      filter(kokuji_num %in% kokuji_master)
    
    # Check if the filtered data has any rows
    if (nrow(df_each_hosp_temp) > 0) {
      # Process the filtered data
      df_each_hosp_temp <- df_each_hosp_temp %>%
        select(-kokuji_num) %>%
        pivot_wider(names_from = kubun, values_from = value) %>%
        mutate(omit = target / uke_n) %>%
        rename(ratio = omit) %>%
        arrange(fy)
      
      df_each_hosp(df_each_hosp_temp)
      
      # Display the result in the main panel
      output$search_output <- renderTable({
        df_each_hosp()
      })
    } else {
      # Display an empty table if the filtered data has no rows
      output$search_output <- renderTable({
        data.frame()
      })
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(selected_hospital(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_each_hosp(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)