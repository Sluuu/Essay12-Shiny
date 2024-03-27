
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(igraph)
library(ggraph)




#Load the Data
data <- readr::read_csv("data/raw_data/Raw_data.csv")

data$Birthplace <- substr(data$Birthplace, 1, 1)
data$Residence <- substr(data$Residence, 1, 1)

ui <- fluidPage(
  titlePanel("Number of victims killed at Auschwitz concentration camp."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selectdata",
                  "Options for the Graph:",
                  choices = c("Birthplace", "Residence", "Religion")),
      uiOutput("letterOrReligionInput")
    ),
    mainPanel(
      plotOutput("murderPlot"),
      DTOutput("murderTable")

      
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"))
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$letterOrReligionInput <- renderUI({
    if (input$selectdata %in% c("Birthplace", "Residence")) {
      checkboxGroupInput("selectLetter",
                         "First letter of the brithplace or residency:",
                         choices = sort(unique(c(data$Birthplace, data$Residence))),
                         selected = sort(unique(c(data$Birthplace, data$Residence)))[1])
    } else {
      checkboxGroupInput("selectReligion",
                         "Religions of the victims:",
                         choices = unique(data$Religion),
                         selected = unique(data$Religion)[1])
    }
  })
  
  
  output$murderPlot <- renderPlot({
    if (input$selectdata %in% c("Birthplace", "Residence")) {
      selected <- if (input$selectdata == "Birthplace") input$selectLetter else input$selectLetter
      data_to_plot <- data %>%
        filter((input$selectdata == "Birthplace" & Birthplace %in% selected) |
                 (input$selectdata == "Residence" & Residence %in% selected)) %>%
        group_by(firstletter = if(input$selectdata == "Birthplace") Birthplace else Residence) %>%
        summarise(Count = n())
      
      ggplot(data_to_plot, aes(x = firstletter, y = Count, fill = firstletter)) +
        geom_bar(stat = "identity") +
        labs(x = "First Letter", y = "Count", title = paste("Amount of victims Murdered by", input$selectdata)) +
        theme_minimal()
      
    } else {
      selected <- input$selectReligion
      data_to_plot <- data %>%
        filter(Religion %in% selected) %>%
        group_by(Religion) %>%
        summarise(Count = n())
      
      ggplot(data_to_plot, aes(x = Religion, y = Count, fill = Religion)) +
        geom_bar(stat = "identity") +
        labs(x = "Religion", y = "Count", title = "Amount of victims Murdered by Religion") +
        theme_minimal()
    }
  })
  
  
  output$murderTable <- renderDT({
    if (input$selectdata %in% c("Birthplace", "Residence")) {
      selected <- if (input$selectdata == "Birthplace") input$selectLetter else input$selectedLetters
      data_to_display <- data %>%
        filter((input$selectdata == "Birthplace" & grepl(paste0("^", selected, collapse = "|"), Birthplace, ignore.case = TRUE)) |
                 (input$selectdata == "Residence" & grepl(paste0("^", selected, collapse = "|"), Residence, ignore.case = TRUE)))
    } else {
      selected <- input$selectReligion
      data_to_display <- data %>%
        filter(Religion %in% selected)
    }
    datatable(data_to_display, options = list(pageLength = 20, scrollX = TRUE))
  })
}

#Run app
shinyApp(ui = ui, server = server)
