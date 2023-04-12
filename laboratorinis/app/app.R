library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinydashboard)

ui = dashboardPage(skin = 'green',
  dashboardHeader(
    title = "Sodros duomenys"),
  
  #Side panel     
  dashboardSidebar(
      selectInput("company", label = "Pasirinkite įmonę", choices = NULL, selected = NULL)
  ),
  
  # Main panel with 3 tabs
  dashboardBody(
    tabsetPanel(
      tabPanel("Vidutinė alga", plotOutput(outputId = "avgWage")), 
      tabPanel("Apdraustųjų skaičius", plotOutput(outputId = "insured")), 
      tabPanel("Mokesčiai", plotOutput(outputId = "tax"))
    )
  )
)

server <- function(input, output, session) {
  
  data = read_csv("../data/lab_sodra.csv")
  dataCode = data %>% 
    filter(ecoActCode == 471100) %>%
    mutate(month = as.factor(substr(month, 5, 6)))
  
  updateSelectizeInput(session, "company", choices = dataCode$name)
  
  #Graph for average wage
  output$avgWage = renderPlot({
    dataCode %>%
      filter(name == input$company) %>%
        ggplot(aes(x = month, y = avgWage, group = name)) + 
        geom_line(color = 'red') +
        theme_bw() +
        labs(title = "Vidutinės algos kitimas 2022 metais", x = "Mėnesiai", y = "Vidutinė alga") +
        scale_y_continuous(breaks = seq(0, 6500, 100))
    })
  
  #Graph for insured workers
  output$insured = renderPlot({
    dataCode %>%
      filter(name == input$company) %>%
      ggplot(aes(x = month, y = numInsured, group = name)) +
      geom_line(color = 'red') +
      theme_bw() +
      labs(title = "Apdraustų darbuotojų skaičiaus kitimas 2022 metais", x = "Mėnesiai", y = "Apdraustųjų skaičius")
  })
  
  #Graph for taxes
  output$tax = renderPlot({
    dataCode %>%
      filter(name == input$company) %>%
      ggplot(aes(x = month, y = tax, group = name)) +
      geom_line(color = 'red') +
      theme_bw() +
      labs(title = "Mokesčių kitimas 2022 metais", x = "Mėnesiai", y = "Mokesčiai")
  })
}

shinyApp(ui, server)
