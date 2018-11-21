library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel(title = div("BC Liquor Store prices",
                         img(src = "bcliquorstore.jpg", width = "20%", height = "20%",
                             align = "right"))),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("subtypeOutput"),
      sliderInput("alcoholInput", "Alcohol Content", 
                  min(bcl$Alcohol_Content),
                  max(bcl$Alcohol_Content), c(10, 20)),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      DT::dataTableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  output$subtypeOutput <- renderUI({
    selectInput("subtypeInput", "Subtype",
                sort(unique(bcl[bcl$Type == input$typeInput, ]$Subtype)),
                selected = "TABLE WINE WHITE")
  })
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput,
             Alcohol_Content >= input$alcoholInput[1],
             Alcohol_Content <= input$alcoholInput[2],
             Subtype == input$subtypeInput
      ) %>% {
      if (nrow(.) == 0) 
        return(NULL) else .
  }})
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)