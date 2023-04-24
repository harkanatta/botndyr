library(shiny)
library(dplyr)
library(DT)

df <- data.frame(
  Artal = c(1999, 1999, 2013, 2013, 2014, 2014, 2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2017, 2017),
  Flokkun = c("Ampharete acutifrons", "Bivalvia", "Harmothoe", "Polynoidae", "Praxillella", "Syllidae", "Syllidae", "Mya", "Mytilidae", "Ampharetidae", "Aricidea", "Capitellidae", "Cirratulidae", "Cossuridae", "Nephtyidae", "Pectinariidae", "Phyllodocida", "Spio", "Spionidae", "Syllidae", "Cardiidae", "Cardiidae", "Mya", "Maldanidae"),
  Name = c("Ampharetinae", "NA", "Harmothoe extenuata", "Harmothoe extenuata", "Praxillella praetermissa", "Syllis cornuta", "Syllis cornuta", "Mya arenaria", "Mytilus edulis", "NA", "Amphitrite cirrata", "Capitella capitata", "Cirratulus cirratus", "Cossura longocirrata", "Nephtys", "Pectinaria koreni", "Phyllodoce maculata", "Spio filicornis", "Spio filicornis", "Syllis", "Cardium", "Cardium", "Mya arenaria", "Praxillella praetermissa")
)


ui <- fluidPage(
  titlePanel("Add New Row"),
  sidebarLayout(
    sidebarPanel(
      numericInput("artal", "Artal:", min = 1900, max = 2100, value = 2000),
      textInput("flokkun", "Flokkun:"),
      textInput("name", "Name:")
    ),
    mainPanel(
      actionButton("add", "Add Row"),
      br(),
      DTOutput("table"),
      downloadButton("downloadData", "Download Data")
    )
  )
)

server <- function(input, output) {
  
  df_new <- reactiveVal(df)
  
  observeEvent(input$add, {
    new_row <- tibble(
      Artal = input$artal,
      Flokkun = input$flokkun,
      Name = input$name
    )
    df_new(df_new() %>% bind_rows(new_row) %>% 
             mutate(New_Column = paste0('Artal == "', Artal, '" & Flokkun == "', Flokkun, 
                                        '" ~ "', Name, '"')))
  })
  
  output$table <- renderDT(df_new()[ order(as.numeric(row.names(df_new())),decreasing = TRUE), ], options = list(pageLength = 100))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_new(), file)
    }
  )
}

shinyApp(ui, server)
