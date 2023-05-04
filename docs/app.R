library(shiny)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(janitor)
library(ggpubr)
library(shinyWidgets)
library(plyr)


# Read and preprocess data
merged_data_allt <- read.csv("merged_data_allt.csv")
merged_data <- subset(merged_data_allt, stod %in% c("C4", "A7", "B5", "B8", "E4", "E3"))
colnames(merged_data) <- janitor::make_clean_names(colnames(merged_data))

text <- read.csv("text.csv")
text$text <- paste0("*", gsub(": ", "*  ", text$text))
# Create a data frame with merged_data column names and their corresponding text
# Create a data frame with merged_data column names and their corresponding text
column_mapping <- data.frame(
  merged_data_col = c("major_group", "food_source", "motility", "habit", "om_ca_he", "food_size_type", "feed_mode", "feeding_guild"),
  text = text$text[c(1, 2, 6, 7, 3, 4, 5, 8)]  # Select only the relevant rows
)


# Update the merged_data column names to be more user-friendly
colnames(merged_data)[which(colnames(merged_data) %in% column_mapping$merged_data_col)] <- column_mapping$merged_data_col

choices = column_mapping$merged_data_col

selected_cols <- c("major_group","food_source","motility","habit","om_ca_he","food_size_type", "feed_mode","feeding_guild" )
years <- c(1999, 2013, 2014, 2015, 2016, 2017)

# Create custom color palette and map for selected columns
combined_palette <- c(brewer.pal(8, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))
column_color_maps <- setNames(lapply(selected_cols, function(col) {
  setNames(combined_palette[seq(1, length(combined_palette), length.out = length(unique(merged_data[[col]])))], unique(merged_data[[col]]))
}), selected_cols)

# User Interface
ui <- fluidPage(
  titlePanel("Taxonomic and Feeding Guild Classification"),
  fluidRow(column(12,prettyRadioButtons(
    inputId = "selected_col", 
    label = "Select a Column:",
    choices = selected_cols,
    selected = selected_cols[1],
    inline = TRUE,
    fill = TRUE,
    animation = "pulse"
  ))),
  fluidRow(column(12, htmlOutput("selected_text"))),
  # Add the tabsetPanel here
  fluidRow(column(12, tabsetPanel(
    tabPanel("Plots by Stations", plotOutput("barPlots", height = "600px")),
    tabPanel("Plots by Years", plotOutput("barPlotsReversed", height = "600px"))
  )))
)



server <- function(input, output) {
  output$selected_text <- renderUI({
    selected_row <- column_mapping[column_mapping$merged_data_col == input$selected_col, "text"]
    HTML(selected_row[1])
  })
  
  output$barPlots <- renderPlot({
    # Get the column name in merged_data corresponding to the input$selected_col
    selected_col_in_merged_data <- column_mapping[column_mapping$merged_data_col == input$selected_col, "merged_data_col"]
    
    plot_list <- lapply(years, function(year) {
      data_col <- na.omit(merged_data[, c("artal", "stod", selected_col_in_merged_data, "n")])
      colnames(data_col)[3] <- "col"
      agg_data <- aggregate(n ~ artal + stod + col, data_col, sum)
      prop_data <- ddply(agg_data, .(artal, stod), transform, prop_n = n / sum(n))
      plot_data <- subset(melt(prop_data, id.vars = c("artal", "stod", "col")), artal == year & variable != "n")
      ggplot(plot_data, aes(x = factor(stod), y = value, fill = col)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = column_color_maps[[input$selected_col]]) +
        labs(x = "Stations", y = "Proportion", title = paste("Year", year, input$selected_col, sep = " - ")) +
        theme_bw() +
        theme(axis.text = element_text(lineheight = 5), axis.title = element_text(lineheight = 5))
    })
    
    ggarrange(plotlist = plot_list[c(2:6, 1)], ncol = 3, nrow = 2)
  })
    
    # New renderPlot for "barPlotsReversed"
    output$barPlotsReversed <- renderPlot({
      # Similar to the original renderPlot, but with "artal" and "stod" reversed
      selected_col_in_merged_data <- column_mapping[column_mapping$merged_data_col == input$selected_col, "merged_data_col"]
      unique_stations <- unique(merged_data$stod)
      
      plot_list <- lapply(unique_stations, function(station) {
        data_col <- na.omit(merged_data[, c("artal", "stod", selected_col_in_merged_data, "n")])
        colnames(data_col)[3] <- "col"
        agg_data <- aggregate(n ~ artal + stod + col, data_col, sum)
        prop_data <- ddply(agg_data, .(artal, stod), transform, prop_n = n / sum(n))
        plot_data <- subset(melt(prop_data, id.vars = c("artal", "stod", "col")), stod == station & variable != "n")
        ggplot(plot_data, aes(x = factor(as.character(artal), levels = as.character(c(2013:2017, 1999))), y = value, fill = col)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = column_color_maps[[input$selected_col]]) +
          labs(x = "Years", y = "Proportion", title = paste("Station", station, input$selected_col, sep = " - ")) +
          theme_bw() +
          theme(axis.text = element_text(lineheight = 5), axis.title = element_text(lineheight = 5))
      })
    
      ggarrange(plotlist = plot_list, ncol = 3, nrow = 2)
  })
}



# Run the app
shinyApp(ui = ui, server = server)
