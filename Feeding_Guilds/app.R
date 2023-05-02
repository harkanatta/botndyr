library(plyr)
library(shiny)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(janitor)
library(ggpubr)
library(shinyWidgets)


# Read the merged_data.csv (assuming you have it saved as a CSV)
merged_data <- read.csv("merged_data.csv")
text <- read.csv("text.csv")
text$text <- paste0("*", gsub(": ", "*  ", text$text))
# Clean the column names
colnames(merged_data) <- janitor::make_clean_names(colnames(merged_data))

# Define selected columns for plotting
selected_cols <- c("major_group","food_source","motility","habit","om_ca_he","food_size_type", "feed_mode","feeding_guild" )
years <- c(1999, 2013, 2014, 2015, 2016, 2017)

# Create a custom divergent color palette
max_unique_vals <- max(sapply(selected_cols, function(x) length(unique(merged_data[[x]]))))
combined_palette <- c(brewer.pal(8, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

# Create a named color map for each column with unique values as names
column_color_maps <- lapply(selected_cols, function(col) {
  unique_vals <- unique(merged_data[[col]])
  colors <- combined_palette[seq(1, length(combined_palette), length.out = length(unique_vals))]
  setNames(colors, unique_vals)
})

names(column_color_maps) <- selected_cols

# User Interface
ui <- fluidPage(
  titlePanel("Melted Data Bar Graph"),
  fluidRow(
    column(12,
           # radioButtons("selected_col", "Select a Column:",
           #              choices = selected_cols,
           #              inline = TRUE)
           prettyRadioButtons(
             inputId = "selected_col", 
             label = "Select a Column:",
             choices = selected_cols,
             selected = selected_cols[1],
             #direction = "horizontal",
             inline = TRUE,
             fill = TRUE,
             animation = "pulse"
           )
           
    )
  ),
  fluidRow(
    column(12,
           plotOutput("barPlots", height = "600px")
    )
  ),
  fluidRow(
    column(12,
          # verbatimTextOutput("selected_text")
          htmlOutput("selected_text")
         
    )
  )
  
)

# Server Logic
server <- function(input, output) {
  output$barPlots <- renderPlot({
    selected_row <- text[text$find_name == input$selected_col, "text"]
    output$selected_text <- renderPrint(selected_row)
    col <- input$selected_col
    LabX <- col
    
    plot_list <- list()
    
    for (year in years) {
      # Create a data frame for the current column
      data_col <- merged_data[, c("artal", "stod", col, "n")]
      
      # Remove rows with missing values
      data_col <- na.omit(data_col)
      colnames(data_col)[3] <- "col"
      
      # Aggregate N values by Artal, stod, and the current column
      agg_data <- aggregate(n ~ artal + stod + col, data_col, sum)
      
      # Compute proportions for each station
      prop_data <- ddply(agg_data, .(artal, stod), transform, prop_n = n / sum(n))
      
      # Reshape data to long format
      melted_data <- melt(prop_data, id.vars = c("artal", "stod", "col"))
      
      # Filter data for the current year
      plot_data <- subset(melted_data, artal == year)
      plot_data <- plot_data[plot_data$variable != "n",]
      
      # Create stacked bar plot
      p <- ggplot(plot_data, aes(x = factor(stod), y = value, fill = col)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = column_color_maps[[col]]) +  # Use the named color map for the current column
        xlab("Stations") +
        ylab("Proportion") +
        ggtitle(paste("Year", year, col, sep = " - ")) +
        theme_bw()
      
      plot_list[[as.character(year)]] <- p + theme(axis.text = element_text(lineheight = 5), # Adjust line spacing for axis text
                                                   axis.title = element_text(lineheight = 5)) # Adjust line spacing for axis title
    }
    
    
    
    
    # Arrange the plots in two lines with 1999, 2013, and 2014 in the upper row
    combined_plots <- ggarrange(plotlist = plot_list[c(2:6,1)], ncol = 3, nrow = 2)
    print(combined_plots)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
