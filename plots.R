library(ggplot2)

data <- read.csv("Cleaned_data.csv")

for (column_name in names(data)) {
  
  if (is.factor(data[[column_name]]) || is.character(data[[column_name]])) {
    data[[column_name]] <- as.factor(data[[column_name]])
    
    plot <- ggplot(data, aes_string(x = column_name, fill = column_name)) +
      geom_bar(fill = "orange",color = "black") +
      labs(title = paste("Bar Plot for", column_name),
           x = column_name,
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if (is.numeric(data[[column_name]])) {
    plot <- ggplot(data, aes_string(x = column_name)) +
      geom_histogram(binwidth = 10, fill = "orange", color = "black") +  
      labs(title = paste("Histogram for", column_name),
           x = column_name,
           y = "Frequency") +
      theme_minimal()
  }
  

  filename <- paste0("plot_", column_name, ".jpeg")
  
  ggsave(filename, plot = plot, width = 8, height = 6, dpi = 300)
}
