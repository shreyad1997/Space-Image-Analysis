# Load necessary libraries
library(imager)
library(dplyr)
library(caret)
library(randomForest)
library(magick)
library(ggplot2)
library(pROC)
library(plotly)
library(colorspace)

# Function to preprocess images
preprocess_image <- function(file_path) {
  img <- load.image(file_path) %>% 
    resize(64, 64) %>%  # Resize to 64x64
    grayscale()         # Convert to grayscale
  
  as.vector(img)  # Flatten the image
}

# Load the dataset
data_dir <- "/Users/shreya/Desktop/archive/APOC64"
image_files <- list.files(data_dir, pattern = "*.jpg", full.names = TRUE)

# Load labels with proper separator
labels <- read.csv("/Users/shreya/Desktop/archive/infos.csv", stringsAsFactors = FALSE, sep = ";", header = FALSE, fill = TRUE, skip = 1)
colnames(labels) <- c("Filename", "Title")

# Define the categories
planets <- c("Earth", "Mars", "Jupiter", "Saturn", "Venus", "Mercury", "Neptune", "Uranus")

# Function to map specific labels to general categories
map_labels <- function(title) {
  if (grepl("galaxy", title, ignore.case = TRUE)) {
    return("Galaxy")
  } else if (any(sapply(planets, function(planet) grepl(planet, title, ignore.case = TRUE)))) {
    return("Planet")
  } else {
    return("Space")
  }
}

# Apply mapping to create a new category column
labels$category <- sapply(labels$Title, map_labels)

# Check the category distribution (Graph 1)
category_dist <- ggplot(labels, aes(x = category, fill = category)) +
  geom_bar() +
  labs(title = "Category Distribution", x = "Category", y = "Count") +
  theme_minimal()
print(category_dist)

# Preprocess all images
image_data <- lapply(image_files, preprocess_image)
image_matrix <- do.call(rbind, image_data)

# Combine image data with new labels
image_df <- data.frame(image_matrix)
image_df$category <- factor(labels$category)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(image_df$category, p = 0.8, list = FALSE)
train_data <- image_df[train_index, ]
test_data <- image_df[-train_index, ]

# Train a random forest model
rf_model <- randomForest(category ~ ., data = train_data, ntree = 100)

# Make predictions
predictions <- predict(rf_model, newdata = test_data)

# Confusion matrix (Graph 2)
conf_matrix <- confusionMatrix(predictions, test_data$category)
conf_matrix_plot <- as.data.frame(conf_matrix$table)
conf_matrix_heatmap <- ggplot(conf_matrix_plot, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()
print(conf_matrix_heatmap)

# ROC curve (Graph 3)
roc_curves <- multiclass.roc(test_data$category, as.numeric(predictions))
roc_list <- roc_curves$rocs

# Plot ROC curves for each class
roc_plot <- ggplot() +
  labs(title = "ROC Curves", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

for (roc in roc_list) {
  roc_df <- data.frame(
    specificity = 1 - roc$specificities,
    sensitivity = roc$sensitivities
  )
  roc_plot <- roc_plot + 
    geom_line(data = roc_df, aes(x = specificity, y = sensitivity))
}
print(roc_plot)

# Accuracy over time (Graph 4)
model_accuracy <- rf_model$err.rate
accuracy_df <- data.frame(Trees = 1:nrow(model_accuracy), Error = model_accuracy[, "OOB"])
accuracy_plot <- ggplot(accuracy_df, aes(x = Trees, y = Error)) +
  geom_line(color = "blue") +
  labs(title = "Model Accuracy Over Time", x = "Number of Trees", y = "OOB Error Rate") +
  theme_minimal()
print(accuracy_plot)

# Function to analyze a new image
analyze_image <- function(image_path, model) {
  # Load the image
  img <- image_read(image_path)
  img_name <- basename(image_path)
  cat("Analyzing image:", img_name, "\n")
  
  # Get image dimensions
  img_info <- image_info(img)
  cat("Image dimensions:", img_info$width, "x", img_info$height, "\n")
  
  # Convert the image to an imager object for further analysis
  imgr <- load.image(image_path)
  
  # Check the number of color channels
  num_channels <- spectrum(imgr)
  cat("Number of color channels:", num_channels, "\n")
  
  # Remove alpha channel if it exists
  if (num_channels == 4) {
    imgr <- imgr[,,1:3,drop=FALSE]
    num_channels <- 3
  }
  
  # Ensure the image has exactly three color channels (RGB)
  if (num_channels != 3) {
    stop("Error: Image does not have exactly three color channels")
  }
  
  # Separate color channels and convert to data frames
  red_df <- as.data.frame(as.vector(imgr[,,1])) %>% mutate(channel = "Red")
  green_df <- as.data.frame(as.vector(imgr[,,2])) %>% mutate(channel = "Green")
  blue_df <- as.data.frame(as.vector(imgr[,,3])) %>% mutate(channel = "Blue")
  
  # Combine data frames
  color_df <- bind_rows(
    red_df %>% rename(value = `as.vector(imgr[, , 1])`),
    green_df %>% rename(value = `as.vector(imgr[, , 2])`),
    blue_df %>% rename(value = `as.vector(imgr[, , 3])`)
  )
  
  # Plot color intensity histograms
  color_histogram <- ggplot(color_df, aes(x = value, fill = channel)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    facet_wrap(~channel, scales = "free") +
    labs(title = "Color Intensity Histograms", x = "Intensity", y = "Frequency") +
    theme_minimal()
  
  # Save the color histogram to the desktop
  color_histogram_path <- "/Users/shreya/Desktop/color_intensity_histogram.png"
  ggsave(color_histogram_path, plot = color_histogram, width = 8, height = 6)
  cat("Color intensity histogram saved to:", color_histogram_path, "\n")
  
  # Edge detection
  edges <- imgradient(imgr, "xy") %>% enorm()
  
  # Convert edges to data frame for plotting
  edges_df <- as.data.frame(edges)
  
  # Plot edge detection result
  edge_plot <- ggplot(edges_df, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "black", high = "white") +
    labs(title = "Edge Detection", x = "X", y = "Y") +
    theme_minimal()
  
  # Save the edge detection plot to the desktop
  edge_plot_path <- "/Users/shreya/Desktop/edge_detection_plot.png"
  ggsave(edge_plot_path, plot = edge_plot, width = 8, height = 6)
  cat("Edge detection plot saved to:", edge_plot_path, "\n")
  
  # Preprocess the image for prediction
  img_preprocessed <- imgr %>%
    resize(64, 64) %>%
    grayscale()
  
  # Flatten the preprocessed image if necessary
  img_vector <- as.vector(img_preprocessed)
  
  # Check the dimensions of the flattened image vector
  if (length(img_vector) != 64*64) {
    stop("Error: Flattened image does not have the expected dimensions")
  }
  
  # Create a data frame for prediction
  img_matrix <- as.data.frame(t(img_vector))
  colnames(img_matrix) <- colnames(train_data)[-ncol(train_data)]  # Match training data columns
  
  # Predict the category
  category_prediction <- predict(model, newdata = img_matrix)
  
  # Output the prediction with a sentence
  cat("Predicted category:", category_prediction, "\n")
  cat("Your image is a", tolower(category_prediction), ".\n")
}

# Function to repeatedly ask for image names and analyze them
repeat_analysis <- function(model) {
  repeat {
    image_name <- readline(prompt = "Enter the image name (e.g., space.jpg): ")
    image_path <- file.path("/Users/shreya/Desktop", image_name)
    analyze_image(image_path, model)
    
    another <- readline(prompt = "Do you want to analyze another image? (yes/no): ")
    if (tolower(another) != "yes") {
      break
    }
  }
  cat("Your analysis is complete.\n")
}

# Start the analysis process
repeat_analysis(rf_model)