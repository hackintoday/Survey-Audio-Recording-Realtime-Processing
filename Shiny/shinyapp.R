# Test Qualtrics Data Processing for Shiny App
# This tests the exact processing logic used in the Shiny app

library(qualtRics)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)

# Set your credentials
Sys.setenv(QUALTRICS_API_KEY = "")
Sys.setenv(QUALTRICS_BASE_URL = "")

# Fetch your survey data
cat("Fetching survey data...\n")
surveys <- all_surveys()
survey_id <- surveys$id[17]  # Change this number to test different surveys
data <- fetch_survey(survey_id)

cat(paste("Testing with survey:", surveys$name[17], "\n"))
cat(paste("Total responses:", nrow(data), "\n\n"))

# Get question columns (excluding metadata)
metadata_cols <- c("ResponseId", "StartDate", "EndDate", "Status", 
                   "IPAddress", "Progress", "Duration (in seconds)",
                   "Finished", "RecordedDate", "RecipientLastName",
                   "RecipientFirstName", "RecipientEmail", "ExternalReference",
                   "LocationLatitude", "LocationLongitude", "DistributionChannel",
                   "UserLanguage")
question_cols <- names(data)[!names(data) %in% metadata_cols]

cat("Available questions:\n")
for(i in 1:length(question_cols)) {
  cat(paste(i, ".", question_cols[i], "\n"))
}

# Test the two columns you care about
cat("\n=== Testing Your Two Key Columns ===\n")

# Column 11: ProximityScore
cat("\n--- Testing ProximityScore ---\n")
question_col <- "__js_QID1_ProximityScore"

if(question_col %in% names(data)) {
  question_data <- data[[question_col]]
  question_data <- question_data[!is.na(question_data)]
  
  cat(paste("Non-NA responses:", length(question_data), "\n"))
  cat("Sample values:\n")
  print(head(question_data, 5))
  
  # Convert to numeric if needed
  if(!is.numeric(question_data)) {
    question_data <- as.numeric(as.character(question_data))
    question_data <- question_data[!is.na(question_data)]
  }
  
  # Calculate statistics
  mean_val <- mean(question_data, na.rm = TRUE)
  median_val <- median(question_data, na.rm = TRUE)
  
  cat(paste("\nMean:", round(mean_val, 2), "\n"))
  cat(paste("Median:", round(median_val, 2), "\n"))
  cat(paste("Min:", min(question_data), "\n"))
  cat(paste("Max:", max(question_data), "\n"))
  
  # Create distribution plot
  df <- data.frame(score = question_data)
  
  p <- ggplot(df, aes(x = score)) +
    geom_histogram(bins = 30, fill = "#3498db", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_val), color = "darkgreen", linetype = "dashed", size = 1) +
    annotate("text", x = mean_val, y = Inf, label = paste("Mean:", round(mean_val, 2)), 
             vjust = 2, hjust = -0.1, color = "red") +
    annotate("text", x = median_val, y = Inf, label = paste("Median:", round(median_val, 2)), 
             vjust = 4, hjust = -0.1, color = "darkgreen") +
    labs(title = "ProximityScore Distribution",
         x = "Score",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  
  print(p)
  cat("\n✓ ProximityScore distribution created!\n")
} else {
  cat("✗ ProximityScore column not found\n")
}

# Column 12: ExactMatch
cat("\n--- Testing ExactMatch ---\n")
question_col <- "__js_QID1_ExactMatch"

if(question_col %in% names(data)) {
  question_data <- data[[question_col]]
  question_data <- question_data[!is.na(question_data)]
  
  cat(paste("Non-NA responses:", length(question_data), "\n"))
  cat("Sample values:\n")
  print(head(question_data, 5))
  
  # Check if it's text or numeric
  is_text <- is.character(question_data) || is.factor(question_data)
  
  if(is_text) {
    cat("\nText data - Creating word cloud...\n")
    
    # Process text
    text_df <- tibble(text = question_data)
    
    word_freq <- text_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word") %>%
      filter(str_length(word) > 2) %>%
      count(word, sort = TRUE) %>%
      filter(n >= 1) %>%  # Lower threshold for your data
      head(100)
    
    cat("\nTop words:\n")
    print(head(word_freq, 20))
    
    if(nrow(word_freq) > 0) {
      cat("\n✓ Word cloud data ready!\n")
      wc <- wordcloud2(word_freq, size = 0.7)
      print(wc)
    } else {
      cat("\n✗ Not enough words for word cloud\n")
    }
    
  } else {
    cat("\nNumeric data - Creating distribution...\n")
    
    # Convert to numeric if needed
    if(!is.numeric(question_data)) {
      question_data <- as.numeric(as.character(question_data))
      question_data <- question_data[!is.na(question_data)]
    }
    
    # Calculate statistics
    mean_val <- mean(question_data, na.rm = TRUE)
    median_val <- median(question_data, na.rm = TRUE)
    
    cat(paste("\nMean:", round(mean_val, 2), "\n"))
    cat(paste("Median:", round(median_val, 2), "\n"))
    cat(paste("Min:", min(question_data), "\n"))
    cat(paste("Max:", max(question_data), "\n"))
    
    # Create distribution plot
    df <- data.frame(score = question_data)
    
    p <- ggplot(df, aes(x = score)) +
      geom_histogram(bins = 30, fill = "#e74c3c", color = "white", alpha = 0.8) +
      geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = median_val), color = "darkgreen", linetype = "dashed", size = 1) +
      annotate("text", x = mean_val, y = Inf, label = paste("Mean:", round(mean_val, 2)), 
               vjust = 2, hjust = -0.1, color = "red") +
      annotate("text", x = median_val, y = Inf, label = paste("Median:", round(median_val, 2)), 
               vjust = 4, hjust = -0.1, color = "darkgreen") +
      labs(title = "ExactMatch Distribution",
           x = "Score",
           y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    
    print(p)
    cat("\n✓ ExactMatch distribution created!\n")
  }
} else {
  cat("✗ ExactMatch column not found\n")
}

cat("\n=== Test Complete ===\n")
cat("If you see the visualization above, your data is processing correctly!\n")

cat("=== Testing Word Cloud with Text Columns ===\n\n")

# Test Transcript column
cat("--- Testing Transcript Column ---\n")
if("__js_QID1_Transcript" %in% names(data)) {
  transcript_data <- data[["__js_QID1_Transcript"]]
  transcript_data <- transcript_data[!is.na(transcript_data)]
  
  cat(paste("Non-NA responses:", length(transcript_data), "\n"))
  cat("Sample transcripts:\n")
  print(head(transcript_data, 3))
  
  if(length(transcript_data) > 0) {
    # Process for word cloud
    text_df <- tibble(text = transcript_data)
    
    word_freq <- text_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word") %>%
      filter(str_length(word) > 2) %>%
      count(word, sort = TRUE) %>%
      filter(n >= 1) %>%
      head(100)
    
    cat("\nTop 20 words:\n")
    print(head(word_freq, 20))
    
    if(nrow(word_freq) > 0) {
      cat("\n✓ Creating word cloud...\n")
      wc <- wordcloud2(word_freq, size = 0.7)
      print(wc)
      cat("✓ Word cloud created!\n")
    } else {
      cat("✗ No words found\n")
    }
  }
} else {
  cat("✗ Transcript column not found\n")
}

cat("\n--- Testing TargetWord Column ---\n")
if("TargetWord" %in% names(data)) {
  target_data <- data[["TargetWord"]]
  target_data <- target_data[!is.na(target_data)]
  
  cat(paste("Non-NA responses:", length(target_data), "\n"))
  cat("Sample target words:\n")
  print(head(target_data, 10))
  
  if(length(target_data) > 0) {
    # Simple word frequency
    word_freq <- tibble(word = target_data) %>%
      count(word, sort = TRUE)
    
    cat("\nWord frequencies:\n")
    print(word_freq)
    
    if(nrow(word_freq) > 0) {
      cat("\n✓ Creating word cloud...\n")
      wc <- wordcloud2(word_freq, size = 1.0)
      print(wc)
      cat("✓ Word cloud created!\n")
    }
  }
} else {
  cat("✗ TargetWord column not found\n")
}

cat("\n=== Test Complete ===\n")