# file_processing.R
# Standalone file processing functions for LimeSurvey to Kahoot converter
# Can be run independently for testing

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Function to find column by pattern matching
find_column <- function(df, patterns) {
  for (pattern in patterns) {
    matches <- grep(pattern, colnames(df), ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      cat("Found column matching '", pattern, "': ", matches[1], "\n", sep = "")
      return(matches[1])
    }
  }
  return(NA)
}

# Process LimeSurvey quiz CSV
process_quiz_csv <- function(filepath) {
  cat("\n=== Processing Quiz CSV ===\n")
  
  df <- read.csv(filepath, stringsAsFactors = FALSE, 
                 fileEncoding = "UTF-8", check.names = TRUE)
  
  cat("Original columns:\n")
  print(colnames(df))
  cat("\n")
  
  # Find required columns
  date_col <- find_column(df, c("Datum\\.Abgeschickt", "Date", "Submitted"))
  group_col <- find_column(df, c("Gruppennamen", "Group"))
  password_col <- find_column(df, c("Gruppenpasswort", "Password"))
  question_col <- find_column(df, c("Frage\\.ein", "Question"))
  answer_a_col <- find_column(df, c("Antwort\\.A", "Answer.*A"))
  answer_b_col <- find_column(df, c("Antwort\\.B", "Answer.*B"))
  answer_c_col <- find_column(df, c("Antwort\\.C", "Answer.*C"))
  answer_d_col <- find_column(df, c("Antwort\\.D", "Answer.*D"))
  correct_col <- find_column(df, c("korrekte\\.Antwort", "Correct"))
  
  # Check for missing columns
  missing_cols <- c()
  if (is.na(question_col)) missing_cols <- c(missing_cols, "Question")
  if (is.na(answer_a_col)) missing_cols <- c(missing_cols, "Answer A")
  if (is.na(answer_b_col)) missing_cols <- c(missing_cols, "Answer B")
  if (is.na(answer_c_col)) missing_cols <- c(missing_cols, "Answer C")
  if (is.na(answer_d_col)) missing_cols <- c(missing_cols, "Answer D")
  if (is.na(correct_col)) missing_cols <- c(missing_cols, "Correct Answer")
  
  if (length(missing_cols) > 0) {
    stop(paste("Could not find columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Parse dates
  if (!is.na(date_col)) {
    tryCatch({
      df$ParsedDate <- as.Date(parse_date_time(df[[date_col]], 
                                               orders = c("ymd HMS", "dmy HMS", "mdy HMS")))
    }, error = function(e) {
      df$ParsedDate <- Sys.Date()
    })
  } else {
    df$ParsedDate <- Sys.Date()
  }
  
  # Create clean data
  clean_data <- data.frame(
    SubmitDate = df$ParsedDate,
    GroupName = if (!is.na(group_col)) as.character(df[[group_col]]) else "",
    Password = if (!is.na(password_col)) as.character(df[[password_col]]) else "",
    Question = as.character(df[[question_col]]),
    Answer_1 = as.character(df[[answer_a_col]]),
    Answer_2 = as.character(df[[answer_b_col]]),
    Answer_3 = as.character(df[[answer_c_col]]),
    Answer_4 = as.character(df[[answer_d_col]]),
    Time_Limit = rep(60, nrow(df)),
    Correct = as.character(df[[correct_col]]),
    Points = rep(1, nrow(df)),
    stringsAsFactors = FALSE
  )
  
  # Remove empty rows
  clean_data <- clean_data[!is.na(clean_data$Question) & 
                             trimws(clean_data$Question) != "", ]
  
  # Convert letter answers to numbers
  clean_data$Correct <- gsub("A", "1", clean_data$Correct)
  clean_data$Correct <- gsub("B", "2", clean_data$Correct)
  clean_data$Correct <- gsub("C", "3", clean_data$Correct)
  clean_data$Correct <- gsub("D", "4", clean_data$Correct)
  
  # Replace NA with empty strings
  clean_data[is.na(clean_data)] <- ""
  
  # Add selection and number columns
  clean_data$Selected <- TRUE
  clean_data$Nr <- seq_len(nrow(clean_data))
  
  cat("Processed", nrow(clean_data), "quiz questions\n")
  cat("\nSample data:\n")
  print(head(clean_data[, c("Nr", "GroupName", "Question", "Points")], 3))
  
  return(clean_data)
}

# Process participants CSV (wide format with multiple member columns)
process_participants_csv <- function(filepath) {
  # filepath <- "data/groups.csv" # for debugging
  cat("\n=== Processing Participants CSV ===\n")
  
  participants <- readr::read_csv(filepath)
  # remove rows with zero participants (Group Size column is 0)
  participants %>% filter(`Group Size` != 0) %>% 
    select(-c(`Group Description`, starts_with("Assigned"))) %>% 
    select(c(`Group Name`, ends_with("ID Number"))) %>% 
    pivot_longer(
      cols = starts_with("Member") | starts_with("Student"),
      names_to = "Member_Column",
      values_to = "Matrikelnummer"
    ) %>% 
    mutate(Member_Column = as.numeric(gsub(".*?(\\d+).*", "\\1", Member_Column))) %>% 
    na.omit() -> clean_participants
  
  
  cat("Original columns:\n")
  print(colnames(participants))
  cat("\nNumber of rows (groups):", nrow(participants), "\n\n")
  
  # Find group name column
  
  
  # Remove duplicates
  clean_participants <- unique(clean_participants)
  
  cat("\n=== Summary ===\n")
  cat("Total groups:", nrow(participants), "\n")
  cat("Total participants:", nrow(clean_participants), "\n")
  cat("Average group size:", round(nrow(clean_participants) / nrow(participants), 1), "\n\n")
  
  cat("Sample pivoted data:\n")
  print(head(clean_participants, 10))
  
  return(clean_participants)
}

# Test function - uncomment and modify paths to test
test_file_processing <- function() {
  # Test quiz CSV
  quiz_data <- process_quiz_csv("data/survey.csv")
  
  # Test participants CSV
  participants_data <- process_participants_csv("data/groups.csv")
  
  cat("\n=== Tests completed ===\n")
}

# Uncomment to run tests:
# test_file_processing()
