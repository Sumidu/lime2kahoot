# file_processing.R
# Standalone file processing functions for LimeSurvey to Kahoot converter
# Can be run independently for testing

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)

# Function to find column by pattern matching
find_column <- function(df, patterns) {
  result <- patterns %>%
    map(~ str_subset(colnames(df), regex(.x, ignore_case = TRUE))) %>%
    detect(~ length(.x) > 0)

  if (!is.null(result) && length(result) > 0) {
    cat("Found column matching: ", result[1], "\n", sep = "")
    return(result[1])
  }
  return(NA_character_)
}

# Process LimeSurvey quiz CSV
process_quiz_csv <- function(filepath) {
  cat("\n=== Processing Quiz CSV ===\n")
  
  df <- read_csv(filepath, locale = locale(encoding = "UTF-8"),
                 show_col_types = FALSE)
  
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
  required_cols <- tibble(
    name = c("Question", "Answer A", "Answer B", "Answer C", "Answer D", "Correct Answer"),
    col = list(question_col, answer_a_col, answer_b_col, answer_c_col, answer_d_col, correct_col)
  )

  missing_cols <- required_cols %>%
    filter(map_lgl(col, is.na)) %>%
    pull(name)

  if (length(missing_cols) > 0) {
    stop(str_c("Could not find columns: ", str_c(missing_cols, collapse = ", ")))
  }
  
  # Parse dates and create clean data with pipes
  clean_data <- df %>%
    mutate(
      ParsedDate = if (!is.na(date_col)) {
        tryCatch(
          as.Date(parse_date_time(.data[[date_col]], orders = c("ymd HMS", "dmy HMS", "mdy HMS"))),
          error = function(e) Sys.Date()
        )
      } else {
        Sys.Date()
      }
    ) %>%
    transmute(
      SubmitDate = ParsedDate,
      GroupName = if (!is.na(group_col)) as.character(.data[[group_col]]) else "",
      Password = if (!is.na(password_col)) as.character(.data[[password_col]]) else "",
      Question = as.character(.data[[question_col]]),
      Answer_1 = as.character(.data[[answer_a_col]]),
      Answer_2 = as.character(.data[[answer_b_col]]),
      Answer_3 = as.character(.data[[answer_c_col]]),
      Answer_4 = as.character(.data[[answer_d_col]]),
      Time_Limit = 60,
      Correct = as.character(.data[[correct_col]]),
      Points = 1
    )
  
  # Remove empty rows, convert letter answers, and add columns
  clean_data <- clean_data %>%
    filter(!is.na(Question), str_trim(Question) != "") %>%
    mutate(
      Correct = Correct %>%
        str_replace_all(c("A" = "1", "B" = "2", "C" = "3", "D" = "4")),
      across(everything(), ~ if_else(is.na(.), "", as.character(.))),
      Selected = TRUE,
      Nr = row_number()
    )
  
  cat("Processed", nrow(clean_data), "quiz questions\n")
  cat("\nSample data:\n")
  print(clean_data %>% select(Nr, GroupName, Question, Points) %>% head(3))
  
  return(clean_data)
}

# Process participants CSV (wide format with multiple member columns)
process_participants_csv <- function(filepath) {
  # filepath <- "data/groups.csv" # for debugging
  cat("\n=== Processing Participants CSV ===\n")

  participants <- read_csv(filepath, show_col_types = FALSE)

  cat("Original columns:\n")
  print(colnames(participants))
  cat("\nNumber of rows (groups):", nrow(participants), "\n\n")

  # Remove rows with zero participants and pivot to long format
  clean_participants <- participants %>%
    filter(`Group Size` != 0) %>%
    select(-c(`Group Description`, starts_with("Assigned"))) %>%
    select(`Group Name`, ends_with("ID Number")) %>%
    pivot_longer(
      cols = starts_with("Member") | starts_with("Student"),
      names_to = "Member_Column",
      values_to = "Matrikelnummer"
    ) %>%
    mutate(Member_Column = as.numeric(str_extract(Member_Column, "\\d+"))) %>%
    drop_na() %>%
    distinct()
  
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
