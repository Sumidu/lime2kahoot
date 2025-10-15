library(shiny)
library(DT)
library(openxlsx)
library(dplyr)
library(lubridate)

ui <- fluidPage(
  titlePanel("LimeSurvey to Kahoot Quiz Converter"),
  
  tags$head(
    tags$style(HTML("
      .validation-error { background-color: #ffcccc !important; }
      .validation-warning { background-color: #fff3cd !important; }
      .validation-ok { background-color: #d4edda !important; }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("csv_file", "Upload LimeSurvey CSV",
                accept = c(".csv")),
      
      conditionalPanel(
        condition = "output.data_loaded",
        hr(),
        h5("Date Filter"),
        dateInput("start_date", "Start Date:",
                  value = Sys.Date() - 7,
                  max = Sys.Date()),
        dateInput("end_date", "End Date:",
                  value = Sys.Date(),
                  max = Sys.Date()),
        actionButton("apply_filter", "Apply Date Filter", 
                     class = "btn-primary btn-sm"),
        hr(),
        h5("Export"),
        checkboxInput("export_selected_only", "Export selected rows only", TRUE),
        downloadButton("download_excel", "Download Kahoot Excel", 
                       class = "btn-success")
      ),
      
      hr(),
      helpText("Instructions:"),
      helpText("1. Upload CSV file"),
      helpText("2. Filter by date range"),
      helpText("3. Check group names & passwords"),
      helpText("4. Review & edit questions"),
      helpText("5. Select rows to export"),
      helpText("6. Download Excel")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Overview",
                 br(),
                 uiOutput("status_message"),
                 br(),
                 DTOutput("overview_table")
        ),
        tabPanel("Quiz Questions",
                 br(),
                 uiOutput("validation_summary"),
                 br(),
                 DTOutput("data_table")
        ),
        tabPanel("Help",
                 br(),
                 h4("About this app"),
                 p("This app converts quiz questions from LimeSurvey CSV format to Kahoot Excel template format."),
                 h4("Features:"),
                 tags$ul(
                   tags$li(strong("Data Overview tab:"), " Review submission dates, group names, and passwords. Check that student group names and passwords match correctly."),
                   tags$li(strong("Quiz Questions tab:"), " Edit questions and answers. Validation highlights problems automatically."),
                   tags$li(strong("Date Filtering:"), " Focus on submissions from a specific date range."),
                   tags$li(strong("Row Selection:"), " Choose which questions to include in the export.")
                 ),
                 h4("Kahoot Requirements:"),
                 tags$ul(
                   tags$li("Questions: max 120 characters"),
                   tags$li("Answers: max 75 characters each"),
                   tags$li("Time limit: 5, 10, 20, 30, 60, 90, 120, or 240 seconds"),
                   tags$li("Correct answer: 1, 2, 3, or 4 (or combinations like '1,2')")
                 ),
                 h4("Color Coding:"),
                 tags$ul(
                   tags$li(tags$span(style = "background-color: #d4edda; padding: 2px 5px;", "Green"), " - Valid"),
                   tags$li(tags$span(style = "background-color: #fff3cd; padding: 2px 5px;", "Yellow"), " - Warning (close to limit)"),
                   tags$li(tags$span(style = "background-color: #ffcccc; padding: 2px 5px;", "Red"), " - Error (exceeds limit)")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  raw_data <- reactiveVal(NULL)
  quiz_data <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  
  output$data_loaded <- reactive({
    !is.null(raw_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Process uploaded CSV
  observeEvent(input$csv_file, {
    req(input$csv_file)
    
    tryCatch({
      # Read CSV
      df <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE, 
                     fileEncoding = "UTF-8", check.names = TRUE)
      
      # Find columns using flexible pattern matching
      find_column <- function(patterns) {
        for (pattern in patterns) {
          matches <- grep(pattern, colnames(df), ignore.case = TRUE, value = TRUE)
          if (length(matches) > 0) return(matches[1])
        }
        return(NA)
      }
      
      # Find each required column
      date_col <- find_column(c("Datum\\.Abgeschickt", "Date", "Submitted"))
      group_col <- find_column(c("Gruppennamen", "Group"))
      password_col <- find_column(c("Gruppenpasswort", "Password"))
      question_col <- find_column(c("Frage\\.ein", "Question"))
      answer_a_col <- find_column(c("Antwort\\.A", "Answer.*A"))
      answer_b_col <- find_column(c("Antwort\\.B", "Answer.*B"))
      answer_c_col <- find_column(c("Antwort\\.C", "Answer.*C"))
      answer_d_col <- find_column(c("Antwort\\.D", "Answer.*D"))
      correct_col <- find_column(c("korrekte\\.Antwort", "Correct"))
      
      # Check if required columns were found
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
      
      # Parse date column if available
      if (!is.na(date_col)) {
        tryCatch({
          df$ParsedDate <- as.Date(parse_date_time(df[[date_col]], 
                                                   orders = c("ymd HMS", "dmy HMS", "mdy HMS")))
        }, error = function(e) {
          # If date parsing fails, use current date
          df$ParsedDate <- Sys.Date()
        })
      } else {
        df$ParsedDate <- Sys.Date()
      }
      
      # Extract data
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
        stringsAsFactors = FALSE
      )
      
      # Remove empty rows
      clean_data <- clean_data[!is.na(clean_data$Question) & 
                                 trimws(clean_data$Question) != "", ]
      
      if (nrow(clean_data) == 0) {
        stop("No valid questions found in the CSV file")
      }
      
      # Convert letter answers to numbers
      clean_data$Correct <- gsub("A", "1", clean_data$Correct)
      clean_data$Correct <- gsub("B", "2", clean_data$Correct)
      clean_data$Correct <- gsub("C", "3", clean_data$Correct)
      clean_data$Correct <- gsub("D", "4", clean_data$Correct)
      
      # Replace NA with empty strings
      clean_data[is.na(clean_data)] <- ""
      
      # Add selection column and number
      clean_data$Selected <- TRUE
      clean_data$Nr <- seq_len(nrow(clean_data))
      
      raw_data(clean_data)
      
      # Auto-apply initial date filter
      updateDateInput(session, "start_date", value = Sys.Date() - 7)
      updateDateInput(session, "end_date", value = Sys.Date())
      
      # Trigger filter
      filtered <- clean_data[clean_data$SubmitDate >= (Sys.Date() - 7) & 
                               clean_data$SubmitDate <= Sys.Date(), ]
      filtered$Nr <- seq_len(nrow(filtered))
      filtered_data(filtered)
      quiz_data(filtered)
      
      showNotification(paste("Loaded", nrow(clean_data), "total questions,", 
                             nrow(filtered), "in date range"), 
                       type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message), 
                       type = "error", duration = NULL)
    })
  })
  
  # Apply date filter
  observeEvent(input$apply_filter, {
    req(raw_data())
    
    data <- raw_data()
    filtered <- data[data$SubmitDate >= input$start_date & 
                       data$SubmitDate <= input$end_date, ]
    
    if (nrow(filtered) == 0) {
      showNotification("No data in selected date range", type = "warning")
      return()
    }
    
    filtered$Nr <- seq_len(nrow(filtered))
    filtered_data(filtered)
    quiz_data(filtered)
    
    showNotification(paste("Filtered to", nrow(filtered), "questions"), 
                     type = "message", duration = 2)
  })
  
  # Validation function
  validate_row <- function(row) {
    issues <- list()
    
    # Check question length
    q_len <- nchar(row$Question)
    if (q_len > 120) {
      issues$question <- "error"
    } else if (q_len > 110) {
      issues$question <- "warning"
    } else {
      issues$question <- "ok"
    }
    
    # Check answer lengths
    for (i in 1:4) {
      ans_len <- nchar(row[[paste0("Answer_", i)]])
      if (ans_len > 75) {
        issues[[paste0("answer_", i)]] <- "error"
      } else if (ans_len > 70) {
        issues[[paste0("answer_", i)]] <- "warning"
      } else {
        issues[[paste0("answer_", i)]] <- "ok"
      }
    }
    
    # Check time limit
    valid_times <- c(5, 10, 20, 30, 60, 90, 120, 240)
    if (!(row$Time_Limit %in% valid_times)) {
      issues$time <- "error"
    } else {
      issues$time <- "ok"
    }
    
    # Check correct answer format
    correct_vals <- strsplit(as.character(row$Correct), ",")[[1]]
    if (all(correct_vals %in% c("1", "2", "3", "4"))) {
      issues$correct <- "ok"
    } else {
      issues$correct <- "error"
    }
    
    return(issues)
  }
  
  # Status message
  output$status_message <- renderUI({
    if (is.null(filtered_data())) {
      div(class = "alert alert-info",
          icon("info-circle"),
          " Please upload a LimeSurvey CSV file to begin.")
    } else {
      data <- filtered_data()
      selected_count <- sum(data$Selected)
      div(class = "alert alert-success",
          icon("check-circle"),
          sprintf(" Showing %d questions (%d selected for export).", 
                  nrow(data), selected_count))
    }
  })
  
  # Validation summary
  output$validation_summary <- renderUI({
    req(quiz_data())
    
    data <- quiz_data()
    error_count <- 0
    warning_count <- 0
    
    for (i in 1:nrow(data)) {
      issues <- validate_row(data[i, ])
      if (any(unlist(issues) == "error")) error_count <- error_count + 1
      if (any(unlist(issues) == "warning")) warning_count <- warning_count + 1
    }
    
    if (error_count > 0) {
      div(class = "alert alert-danger",
          icon("exclamation-triangle"),
          sprintf(" %d row(s) have validation errors. Red cells exceed Kahoot limits.", error_count))
    } else if (warning_count > 0) {
      div(class = "alert alert-warning",
          icon("exclamation-circle"),
          sprintf(" %d row(s) have warnings. Yellow cells are close to limits.", warning_count))
    } else {
      div(class = "alert alert-success",
          icon("check-circle"),
          " All entries are valid!")
    }
  })
  
  # Overview table
  output$overview_table <- renderDT({
    req(filtered_data())
    
    data <- filtered_data()
    
    # Make sure we have all required columns
    if (!all(c("Selected", "Nr", "SubmitDate", "GroupName", "Password", "Question") %in% colnames(data))) {
      return(NULL)
    }
    
    overview <- data[, c("Selected", "Nr", "SubmitDate", "GroupName", "Password", "Question")]
    
    datatable(
      overview,
      editable = list(target = "cell", disable = list(columns = c(1, 2))),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:2),
          list(width = '40px', targets = 0),
          list(width = '40px', targets = 1),
          list(width = '100px', targets = 2),
          list(width = '150px', targets = 3:4),
          list(width = '300px', targets = 5)
        )
      ),
      rownames = FALSE,
      selection = 'none',
      class = 'cell-border stripe'
    )
  })
  
  # Quiz data table with validation highlighting
  output$data_table <- renderDT({
    req(quiz_data())
    
    data <- quiz_data()
    display_data <- data[, c("Selected", "Nr", "Question", "Answer_1", "Answer_2", 
                             "Answer_3", "Answer_4", "Time_Limit", "Correct")]
    
    dt <- datatable(
      display_data,
      editable = list(target = "cell", disable = list(columns = c(1, 2))),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(width = '30px', targets = 0),
          list(width = '30px', targets = 1),
          list(width = '300px', targets = 2),
          list(width = '150px', targets = 3:6),
          list(width = '80px', targets = 7:8)
        )
      ),
      rownames = FALSE,
      selection = 'none',
      class = 'cell-border stripe'
    )
    
    # Apply validation styling
    for (i in 1:nrow(data)) {
      issues <- validate_row(data[i, ])
      
      # Question column (index 2)
      if (issues$question == "error") {
        dt <- dt %>% formatStyle(3, target = 'row',
                                 backgroundColor = styleEqual(i, '#ffcccc'))
      } else if (issues$question == "warning") {
        dt <- dt %>% formatStyle(3, target = 'row',
                                 backgroundColor = styleEqual(i, '#fff3cd'))
      }
    }
    
    dt
  })
  
  # Handle cell edits in overview
  observeEvent(input$overview_table_cell_edit, {
    info <- input$overview_table_cell_edit
    data <- filtered_data()
    
    col_name <- colnames(data)[info$col + 1]
    data[info$row, col_name] <- info$value
    
    filtered_data(data)
    quiz_data(data)
  })
  
  # Handle cell edits in quiz table
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    data <- quiz_data()
    
    display_cols <- c("Selected", "Nr", "Question", "Answer_1", "Answer_2", 
                      "Answer_3", "Answer_4", "Time_Limit", "Correct")
    col_name <- display_cols[info$col + 1]
    
    # Handle checkbox for Selected column
    if (col_name == "Selected") {
      data[info$row, col_name] <- as.logical(info$value)
    } else {
      data[info$row, col_name] <- info$value
    }
    
    quiz_data(data)
    filtered_data(data)
  })
  
  # Download handler
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("kahoot_quiz_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(quiz_data())
      
      tryCatch({
        data <- quiz_data()
        
        # Filter to selected rows if checkbox is checked
        if (input$export_selected_only) {
          data <- data[data$Selected, ]
        }
        
        if (nrow(data) == 0) {
          showNotification("No rows selected for export", type = "error")
          return()
        }
        
        # Validate before export
        has_errors <- FALSE
        for (i in 1:nrow(data)) {
          issues <- validate_row(data[i, ])
          if (any(unlist(issues) == "error")) {
            has_errors <- TRUE
            break
          }
        }
        
        if (has_errors) {
          showNotification("Warning: Some rows have validation errors. Excel file created but may not work in Kahoot.", 
                           type = "warning", duration = 5)
        }
        
        # Create workbook
        wb <- createWorkbook()
        addWorksheet(wb, "Sheet1")
        
        # Add title and instructions
        writeData(wb, sheet = 1, x = "Quiz template", startCol = 2, startRow = 2)
        writeData(wb, sheet = 1, 
                  x = "Add questions, at least two answer alternatives, time limit and choose correct answers (at least one). Have fun creating your awesome quiz!", 
                  startCol = 2, startRow = 3)
        writeData(wb, sheet = 1, 
                  x = "Remember: questions have a limit of 120 characters and answers can have 75 characters max. Text will turn red in Excel or Google Docs if you exceed this limit. If several answers are correct, separate them with a comma.", 
                  startCol = 2, startRow = 4)
        
        # Add headers in row 8
        headers <- c("", "Question - max 120 characters", "Answer 1 - max 75 characters", 
                     "Answer 2 - max 75 characters", "Answer 3 - max 75 characters", 
                     "Answer 4 - max 75 characters", 
                     "Time limit (sec) – 5, 10, 20, 30, 60, 90, 120, or 240 secs", 
                     "Correct answer(s) - choose at least one")
        writeData(wb, sheet = 1, x = t(headers), startRow = 8, colNames = FALSE)
        
        # Add quiz data starting from row 9
        for (i in 1:nrow(data)) {
          row_data <- c(
            i,  # Renumber for export
            data$Question[i],
            data$Answer_1[i],
            data$Answer_2[i],
            data$Answer_3[i],
            data$Answer_4[i],
            data$Time_Limit[i],
            data$Correct[i]
          )
          writeData(wb, sheet = 1, x = t(row_data), startRow = 8 + i, colNames = FALSE)
        }
        
        # Style the worksheet
        addStyle(wb, sheet = 1, 
                 style = createStyle(textDecoration = "bold", fontSize = 11),
                 rows = 8, cols = 1:8, gridExpand = TRUE)
        
        addStyle(wb, sheet = 1, 
                 style = createStyle(textDecoration = "bold", fontSize = 14),
                 rows = 2, cols = 2)
        
        # Set column widths
        setColWidths(wb, sheet = 1, cols = 1, widths = 12)
        setColWidths(wb, sheet = 1, cols = 2, widths = 50)
        setColWidths(wb, sheet = 1, cols = 3:6, widths = 32)
        setColWidths(wb, sheet = 1, cols = 7, widths = 26)
        setColWidths(wb, sheet = 1, cols = 8, widths = 33)
        
        # Save workbook
        saveWorkbook(wb, file, overwrite = TRUE)
        
        showNotification(paste("Excel file created with", nrow(data), "questions!"), 
                         type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error creating Excel:", e$message), 
                         type = "error", duration = NULL)
      })
    }
  )
}

shinyApp(ui = ui, server = server)