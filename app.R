library(shiny)
library(DT)
library(openxlsx)
library(dplyr)
library(lubridate)

# UI START ----
ui <- fluidPage(
  titlePanel("LimeSurvey to Kahoot Quiz Converter"),
  
  tags$head(
    tags$style(HTML("
      .validation-error { background-color: #ffcccc !important; }
      .validation-warning { background-color: #fff3cd !important; }
      .validation-ok { background-color: #d4edda !important; }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        console.log('Document ready, setting up checkbox handlers');
      });
      
      $(document).on('change', 'input.row-select[type=\"checkbox\"]', function(e) {
        console.log('Checkbox changed!');
        var checkbox = $(this);
        var isChecked = checkbox.prop('checked');
        var row = checkbox.closest('tr');
        var table = checkbox.closest('table').DataTable();
        var rowIndex = table.row(row).index();
        
        var allParentIds = [];
        checkbox.parents().each(function() {
          var id = $(this).attr('id');
          if (id) {
            allParentIds.push(id);
          }
        });
        console.log('All parent IDs:', allParentIds);
        
        var containerId = null;
        checkbox.parents().each(function() {
          var id = $(this).attr('id');
          if (id === 'overview_table' || id === 'data_table') {
            containerId = id;
            return false;
          }
        });
        
        console.log('Container ID:', containerId);
        console.log('Row Index:', rowIndex);
        console.log('Is Checked:', isChecked);
        
        if(containerId === 'overview_table') {
          console.log('Sending to overview_checkbox_change');
          Shiny.setInputValue('overview_checkbox_change', {
            row: rowIndex,
            checked: isChecked,
            timestamp: new Date().getTime()
          }, {priority: 'event'});
        } else if(containerId === 'data_table') {
          console.log('Sending to quiz_checkbox_change');
          Shiny.setInputValue('quiz_checkbox_change', {
            row: rowIndex,
            checked: isChecked,
            timestamp: new Date().getTime()
          }, {priority: 'event'});
        } else {
          console.log('Container ID not recognized:', containerId);
        }
      });
    "))
  ),
  
  sidebarLayout(
    ## Sidebar Panel ----
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
        selectizeInput("selected_groups", "Select Groups to Include:",
                       choices = NULL,
                       multiple = TRUE),
        actionButton("apply_filter", "Apply Filter", 
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
      helpText("5. Set point values"),
      helpText("6. Select rows to export"),
      helpText("7. Download Kahoot Excel"),
      helpText("8. Upload participants & grade")
    ),
    
    mainPanel(
      width = 9,
      ### Data Overview Panel ----
      tabsetPanel(
        tabPanel("Data Overview",
                 br(),
                 uiOutput("status_message"),
                 br(),
                 DTOutput("overview_table")
        ),
        ### Quiz Questions Panel ----
        tabPanel("Quiz Questions",
                 br(),
                 uiOutput("validation_summary"),
                 br(),
                 DTOutput("data_table")
        ),
        ### Grading Panel ----
        tabPanel("Grading",
                 br(),
                 fluidRow(
                   column(4,
                          fileInput("participants_file", "Upload Participants CSV",
                                    accept = c(".csv")),
                          helpText("CSV should contain: Matrikelnummer, Gruppenname")
                   ),
                   column(8,
                          uiOutput("grading_summary")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          downloadButton("download_grades", "Download Grades CSV", 
                                         class = "btn-success"),
                          hr(),
                          DTOutput("grading_table")
                   )
                 )
        ),
        ### Help Panel ----
        help_ui()
      )
    )
  )
)
# UI END ----

server <- function(input, output, session) {
  
  # Reactive values ----
  raw_data <- reactiveVal(NULL)
  quiz_data <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  participants_data <- reactiveVal(NULL)
  
  # Indicate if data is loaded  
  output$data_loaded <- reactive({
    # we achieve this by checking if raw_data is not NULL
    !is.null(raw_data())
  })
  
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Process uploaded CSV file ----
  observeEvent(input$csv_file, {
    req(input$csv_file)
    
    # we read the CSV file
    tryCatch({
      
      # old approach

      #df <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE, 
      #               fileEncoding = "UTF-8", check.names = TRUE)
      
      # new approach
      clean_data <- process_quiz_csv(input$csv_file$datapath)
      
      clean_data$Nr <- seq_len(nrow(clean_data))
      
      raw_data(clean_data)
      
      updateDateInput(session, "start_date", value = Sys.Date() - 7)
      updateDateInput(session, "end_date", value = Sys.Date())
      
      updateSelectizeInput(session, "selected_groups", 
                           choices = unique(clean_data$GroupShortName), 
                           selected = unique(clean_data$GroupShortName)
                           )
      
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
  
  # Process participants file ----
  observeEvent(input$participants_file, {
    req(input$participants_file)
    
    tryCatch({
      clean_participants <- process_participants_csv(input$participants_file$datapath)

      cat("Pivoted data: ", nrow(clean_participants), "participants from", 
          nrow(participants), "groups\n")
      cat("Sample:\n")
      print(head(clean_participants, 5))
      
      participants_data(clean_participants)
      
      showNotification(paste("Loaded", nrow(clean_participants), "participants from", 
                             nrow(participants), "groups"), 
                       type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error reading participants file:", e$message), 
                       type = "error", duration = NULL)
    })
  })
  
  # Calculate scores for participants ----
  participant_scores <- reactive({
    req(participants_data(), quiz_data())
    
    participants <- participants_data()
    quiz <- quiz_data()
    
    selected_quiz <- quiz[quiz$Selected == TRUE, ]
    # Handle case with no selected questions
    if (nrow(selected_quiz) == 0) {
      return(data.frame(
        Matrikelnummer = participants$Matrikelnummer,
        Gruppenname = participants$Gruppenname,
        Score = 0,
        Questions_Answered = 0
      ))
    }
    
    # Calculate scores ----
    scores <- lapply(1:nrow(participants), function(i) {
      # get group name (is already short version)
      participant_group <- participants$Gruppenname[i]
      cat("Processing participant", participants$Matrikelnummer[i], 
          "in group", participant_group, "\n")
      
      # find all questions that are selected and belong to this group
      
      group_questions <- selected_quiz[selected_quiz$GroupShortName == participant_group, ]
      #cat("----")
      #print(group_questions)
      
      # add the points marked down
      total_score <- sum(as.numeric(group_questions$Points), na.rm = TRUE)
      # count the rows
      num_questions <- nrow(group_questions)
      
      data.frame(
        Matrikelnummer = participants$Matrikelnummer[i],
        Gruppenname = participants$Gruppenname[i],
        Score = total_score,
        Questions_Answered = num_questions
      )
    })
    
    do.call(rbind, scores)
  })
  
  # Apply date filter ----
  observeEvent(input$apply_filter, {
    req(raw_data())
    
    data <- raw_data()
    filtered <- data[data$SubmitDate >= input$start_date & 
                       data$SubmitDate <= input$end_date &
                       data$GroupShortName %in% input$selected_groups, ]
    
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
  
  # Validation function ----
  validate_row <- function(row) {
    issues <- list()
    
    q_len <- nchar(as.character(row$Question))
    if (is.na(q_len)) q_len <- 0
    if (q_len > 120) {
      issues$question <- "error"
    } else if (q_len > 110) {
      issues$question <- "warning"
    } else {
      issues$question <- "ok"
    }
    
    for (i in 1:4) {
      ans_len <- nchar(as.character(row[[paste0("Answer_", i)]]))
      if (is.na(ans_len)) ans_len <- 0
      if (ans_len > 75) {
        issues[[paste0("answer_", i)]] <- "error"
      } else if (ans_len > 70) {
        issues[[paste0("answer_", i)]] <- "warning"
      } else {
        issues[[paste0("answer_", i)]] <- "ok"
      }
    }
    
    valid_times <- c(5, 10, 20, 30, 60, 90, 120, 240)
    time_val <- as.numeric(row$Time_Limit)
    if (is.na(time_val) || !(time_val %in% valid_times)) {
      issues$time <- "error"
    } else {
      issues$time <- "ok"
    }
    
    correct_val <- as.character(row$Correct)
    if (is.na(correct_val) || correct_val == "") {
      issues$correct <- "error"
    } else {
      correct_vals <- strsplit(correct_val, ",")[[1]]
      correct_vals <- trimws(correct_vals)
      if (all(correct_vals %in% c("1", "2", "3", "4"))) {
        issues$correct <- "ok"
      } else {
        issues$correct <- "error"
      }
    }
    
    return(issues)
  }
  
  # Status message ----
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
  
  # Validation summary ----
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
          sprintf(" %d row(s) have validation errors.", error_count))
    } else if (warning_count > 0) {
      div(class = "alert alert-warning",
          icon("exclamation-circle"),
          sprintf(" %d row(s) have warnings.", warning_count))
    } else {
      div(class = "alert alert-success",
          icon("check-circle"),
          " All entries are valid!")
    }
  })
  
  # Grading summary ----
  output$grading_summary <- renderUI({
    if (is.null(participants_data())) {
      div(class = "alert alert-info",
          icon("info-circle"),
          " Upload a participants CSV file to calculate grades.")
    } else if (is.null(quiz_data())) {
      div(class = "alert alert-warning",
          icon("exclamation-circle"),
          " Please upload quiz data first.")
    } else {
      scores <- participant_scores()
      avg_score <- mean(scores$Score, na.rm = TRUE)
      total_participants <- nrow(scores)
      
      div(class = "alert alert-success",
          icon("check-circle"),
          sprintf(" %d participants loaded. Average score: %.2f points.", 
                  total_participants, avg_score))
    }
  })
  
  # Overview table ----
  output$overview_table <- renderDT({
    req(filtered_data())
    
    data <- filtered_data()
    
    if (!all(c("Selected", "Nr", "SubmitDate", "GroupName", "GroupShortName", "Password", "Question") %in% colnames(data))) {
      return(NULL)
    }
    
    overview <- data[, c("Selected", "Nr", "SubmitDate", "GroupName", "GroupShortName", "Password", "Question")]
    
    datatable(
      overview,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:2),
          list(width = '40px', targets = 0),
          list(width = '40px', targets = 1),
          list(width = '100px', targets = 2),
          list(width = '150px', targets = 3:4),
          list(width = '300px', targets = 5),
          list(
            targets = 0,
            render = JS(
              "function(data, type, row, meta) {",
              "  if(type === 'display'){",
              "    return '<input type=\"checkbox\" class=\"row-select\" ' + (data ? 'checked' : '') + '>';",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      rownames = FALSE,
      selection = 'none',
      class = 'cell-border stripe',
      escape = FALSE
    )
  })
  
  # Quiz data table ----
  output$data_table <- renderDT({
    req(quiz_data())
    
    data <- quiz_data()
    display_data <- data[, c("Selected", "Nr", "Question", "Answer_1", "Answer_2", 
                             "Answer_3", "Answer_4", "Time_Limit", "Correct", "Points")]
    
    dt <- datatable(
      display_data,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(width = '40px', targets = 0),
          list(width = '40px', targets = 1),
          list(width = '300px', targets = 2),
          list(width = '150px', targets = 3:6),
          list(width = '80px', targets = 7:9),
          list(className = 'dt-center', targets = c(0, 1, 7, 8, 9)),
          list(
            targets = 0,
            render = JS(
              "function(data, type, row, meta) {",
              "  if(type === 'display'){",
              "    return '<input type=\"checkbox\" class=\"row-select\" ' + (data ? 'checked' : '') + '>';",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      rownames = FALSE,
      selection = 'none',
      class = 'cell-border stripe',
      escape = FALSE
    )
    
    for (i in 1:nrow(data)) {
      issues <- validate_row(data[i, ])
      
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
  
  # Grading table ----
  output$grading_table <- renderDT({
    req(participant_scores())
    
    scores <- participant_scores()
    
    datatable(
      scores,
      options = list(
        pageLength = 55,
        scrollX = TRUE,
        order = list(list(2, 'desc'))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound('Score', 2)
  })
  
  # Handle checkbox changes
  observeEvent(input$overview_checkbox_change, {
    req(filtered_data())
    info <- input$overview_checkbox_change
    data <- filtered_data()
    
    cat("Overview checkbox changed - Row:", info$row + 1, "Checked:", info$checked, "\n")
    
    data$Selected[info$row + 1] <- info$checked
    filtered_data(data)
    quiz_data(data)
    
    cat("Selected count:", sum(data$Selected), "\n")
  })
  
  observeEvent(input$quiz_checkbox_change, {
    req(quiz_data())
    info <- input$quiz_checkbox_change
    data <- quiz_data()
    
    cat("Quiz checkbox changed - Row:", info$row + 1, "Checked:", info$checked, "\n")
    
    data$Selected[info$row + 1] <- info$checked
    quiz_data(data)
    filtered_data(data)
    
    cat("Selected count:", sum(data$Selected), "\n")
  })
  
  # Handle cell edits ----
  observeEvent(input$overview_table_cell_edit, {
    info <- input$overview_table_cell_edit
    data <- filtered_data()
    
    col_name <- colnames(data)[info$col + 1]
    
    if (col_name == "Selected") return()
    
    data[info$row, col_name] <- info$value
    
    filtered_data(data)
    quiz_data(data)
  })
  
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    data <- quiz_data()
    
    display_cols <- c("Selected", "Nr", "Question", "Answer_1", "Answer_2", 
                      "Answer_3", "Answer_4", "Time_Limit", "Correct", "Points")
    col_name <- display_cols[info$col + 1]
    
    if (col_name == "Selected") return()
    
    if (col_name == "Points") {
      val <- as.numeric(info$value)
      if (is.na(val) || !val %in% c(0, 0.5, 1)) {
        showNotification("Points must be 0, 0.5, or 1", type = "warning", duration = 2)
        return()
      }
      data[info$row, col_name] <- val
    } else {
      data[info$row, col_name] <- info$value
    }
    
    quiz_data(data)
    filtered_data(data)
  })
  
  # Download handlers ----
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("kahoot_quiz_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(quiz_data())
      
      tryCatch({
        data <- quiz_data()
        
        cat("Total rows:", nrow(data), "\n")
        cat("Selected rows:", sum(data$Selected), "\n")
        cat("Export selected only:", input$export_selected_only, "\n")
        
        if (input$export_selected_only) {
          data <- data[data$Selected == TRUE, ]
          cat("After filtering:", nrow(data), "\n")
        }
        
        if (nrow(data) == 0) {
          showNotification("No rows selected for export", type = "error")
          return()
        }
        
        has_errors <- FALSE
        for (i in 1:nrow(data)) {
          issues <- validate_row(data[i, ])
          if (any(unlist(issues) == "error")) {
            has_errors <- TRUE
            break
          }
        }
        
        if (has_errors) {
          showNotification("Warning: Some rows have validation errors.", 
                           type = "warning", duration = 5)
        }
        
        wb <- createWorkbook()
        addWorksheet(wb, "Sheet1")
        
        writeData(wb, sheet = 1, x = "Quiz template", startCol = 2, startRow = 2)
        writeData(wb, sheet = 1, 
                  x = "Add questions, at least two answer alternatives, time limit and choose correct answers (at least one). Have fun creating your awesome quiz!", 
                  startCol = 2, startRow = 3)
        writeData(wb, sheet = 1, 
                  x = "Remember: questions have a limit of 120 characters and answers can have 75 characters max. Text will turn red in Excel or Google Docs if you exceed this limit. If several answers are correct, separate them with a comma.", 
                  startCol = 2, startRow = 4)
        
        headers <- c("", "Question - max 120 characters", "Answer 1 - max 75 characters", 
                     "Answer 2 - max 75 characters", "Answer 3 - max 75 characters", 
                     "Answer 4 - max 75 characters", 
                     "Time limit (sec) – 5, 10, 20, 30, 60, 90, 120, or 240 secs", 
                     "Correct answer(s) - choose at least one")
        writeData(wb, sheet = 1, x = t(headers), startRow = 8, colNames = FALSE)
        
        for (i in 1:nrow(data)) {
          row_data <- c(
            i,
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
        
        addStyle(wb, sheet = 1, 
                 style = createStyle(textDecoration = "bold", fontSize = 11),
                 rows = 8, cols = 1:8, gridExpand = TRUE)
        
        addStyle(wb, sheet = 1, 
                 style = createStyle(textDecoration = "bold", fontSize = 14),
                 rows = 2, cols = 2)
        
        setColWidths(wb, sheet = 1, cols = 1, widths = 12)
        setColWidths(wb, sheet = 1, cols = 2, widths = 50)
        setColWidths(wb, sheet = 1, cols = 3:6, widths = 32)
        setColWidths(wb, sheet = 1, cols = 7, widths = 26)
        setColWidths(wb, sheet = 1, cols = 8, widths = 33)
        
        saveWorkbook(wb, file, overwrite = TRUE)
        
        showNotification(paste("Excel file created with", nrow(data), "questions!"), 
                         type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error creating Excel:", e$message), 
                         type = "error", duration = NULL)
      })
    }
  )
  
  # Grades download ----
  output$download_grades <- downloadHandler(
    filename = function() {
      paste0("grades_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(participant_scores())
      
      scores <- participant_scores()
      
      export_data <- data.frame(
        Matrikelnummer = scores$Matrikelnummer,
        Score = scores$Score
      )
      
      write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      
      showNotification("Grades CSV downloaded!", type = "message", duration = 2)
    }
  )
}

shinyApp(ui = ui, server = server)