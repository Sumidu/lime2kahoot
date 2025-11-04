library(shiny)
library(DT)
library(openxlsx)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(logging)

# Initialize logging
basicConfig(level = "INFO")

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
      helpText("5. Set point values"),
      helpText("6. Select rows to export"),
      helpText("7. Download Kahoot Excel"),
      helpText("8. Upload participants & grade")
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
        tabPanel("Help",
                 br(),
                 h4("About this app"),
                 p("This app converts quiz questions from LimeSurvey CSV format to Kahoot Excel template format, and calculates participant grades."),
                 h4("Features:"),
                 tags$ul(
                   tags$li(strong("Data Overview tab:"), " Review submission dates, group names, and passwords."),
                   tags$li(strong("Quiz Questions tab:"), " Edit questions and answers. Assign point values (0, 0.5, or 1)."),
                   tags$li(strong("Grading tab:"), " Upload participant information and calculate scores based on group performance."),
                   tags$li(strong("Date Filtering:"), " Focus on submissions from a specific date range."),
                   tags$li(strong("Row Selection:"), " Choose which questions to include in export and grading.")
                 ),
                 h4("Grading Workflow:"),
                 tags$ol(
                   tags$li("Upload LimeSurvey CSV with quiz questions"),
                   tags$li("Filter by date and review submissions"),
                   tags$li("Select questions to grade (checkboxes)"),
                   tags$li("Assign point values in Points column"),
                   tags$li("Go to Grading tab and upload participants CSV"),
                   tags$li("Review calculated scores"),
                   tags$li("Download grades CSV")
                 ),
                 h4("Participants CSV Format:"),
                 p("The participants CSV should contain at least two columns:"),
                 tags$ul(
                   tags$li(strong("Matrikelnummer:"), " Student ID"),
                   tags$li(strong("Gruppenname:"), " Group name (must match group names in quiz data)")
                 ),
                 h4("Kahoot Requirements:"),
                 tags$ul(
                   tags$li("Questions: max 120 characters"),
                   tags$li("Answers: max 75 characters each"),
                   tags$li("Time limit: 5, 10, 20, 30, 60, 90, 120, or 240 seconds"),
                   tags$li("Correct answer: 1, 2, 3, or 4 (or combinations like '1,2')"),
                   tags$li("Points: 0, 0.5, or 1 (for grading only)")
                 ),
                 h4("Color Coding:"),
                 tags$ul(
                   tags$li(tags$span(style = "background-color: #d4edda; padding: 2px 5px;", "Green"), " - Valid"),
                   tags$li(tags$span(style = "background-color: #fff3cd; padding: 2px 5px;", "Yellow"), " - Warning"),
                   tags$li(tags$span(style = "background-color: #ffcccc; padding: 2px 5px;", "Red"), " - Error")
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
  participants_data <- reactiveVal(NULL)
  
  output$data_loaded <- reactive({
    !is.null(raw_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Process uploaded CSV
  observeEvent(input$csv_file, {
    req(input$csv_file)

    loginfo("Processing uploaded CSV file: %s", input$csv_file$name)

    tryCatch({
      df <- read_csv(input$csv_file$datapath,
                     locale = locale(encoding = "UTF-8"),
                     show_col_types = FALSE)
      loginfo("Successfully loaded CSV with %d rows and %d columns", nrow(df), ncol(df))

      find_column <- function(patterns) {
        result <- patterns %>%
          map(~ str_subset(colnames(df), regex(.x, ignore_case = TRUE))) %>%
          detect(~ length(.x) > 0)

        if (!is.null(result) && length(result) > 0) {
          loginfo("Found column matching: %s", result[1])
          return(result[1])
        }
        logwarn("Could not find column matching any pattern: %s", str_c(patterns, collapse = ", "))
        return(NA_character_)
      }
      
      date_col <- find_column(c("Datum\\.Abgeschickt", "Date", "Submitted"))
      group_col <- find_column(c("Gruppennamen", "Group"))
      password_col <- find_column(c("Gruppenpasswort", "Password"))
      question_col <- find_column(c("Frage\\.ein", "Question"))
      answer_a_col <- find_column(c("Antwort\\.A", "Answer.*A"))
      answer_b_col <- find_column(c("Antwort\\.B", "Answer.*B"))
      answer_c_col <- find_column(c("Antwort\\.C", "Answer.*C"))
      answer_d_col <- find_column(c("Antwort\\.D", "Answer.*D"))
      correct_col <- find_column(c("korrekte\\.Antwort", "Correct"))
      
      required_cols <- tibble(
        name = c("Question", "Answer A", "Answer B", "Answer C", "Answer D", "Correct Answer"),
        col = list(question_col, answer_a_col, answer_b_col, answer_c_col, answer_d_col, correct_col)
      )

      missing_cols <- required_cols %>%
        filter(map_lgl(col, is.na)) %>%
        pull(name)

      if (length(missing_cols) > 0) {
        error_msg <- str_c("Could not find required columns: ", str_c(missing_cols, collapse = ", "))
        logerror(error_msg)
        stop(error_msg)
      }
      loginfo("All required columns found successfully")
      
      # Parse dates and create clean data with pipes
      loginfo("Starting data transformation and cleaning")
      clean_data <- df %>%
        mutate(
          ParsedDate = if (!is.na(date_col)) {
            tryCatch(
              as.Date(parse_date_time(.data[[date_col]], orders = c("ymd HMS", "dmy HMS", "mdy HMS"))),
              error = function(e) {
                logwarn("Date parsing failed: %s. Using current date.", e$message)
                Sys.Date()
              }
            )
          } else {
            loginfo("No date column found, using current date")
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
        ) %>%
        filter(!is.na(Question), str_trim(Question) != "")
      
      if (nrow(clean_data) == 0) {
        error_msg <- "No valid questions found in the CSV file"
        logerror(error_msg)
        stop(error_msg)
      }

      clean_data <- clean_data %>%
        mutate(
          Correct = Correct %>%
            str_replace_all(c("A" = "1", "B" = "2", "C" = "3", "D" = "4")),
          across(everything(), ~ if_else(is.na(.), "", as.character(.))),
          Selected = TRUE,
          Nr = row_number()
        )

      loginfo("Data cleaning complete. Total questions: %d", nrow(clean_data))

      raw_data(clean_data)

      updateDateInput(session, "start_date", value = Sys.Date() - 7)
      updateDateInput(session, "end_date", value = Sys.Date())

      filtered <- clean_data %>%
        filter(SubmitDate >= (Sys.Date() - 7), SubmitDate <= Sys.Date()) %>%
        mutate(Nr = row_number())

      filtered_data(filtered)
      quiz_data(filtered)

      loginfo("Loaded %d total questions, %d in default date range", nrow(clean_data), nrow(filtered))
      showNotification(paste("Loaded", nrow(clean_data), "total questions,",
                             nrow(filtered), "in date range"),
                       type = "message", duration = 3)
      
    }, error = function(e) {
      logerror("Error processing CSV file: %s", e$message)
      showNotification(paste("Error reading CSV:", e$message),
                       type = "error", duration = NULL)
    })
  })
  
  # Process participants file
  observeEvent(input$participants_file, {
    req(input$participants_file)

    loginfo("Processing uploaded participants file: %s", input$participants_file$name)

    tryCatch({
      participants <- read_csv(input$participants_file$datapath,
                               locale = locale(encoding = "UTF-8"),
                               show_col_types = FALSE)

      loginfo("Successfully loaded participants CSV with %d rows and %d columns", nrow(participants), ncol(participants))
      logdebug("Participants file columns: %s", str_c(colnames(participants), collapse = ", "))

      find_column <- function(patterns) {
        result <- patterns %>%
          map(~ str_subset(colnames(participants), regex(.x, ignore_case = TRUE))) %>%
          detect(~ length(.x) > 0)

        if (!is.null(result) && length(result) > 0) {
          loginfo("Found column matching: %s", result[1])
          return(result[1])
        }
        logwarn("Could not find column matching any pattern: %s", str_c(patterns, collapse = ", "))
        return(NA_character_)
      }
      
      # Find group name column
      group_col <- find_column(c("Group\\.Name", "GroupName", "Gruppenname", 
                                 "gruppe", "Group", "Team"))
      
      if (is.na(group_col)) {
        available_cols <- str_c(colnames(participants), collapse = ", ")
        error_msg <- str_c("Could not find group name column. Looking for patterns like: Group Name, GroupName, Gruppenname.\n\nAvailable columns: ", available_cols)
        logerror(error_msg)
        stop(error_msg)
      }

      loginfo("Using Group Name column: %s", group_col)
      
      # Find all member ID columns (Member 1 ID Number, Member 2 ID Number, etc.)
      member_id_cols <- grep("Member.*ID.*Number|Member.*ID|Student.*ID", 
                             colnames(participants), 
                             ignore.case = TRUE, 
                             value = TRUE)
      
      if (length(member_id_cols) == 0) {
        # Try alternative patterns
        member_id_cols <- grep("Mitglied|Member|Student|Teilnehmer", 
                               colnames(participants), 
                               ignore.case = TRUE, 
                               value = TRUE)
      }
      
      if (length(member_id_cols) == 0) {
        available_cols <- str_c(colnames(participants), collapse = ", ")
        error_msg <- str_c("Could not find member ID columns. Looking for patterns like: Member 1 ID Number, Member 2 ID Number.\n\nAvailable columns: ", available_cols)
        logerror(error_msg)
        stop(error_msg)
      }

      loginfo("Found %d member ID columns: %s", length(member_id_cols), str_c(member_id_cols, collapse = ", "))

      # Pivot longer: create one row per participant using tidyverse
      clean_participants <- participants %>%
        select(all_of(c(group_col, member_id_cols))) %>%
        pivot_longer(
          cols = all_of(member_id_cols),
          names_to = "Member_Column",
          values_to = "Matrikelnummer"
        ) %>%
        rename(Gruppenname = !!sym(group_col)) %>%
        mutate(
          Matrikelnummer = str_trim(as.character(Matrikelnummer)),
          Gruppenname = str_trim(as.character(Gruppenname))
        ) %>%
        filter(
          !is.na(Matrikelnummer), Matrikelnummer != "",
          !is.na(Gruppenname), Gruppenname != ""
        ) %>%
        select(Matrikelnummer, Gruppenname) %>%
        distinct()
      
      loginfo("Successfully pivoted data: %d participants from %d groups", nrow(clean_participants), nrow(participants))
      logdebug("Sample participants data (first 5 rows): %s",
               str_c(capture.output(print(head(clean_participants, 5))), collapse = "\n"))

      participants_data(clean_participants)

      showNotification(paste("Loaded", nrow(clean_participants), "participants from",
                             nrow(participants), "groups"),
                       type = "message", duration = 3)

    }, error = function(e) {
      logerror("Error processing participants file: %s", e$message)
      showNotification(paste("Error reading participants file:", e$message),
                       type = "error", duration = NULL)
    })
  })
  
  # Calculate scores for participants
  participant_scores <- reactive({
    req(participants_data(), quiz_data())
    
    participants <- participants_data()
    quiz <- quiz_data()

    selected_quiz <- quiz %>%
      filter(Selected == TRUE)
    
    if (nrow(selected_quiz) == 0) {
      return(participants %>%
        mutate(Score = 0, Questions_Answered = 0))
    }

    # Calculate scores using tidyverse
    group_scores <- selected_quiz %>%
      group_by(GroupName) %>%
      summarise(
        Score = sum(as.numeric(Points), na.rm = TRUE),
        Questions_Answered = n(),
        .groups = "drop"
      )

    participants %>%
      left_join(group_scores, by = c("Gruppenname" = "GroupName")) %>%
      mutate(
        Score = if_else(is.na(Score), 0, Score),
        Questions_Answered = if_else(is.na(Questions_Answered), 0L, as.integer(Questions_Answered))
      )
  })
  
  # Apply date filter
  observeEvent(input$apply_filter, {
    req(raw_data())

    loginfo("Applying date filter: %s to %s", input$start_date, input$end_date)

    filtered <- raw_data() %>%
      filter(
        SubmitDate >= input$start_date,
        SubmitDate <= input$end_date
      ) %>%
      mutate(Nr = row_number())

    if (nrow(filtered) == 0) {
      logwarn("No data found in selected date range: %s to %s", input$start_date, input$end_date)
      showNotification("No data in selected date range", type = "warning")
      return()
    }

    filtered_data(filtered)
    quiz_data(filtered)

    loginfo("Date filter applied: %d questions in range", nrow(filtered))
    showNotification(paste("Filtered to", nrow(filtered), "questions"),
                     type = "message", duration = 2)
  })
  
  # Validation function
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

    # Validate all rows using functional approach
    validation_results <- data %>%
      split(1:nrow(.)) %>%
      map(validate_row)

    error_count <- validation_results %>%
      map_lgl(~ any(unlist(.) == "error")) %>%
      sum()

    warning_count <- validation_results %>%
      map_lgl(~ any(unlist(.) == "warning")) %>%
      sum()
    
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
  
  # Grading summary
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
  
  # Overview table
  output$overview_table <- renderDT({
    req(filtered_data())

    data <- filtered_data()

    required_cols <- c("Selected", "Nr", "SubmitDate", "GroupName", "Password", "Question")
    if (!all(required_cols %in% colnames(data))) {
      return(NULL)
    }

    overview <- data %>%
      select(Selected, Nr, SubmitDate, GroupName, Password, Question)
    
    datatable(
      overview,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
      options = list(
        pageLength = 15,
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
  
  # Quiz data table
  output$data_table <- renderDT({
    req(quiz_data())

    data <- quiz_data()
    display_data <- data %>%
      select(Selected, Nr, Question, Answer_1, Answer_2,
             Answer_3, Answer_4, Time_Limit, Correct, Points)
    
    dt <- datatable(
      display_data,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(
        pageLength = 15,
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
    
    # Apply row validation styling using functional approach
    validation_styles <- data %>%
      split(1:nrow(.)) %>%
      map(validate_row) %>%
      imap(~ list(row = .y, status = .x$question))

    dt <- validation_styles %>%
      reduce(
        function(dt_obj, item) {
          if (item$status == "error") {
            dt_obj %>% formatStyle(3, target = 'row',
                                   backgroundColor = styleEqual(item$row, '#ffcccc'))
          } else if (item$status == "warning") {
            dt_obj %>% formatStyle(3, target = 'row',
                                   backgroundColor = styleEqual(item$row, '#fff3cd'))
          } else {
            dt_obj
          }
        },
        .init = dt
      )

    dt
  })
  
  # Grading table
  output$grading_table <- renderDT({
    req(participant_scores())
    
    scores <- participant_scores()
    
    datatable(
      scores,
      options = list(
        pageLength = 25,
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

    logdebug("Overview checkbox changed - Row: %d, Checked: %s", info$row + 1, info$checked)

    data$Selected[info$row + 1] <- info$checked
    filtered_data(data)
    quiz_data(data)

    loginfo("Updated selection - %d questions selected", sum(data$Selected))
  })

  observeEvent(input$quiz_checkbox_change, {
    req(quiz_data())
    info <- input$quiz_checkbox_change
    data <- quiz_data()

    logdebug("Quiz checkbox changed - Row: %d, Checked: %s", info$row + 1, info$checked)

    data$Selected[info$row + 1] <- info$checked
    quiz_data(data)
    filtered_data(data)

    loginfo("Updated selection - %d questions selected", sum(data$Selected))
  })
  
  # Handle cell edits
  observeEvent(input$overview_table_cell_edit, {
    info <- input$overview_table_cell_edit
    data <- filtered_data()

    col_name <- colnames(data)[info$col + 1]

    if (col_name == "Selected") return()

    logdebug("Overview table cell edited - Row: %d, Column: %s, New value: %s", info$row, col_name, info$value)

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

    logdebug("Quiz table cell edited - Row: %d, Column: %s, New value: %s", info$row, col_name, info$value)

    if (col_name == "Points") {
      val <- as.numeric(info$value)
      if (is.na(val) || !val %in% c(0, 0.5, 1)) {
        logwarn("Invalid points value attempted: %s (must be 0, 0.5, or 1)", info$value)
        showNotification("Points must be 0, 0.5, or 1", type = "warning", duration = 2)
        return()
      }
      data[info$row, col_name] <- val
      loginfo("Points updated for row %d: %s", info$row, val)
    } else {
      data[info$row, col_name] <- info$value
    }

    quiz_data(data)
    filtered_data(data)
  })
  
  # Download handlers
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("kahoot_quiz_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(quiz_data())
      
      loginfo("Starting Excel export")

      tryCatch({
        data <- quiz_data()

        loginfo("Total rows: %d, Selected rows: %d, Export selected only: %s",
                nrow(data), sum(data$Selected), input$export_selected_only)

        if (input$export_selected_only) {
          data <- data %>% filter(Selected == TRUE)
          loginfo("After filtering for selected only: %d rows", nrow(data))
        }

        if (nrow(data) == 0) {
          logerror("Export failed: No rows selected for export")
          showNotification("No rows selected for export", type = "error")
          return()
        }

        # Check for validation errors using functional approach
        has_errors <- data %>%
          split(1:nrow(.)) %>%
          map(validate_row) %>%
          map(~ any(unlist(.) == "error")) %>%
          any()

        if (has_errors) {
          logwarn("Export contains rows with validation errors")
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

        loginfo("Excel file successfully created with %d questions", nrow(data))
        showNotification(paste("Excel file created with", nrow(data), "questions!"),
                         type = "message", duration = 3)

      }, error = function(e) {
        logerror("Error creating Excel file: %s", e$message)
        showNotification(paste("Error creating Excel:", e$message),
                         type = "error", duration = NULL)
      })
    }
  )
  
  output$download_grades <- downloadHandler(
    filename = function() {
      paste0("grades_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(participant_scores())

      loginfo("Starting grades CSV export")

      tryCatch({
        scores <- participant_scores()

        loginfo("Exporting grades for %d participants", nrow(scores))

        export_data <- data.frame(
          Matrikelnummer = scores$Matrikelnummer,
          Score = scores$Score
        )

        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")

        loginfo("Grades CSV successfully created with %d records", nrow(export_data))
        showNotification("Grades CSV downloaded!", type = "message", duration = 2)

      }, error = function(e) {
        logerror("Error creating grades CSV: %s", e$message)
        showNotification(paste("Error creating grades CSV:", e$message),
                         type = "error", duration = NULL)
      })
    }
  )
}

shinyApp(ui = ui, server = server)