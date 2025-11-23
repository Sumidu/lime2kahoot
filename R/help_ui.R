help_ui <- function(){
  return(tabPanel("Help",
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
  ))
}