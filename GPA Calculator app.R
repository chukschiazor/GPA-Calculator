# It is not recommended to install packages every time you run the script.
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("auth0")
# install.packages("usethis")

library(shiny)
library(ggplot2)
library(auth0)
usethis::edit_r_environ()

# Load the Auth0 configuration
auth0_config <- auth0::auth0_config()

# Define the UI using Auth0's secure_ui function

ui <- fluidPage(
  titlePanel("GPA Calculator - Secure with Auth0"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Enter the number of courses
      numericInput("courses", "Number of Courses:", 5, min = 1),
      # Dynamic generation of inputs for course details
      uiOutput("courseInputs")
    ),
    mainPanel(
      # Output: Display the calculated GPA
      textOutput("gpa"),
      # Output: Visualization of course ranking
      plotOutput("gradeVisualization")
    )
  )
)

# Define the server logic using Auth0's secure_server function
server <- function(input, output, session) {
  
  # UK Grade to GPA point mapping
  gradePoints <- c("A+" = 4.5, "A" = 4.0, "A-" = 3.7,
                   "B+" = 3.5, "B" = 3.0, "B-" = 2.7,
                   "C+" = 2.5, "C" = 2.0, "C-" = 1.7,
                   "D+" = 1.5, "D" = 1.0, "D-" = 0.7,
                   "F" = 0)
  
  # Function to calculate the GPA
  calculateGPA <- function(grades, credits) {
    weightedPoints <- grades * credits
    GPA <- sum(weightedPoints) / sum(credits)
    return(GPA)
  }
  
  # Generate the course input fields based on the number of courses
  output$courseInputs <- renderUI({
    numCourses <- input$courses
    lapply(1:numCourses, function(i) {
      fluidRow(
        column(4, textInput(paste0("course", i), label = sprintf("Course %d Name:", i), value = sprintf("Course %d", i))),
        column(4, selectInput(paste0("grade", i), label = "Grade:",
                              choices = names(gradePoints))),
        column(4, numericInput(paste0("credits", i), label = "Credits:", value = 1, min = 0))
      )
    })
  })
  
  # Calculate and display the GPA
  output$gpa <- renderText({
    req(input$courses) # Require that the number of courses is inputted
    grades <- sapply(1:input$courses, function(i) gradePoints[input[[paste0("grade", i)]]])
    credits <- sapply(1:input$courses, function(i) input[[paste0("credits", i)]])
    GPA <- calculateGPA(grades, credits)
    sprintf("Your GPA is: %.2f", GPA)
  })
  
  # Render the bar chart for course performance
  output$gradeVisualization <- renderPlot({
    # Ensure input from the dynamically generated course and grade inputs
    req(input$courses)
    # Create a data frame to hold the course names, grades, and numerical values
    data <- data.frame(
      Course = sapply(1:input$courses, function(i) input[[paste0("course", i)]]),
      Grade = sapply(1:input$courses, function(i) input[[paste0("grade", i)]]),
      stringsAsFactors = FALSE
    )
    data$Points <- gradePoints[data$Grade]
    
    # Sort the data frame by the numerical values of grades
    data <- data[order(-data$Points),]
    
    # Plot
    ggplot(data, aes(x = reorder(Course, -Points), y = Points, fill = Grade)) +
      geom_bar(stat = "identity") +
      labs(x = "Course", y = "Grade Points", title = "Course Performance Ranking") +
      theme_minimal() +
      coord_flip() # Flip the coordinates to make it a horizontal bar chart
  })
  
}

# Set options and run the application with Auth0 security
options(shiny.port = 8081)

# Instead of the usual shinyApp() function, use shinyAppAuth0()
auth0App <- auth0::shinyAppAuth0(ui, server)

shiny::runApp(auth0App)
