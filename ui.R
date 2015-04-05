library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("One-Way ANOVA Calculator"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  fluidRow(
    column(3,inputPanel(
      radioButtons("atype", "Test type:",
                   c("Independent Measures" = "indep",
                     "Repeated Measures" = "repm")),
      br(),
      sliderInput("alpha", 
                  "alpha:", 
                  value = 0.05,
                  min = 0.0001, 
                  max = 0.5),
      br(),
      
      textInput("text1", label = h4("Data"), value = "1 2 3"),
      textInput("text2", label = "", value = "4 5 5"),
      textInput("text3", label = "", value = " "),
      textInput("text4", label = "", value = " "),
      textInput("text5", label = "", value = " "),
      textInput("text6", label = "", value = " "),
      textInput("text7", label = "", value = " "),
      textInput("text8", label = "", value = " "),
      actionButton("clr", label = "Clear")
    )),
    
    column(3,wellPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )),
    
    column(6,wellPanel(
      plotOutput("Fplot"),
      tableOutput("summary_mat"),
      tableOutput("table")
      
    ))
  )
))