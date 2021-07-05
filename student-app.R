library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(xlsx)
library(DT)

df <- read.xlsx("student-mat.xlsx", 1)

# transforming into factor variables
df$higher <- as.factor(df$higher)
df$activities <- as.factor(df$activities)
df$schoolsup <- as.factor(df$schoolsup)
df$paid <- as.factor(df$paid)
df$sex <- as.factor(df$sex)

ui <- dashboardPage(skin="purple",
                    dashboardHeader(title = "Student Success in Math Class", titleWidth=350),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Dashboard", tabName = "dashboard", icon = icon("door-open")),
                      menuItem("Histogram", tabName = "histogram", icon = icon("chart-bar")),
                      menuItem("Regression", tabName = "regression", icon = icon("chart-line")),
                      menuItem("Data Table", tabName = "datatable", icon = icon("table"))
                    )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                h1("Overview of this Dashboard"),
                                br(),
                                p(strong(tags$u("What is this dashboard all about?"))),
                                p("Here in this dashboard I will be evaluating student progress on math exams given a variety of attributes. To look at the code that went into this Shiny app
                                  and the dataset used, please refer to my GitHub repository:",tags$a("https://github.com/lstaugler/Shiny_Student_App")),
                                p("The problem defined here is to see how different attributes will impact the scores of different aged students in their math classes.
                                This is a problem of interest for me since I am pursuing an undergraduate degree in mathematics and have always had an interest in teaching.
                                This information could help a future teacher learn how to better meet the needs of their students to promote success.
                                Going through the data science life cycle, the given data did not require any extensive cleaning which was a helpful
                                  start. One thing I did do was change some of the variables into factor variables for better analysis.
                                  Next I chose to do a regression problem since a lot of the data was numeric and the target attribute, G3, was numeric.
                                  I then built the regression formula as seen in the regression tab. This allows the user to build their ideal
                                  linear regression model given different attributes to see how well we can fit the data. From this model, we can look at the
                                  summary output which allows the user to look at the different intercepts and P-values for each attribute. For example,
                                  if G1 and G2 are chosen as the x-values, we see that both have a P-value of 0.05 meaning they are both statistically significant
                                  for this model."),  
                                br(),
                                p(strong(tags$u("Where is the data from?"))),
                                p("Data was used from the UCI Machine Learning Repository. The attributes can be described as follows:"),
                                p(span("school", style="color:red"), "- student's school",
span("sex", style="color:red"),"- student's sex",
span("age",style="color:red")," - student's age",
span("address",style="color:red")," - student's home address type",
span("famsize",style="color:red")," - family size",
span("Pstatus",style="color:red")," - parent's cohabitation status",
span("Medu",style="color:red")," - mother's education",
span("Fedu",style="color:red")," - father's education",
span("Mjob",style="color:red"), "- mother's job",
span("Fjob",style="color:red"), "- father's job",
span("reason",style="color:red"), "- reason to choose this school",
span("guardian",style="color:red"), "- student's guardian",
span("traveltime",style="color:red"), "- home to school travel time",
span("studytime",style="color:red"), "- weekly study time",
span("failures",style="color:red"), "- number of past class failures",
span("schoolsup",style="color:red"), "- extra educational support",
span("famsup",style="color:red"), "- family educational support",
span("paid",style="color:red"), "- extra paid classes within the course subject",
span("activities",style="color:red"), "- extra-curricular activities",
span("nursery",style="color:red"), "- attended nursery school",
span("higher",style="color:red"), "- wants to take higher education",
span("internet",style="color:red"), "- Internet access at home",
span("romantic",style="color:red"), "- with a romantic relationship",
span("famrel",style="color:red"), "- quality of family relationships",
span("freetime",style="color:red"), "- free time after school",
span("goout",style="color:red"), "- going out with friends",
span("Dalc",style="color:red"), "- workday alcohol consumption",
span("Walc",style="color:red"), "- weekend alcohol consumption",
span("health",style="color:red"), "- current health status",
span("absences",style="color:red"), "- number of school absences",

# these grades are related with the course subject, Math or Portuguese:
span("G1",style="color:red"), "- first period grade",
span("G2",style="color:red"), "- second period grade",
span("G3",style="color:red"), "- final grade"),
                                br(),
                                p(strong(tags$u("How can I use this dashboard?"))),
                                p("The histogram contains a histogram of the students' final scores on their math exams.
                                       The regression tab goes through linear regression for all of the attributes. You have the
                                  choice to run simple linear regression or multiple linear regression. Regression was the choice
                                  here since the target attribute G3 is numeric. I wanted to see how well we would be able to fit
                                  the G3 value given different attributes.")
                        ),

                        tabItem(tabName = "histogram",
                                fluidRow(
                                  h1("Histogram of Final Scores"),
                                  box(plotOutput(outputId = "distPlot")),
                                  
                                  box(
                                    title = "# of Students broken into bins",
                                    sliderInput(inputId = "bins",
                                                label = "Number of bins:",
                                                min = 2,
                                                max = 10,
                                                value = 4)
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "regression",
                                fluidRow(
                                  h1("Linear Regression"),
                                  sidebarPanel(
                                    uiOutput("xvariable"),
                                    uiOutput("yvariable")
                                  ),
                                  mainPanel(
                                    fluidRow(column(6, verbatimTextOutput('LMSum')) , column(6, plotOutput('diagPlot')))
                                  )
                                )
                        ),

                        tabItem(
                          tabName = "datatable",
                          fluidRow(
                            h1("Student Table"),
                            DT::dataTableOutput("studentTable")
                          )
                        )
                      )
                    )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    x    <- df$G3
    bits <- seq(min(x), max(x),length.out = input$bins + 1)
    
    hist(x, breaks = bits, col = "pink", border = "black",
         xlab = "Number of Students per Score",
         main = "Final Scores of Students")
  })
  output$tb1 <- renderDT(df)
  
  output$xvariable <- renderUI({
    req(df)
    xcol<-colnames(df)
    pickerInput(inputId = 'xvar',
                label = 'Choose x-axis variable(s)',
                choices = c(xcol[1:length(xcol)]), selected=xcol[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(df)
    ycol<-colnames(df) 
    pickerInput(inputId = 'yvar',
                label = 'Target variable',
                choices = c(ycol[33]),
                options = list(`style` = "btn-info"),
                multiple = FALSE)
  })
  
  MLR <- reactive({
    req(df,input$xvar,input$yvar)
    x <- as.numeric(df[[as.name(input$xvar)]])
    y <- as.numeric(df[[as.name(input$yvar)]])
    current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = df, na.action=na.exclude)
    return(model)
  })
  
  output$LMSum <- renderPrint({
    req(MLR())
    summary(MLR())
  })
  
  output$diagPlot <- renderPlot({
    req(MLR())
    par(mfrow = c(2,2))
    plot(MLR())
  })
  output$studentTable <- DT::renderDataTable({df})
}

shinyApp(ui, server)