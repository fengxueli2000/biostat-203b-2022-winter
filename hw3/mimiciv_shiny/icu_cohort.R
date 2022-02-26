library(shiny)
library(ggplot2)
library(tidyverse)
counties <- readRDS("icu_cohort.rds")
ui <- fluidPage(
  titlePanel("ICU Cohort Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create histgram and summary for lab variables 
                "),
      
      selectInput("var1", 
                  label = "Patient whether die within 30 days 
                  of hospital admission",
                  choices = c("Yes","No"),
                  selected = "Yes"),
      
      selectInput("var2", 
                  label = "vital variables",
                  choices = c("heart_rate",  
                              "systolic_blood_pressure",
                              "mean_blood_pressure",
                              "respiratory_rate",
                              "body_temperature"),
                  selected = "heart_rate"),),
    mainPanel(plotOutput("histgram_vital"),
              verbatimTextOutput("summary_vital")
    )),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create histgram and summary for lab variables 
                "),
      
      selectInput("var3", 
                  label = "Patient whether die within 30 days 
                  of hospital admission",
                  choices = c("Yes","No"),
                  selected = "Yes"),
      
      selectInput("var4", 
                  label = "Lab variables",
                  choices = c(
                    "bicarbonate", "calcium",
                    "chloride", "creatinine", "glucose",
                    "magnesium", "potassium", "sodium",
                    "hematocrit", "white_blood_cell"),
                  selected = "heart_rate"),),
    mainPanel(plotOutput("histgram_lab"),
              verbatimTextOutput("summary_lab")
    )),
  
  sidebarLayout(
    sidebarPanel( 
      helpText("Barchart for demographics variable
                "),
      selectInput("var5", 
                  label = "Patient whether die within 30 days 
                  of hospital admission",
                  choices = c("Yes","No"),
                  selected = "Yes"),
      selectInput("var6", 
                  label = "demographics variable",
                  choices = c("language","ethnicity",
                              "insurance","marital status",
                              "gender"),
                  selected = "language"),
    ),
    mainPanel(plotOutput("barchart")
    )
  )
)


server <- function(input, output) {
  
  output$histgram_vital <- renderPlot({
    data2 <- switch(input$var1,
                    "Yes" = counties[counties$thirty_day_mort == "Yes", ],
                    "No" = counties[counties$thirty_day_mort == "No", ]
    )
    data <- switch(input$var2, 
                   "heart_rate" = data2$vital220045,
                   "systolic_blood_pressure" = data2$vital220179,
                   "mean_blood_pressure" = data2$vital220181,
                   "respiratory_rate" = data2$vital220210,
                   "body_temperature" = data2$vital223761)
    ggplot(mapping = aes(x = data)) +
      geom_histogram(outlier.shape = NA) 
  })
  
  output$summary_vital <- renderPrint({
    data2 <- switch(input$var1,
                    "Yes" = counties[counties$thirty_day_mort == "Yes", ],
                    "No" = counties[counties$thirty_day_mort == "No", ]
    )
    data <- switch(input$var2, 
                   "heart_rate" = data2$vital220045,
                   "systolic_blood_pressure" = data2$vital220179,
                   "mean_blood_pressure" = data2$vital220181,
                   "respiratory_rate" = data2$vital220210,
                   "body_temperature" = data2$vital223761)
    summary(data)
  })
  
  output$histgram_lab <- renderPlot({
    data2 <- switch(input$var3,
                    "Yes" = counties[counties$thirty_day_mort == "Yes", ],
                    "No" = counties[counties$thirty_day_mort == "No", ]
    )
    data <- switch(input$var4, 
                   "bicarbonate" = data2$lab50882,
                   "calcium" = data2$lab50893,
                   "chloride" = data2$lab50902,
                   "creatinine" = data2$lab50912,
                   "glucose" = data2$lab50931,
                   "magnesium" = data2$lab50960,
                   "potassium" = data2$lab50971,
                   "sodium" = data2$lab50983,
                   "hematocrit" = data2$lab51221,
                   "white_blood_cell" = data2$lab51301)
    ggplot(mapping = aes(x = data)) +
      geom_histogram(outlier.shape = NA) 
  })
  
  output$summary_lab <- renderPrint({
    data2 <- switch(input$var3,
                    "Yes" = counties[counties$thirty_day_mort == "Yes", ],
                    "No" = counties[counties$thirty_day_mort == "No", ]
    )
    data <- switch(input$var4, 
                   "bicarbonate" = data2$lab50882,
                   "calcium" = data2$lab50893,
                   "chloride" = data2$lab50902,
                   "creatinine" = data2$lab50912,
                   "glucose" = data2$lab50931,
                   "magnesium" = data2$lab50960,
                   "potassium" = data2$lab50971,
                   "sodium" = data2$lab50983,
                   "hematocrit" = data2$lab51221,
                   "white_blood_cell" = data2$lab51301)
    summary(data)
  })
  
  output$barchart <- renderPlot({
    data2 <- switch(input$var5,
                    "Yes" = counties[counties$thirty_day_mort == "Yes", ],
                    "No" = counties[counties$thirty_day_mort == "No", ])
    
    data1 <- switch(input$var6, 
                    "language" = data2$language,
                    "ethnicity" = data2$ethnicity,
                    "insurance" = data2$insurance,
                    "marital status" = data2$marital_status,
                    "gender" = data2$gender)
    count <- table(data1)
    barplot(count)
  })
}  


# Run the application 
shinyApp(ui = ui, server = server)