library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)

data <- read.csv('BodyFat_cleaned.csv', sep=",", dec=".", header = TRUE)
# put lasso fit here

server <- function(input, output) {
  observeEvent(input$calculate, {
    age <- as.numeric(input$age)
    height <- as.numeric(input$height)
    abdomen <- as.numeric(input$abdomen)
    wrist <- as.numeric(input$wrist)
    
    if (age <= 0 | height <= 0 | abdomen <= 0 | wrist <= 0 | age > 100 | height > 100 | abdomen > 150 | wrist > 60) {
      output$bodyfat_result <- renderText({
        paste0('Invalid Input: values are not in a reasonable range! Age(0-100), Height(0-100), Abdomen(0-150), Wrist(0-60)')
      })
    } else {
      # standardize input
      age <- (age - mean(data$AGE))/sd(data$AGE)
      height <- (height - mean(data$HEIGHT))/sd(data$HEIGHT)
      abdomen <- (abdomen - mean(data$ABDOMEN))/sd(data$ABDOMEN)
      wrist <- (wrist - mean(data$WRIST))/sd(data$WRIST)
      bodyfat_per <- (18.8971429 + 0.2237683*age - 0.6684841*height + 6.2077191*abdomen -0.4217869*wrist)
      output$bodyfat_result <- renderText({
        paste0('Your Body Fat Percentage is: ', round(bodyfat_per, 2), '%')
      })
    }
  })
  output$data_table <- renderDT({
    datatable(data[c('BODYFAT', 'AGE', 'HEIGHT', 'ABDOMEN', 'WRIST')],options = list(paging = TRUE, searching = TRUE))
  })
  observe({
    feature <- input$feature
    output$hist_plot <- renderPlot({
      age <- as.numeric(input$age)
      height <- as.numeric(input$height)
      abdomen <- as.numeric(input$abdomen)
      wrist <- as.numeric(input$wrist)
      
      age_normalize <- (age - mean(data$AGE))/sd(data$AGE)
      height_normalize <- (height - mean(data$HEIGHT))/sd(data$HEIGHT)
      abdomen_normalize <- (abdomen - mean(data$ABDOMEN))/sd(data$ABDOMEN)
      wrist_normalize <- (wrist - mean(data$WRIST))/sd(data$WRIST)
      bodyfat_per <- (18.8971429 + 0.2237683*age_normalize - 0.6684841*height_normalize + 6.2077191*abdomen_normalize - 0.4217869*wrist_normalize)
      
      hist(data[[feature]], main = paste0('Histogram of ', feature), xlim=c(round(min(data[[feature]])), round(max(data[[feature]]))))
      if (feature == 'BODYFAT') {
        abline(v=bodyfat_per, col='red', lwd=2)
      }
      if (feature == 'AGE') {
        abline(v=age, col='red', lwd=2)
      }
      if (feature == 'HEIGHT') {
        abline(v = height, col = 'red', lwd=2)
      }
      if (feature == 'ABDOMEN') {
        abline(v = abdomen, col = 'red', lwd=2)
      }
      if (feature == 'WRIST') {
        abline(v = wrist, col = 'red', lwd=2)
      }
    })
  })
}
