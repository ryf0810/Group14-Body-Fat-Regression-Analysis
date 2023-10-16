library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Body Fat Percentage Prediction Website"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
      # menuItem("Charts", tabName = "settings", icon = icon("sliders"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = 'Inputs',
                  status = 'primary', # has a prominent color to draw attention
                  solidHeader = TRUE, # solid background, make it distinct
                  height = 800,
                  img(src='exercise.gif'),
                  textInput('age', 'Input your Age(years): ', value='0'),
                  textInput('height', 'Input your Height(inches): ', value='0'),
                  textInput('abdomen', 'Input your Abdomen circumference(cm): ', value='0'),
                  textInput('wrist', 'Input your Wrist circumference(cm): ', value='0'),
                  actionButton('calculate', 'Calculate your Body Fat percentage'),
                  h4(textOutput('bodyfat_result'))
                ),
                box(
                  title = 'Histograms',
                  status = 'info',
                  height = 800,
                  selectInput(
                    'feature',
                    'Select features to display: ',
                    choices = c("BODYFAT", "AGE", "HEIGHT", "ABDOMEN", "WRIST"),
                    selected = 'BODYFAT'
                  ),
                  plotOutput('hist_plot')
                ),
                fluidRow(
                  box(
                    title = "Cleaned Training Data Table",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    DTOutput("data_table")
                  )
                )
              )
      )
    ),
    tags$div(class="footer",
             p("Contact Information - Yifan Ren"),
             p("Email: yren86@wisc.edu")
    )
  )
)
