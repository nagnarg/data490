#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require("shiny")) install.packages("shiny")
library(shiny)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("haven")) install.packages("haven")
library(haven)

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

if (!require("shinyWidgets")) install.packages("shinyWidgets")
library(shinyWidgets)

if (!require("tidymodels")) install.packages("tidymodels")
library(tidymodels)

if (!require("writexl")) install.packages("writexl")
library(writexl)


load(url("https://github.com/nagnarg/data490/raw/main/merged_df.RData"))

merged_df$Year <- as.integer(merged_df$Year)
merged_df$Age <- as.integer(merged_df$Age)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var stickySidebar = $('#sticky-sidebar');
      if (stickySidebar.length > 0) {  
        var originalOffsetY = stickySidebar.offset().top;
        $(window).on('scroll', function(event) {
          var scrollTop = $(window).scrollTop();
          stickySidebar.toggleClass('sticky', scrollTop >= originalOffsetY);
        });  
      }
    });
  "))),
  tags$head(
    tags$style(HTML("
    .sticky {
      position: fixed;
      top: 10;
    }
  "))),
  titlePanel("Analyzing Health Insurance Impact on Age-Adjusted Death Rates"),
  tabsetPanel(
    tabPanel("Overview",
             HTML("
             <h4><b>NOTE: The visualizations may take a few minutes to load.</b></h4>
             <h3><b>Author</b></h3>
             <p>Ed Dungo</p>
             <h3><b>Overview</b></h3>
             <p>In this project, I looked into the effects of various health insurance factors on age-adjusted death rates. 
             The age-adjusted death rate serves as a statistical tool for comparing mortality rates across populations with different age distributions. 
             Essentially, it allows organizations to assess how many individuals pass away in different locations or time periods. 
             My primary goal is to investigate whether health insurance has a significant role in influencing death rates within the United States. 
             It’s important to note that my study focuses specifically on the White/Caucasian and Black/African-American populations.</p>
             
                  <h3><b>Analysis</b></h3>
                   <p><b>Visualizations</b>: contain charts and plots relating to insurance types and age-adjusted death rate. 
                   The date filter lets you see the trend over time, which is useful for the density plot. 
                   In the box plot, without any filters, it appears that there's no significant variation,
                   but the range of death rates and outliers vary among groups. 
                   The bar chart on age-adjusted death rate over time by race is a static image.</p>
                  <p><b>Table</b>: contains a snapshot of the data used in this project.</p>
                  <p><b>Models</b>: summarizes the result of two linear regression models.
                  
                  <h3><b>Data Sources</b></h3>
             <p><a href='https://healthsurveys.ipums.org/'>IPUMS Health Surveys</a></p>
             <p><a href='https://catalog.data.gov/dataset/nchs-death-rates-and-life-expectancy-at-birth'>NCHS - Death Rates and Life Expectancy at Birth</a></p>
                  
                                    <h3><b>Data Description</b></h3>
                   <p><b>Year (Both)</b>: from 1996 to 2017.</p>
                   <p><b>Age (IPUMS)</b>: from 0 to 90.</p>
                   <p><b>Sex (Both)</b>: male or female.</p>
                   <p><b>Race (Both)</b>: White/Caucasian or Black/African-American.</p>
                   <p><b>Insurance Type (IPUMS)</b>:</p>
                   <ul>
                   <li>Has no insurance</li>
                   <li>Has two different insurance coverage in a calendar year</li>
                   <li>Has private health insurance</li>
                   <li>Has CHAMPUS, TRICARE, or CHAMP-VA insurance</li>
                   <li>Has Medicaid and/or SCHIP</li>
                   </ul>
                   </p>
                  <p><b>Average Life Expectancy (NCHS)</b>: from 66.10 to 81.40.</p>
                  <p><b>Age-Adjusted Death Rate (NCHS)</b>: summarizes the result of two linear regression models.
                  <h3><b>Data Cleaning and Merging</b></h3>
                  <p>Cleaning Process: <a href='https://github.com/nagnarg/data490/blob/main/data_cleaning.pdf'>GitHub Page</a></p>")),
    tabPanel("Visualizations", sidebarLayout(
      sidebarPanel(id = "sticky-sidebar",
                   textOutput("vizRowCount"),
                   sliderInput("vizFilterYear", "Year:", 
                               min = min(merged_df$Year, na.rm = TRUE), 
                               max = max(merged_df$Year, na.rm = TRUE), 
                               value = c(min(merged_df$Year, na.rm = TRUE), max(merged_df$Year, na.rm = TRUE)),
                               step = 1, 
                               ticks = FALSE, 
                               animate = TRUE),
        selectInput("vizFilterSex", "Sex:", choices = c("All", unique(merged_df$Sex))),
        selectInput("vizFilterRace", "Race:", choices = c("All", unique(merged_df$Race))),
        selectInput("densityFilterInsurance", "Insurance Type for Density Plot:", 
                    choices = c("All", unique(merged_df$`Insurance Type`)), 
                    selected = "All")
      ),
      mainPanel(
        plotOutput("insurancePlot"),
        plotOutput("deathRatePlot"),
        plotOutput("densityPlot"),
        plotOutput("deathRateByRacePlot")))),
    tabPanel("Table", 
             sidebarLayout(
               sidebarPanel(class = "sticky-sidebar",
                            textOutput("rowCount"),
                 selectInput("filterYear", "Year:", choices = unique(merged_df$Year)),
                 selectInput("filterSex", "Sex:", choices = unique(merged_df$Sex)),
                 selectInput("filterRace", "Race:", choices = unique(merged_df$Race)),
                 selectInput("filterInsurance", "Insurance Type:", choices = unique(merged_df$`Insurance Type`)),
                 uiOutput("insuranceTypeDesc")
               ),
               mainPanel(
                 tableOutput("table")))),
    tabPanel("Models",
             uiOutput("deathRateModelDescription"),
             verbatimTextOutput("deathRateModelSummary"),
             uiOutput("lifeExpectancyModelDescription"),
             verbatimTextOutput("lifeExpectancyModelSummary"))
  )
)

server <- function(input, output) {

  output$insuranceTypeDesc <- renderUI({
    insuranceDescriptions <- list(
      "Private" = "Indicates the person had private health insurance coverage from any source, including TRICARE/CHAMPVA, for at least one day during the calendar year.",
      "Two Insurance" = "Indicates the person had a mix two of insurance (where one could've been private) in a calendar year.",
      "Medicaid and/or SCHIP" = "Indicates the person had health insurance coverage through public assistance, Medicaid, or a State Children's Health Insurance Program (SCHIP).",
      "No Insurance" = "Indicates the person was not covered by health insurance at any time in the current calendar year.",
      "Medicare" = "Indicates the person had Medicare coverage for at least one day of the calendar year.",
      "CHAMPUS, TRICARE, or CHAMP-VA" = "Indicates the person had TRICARE health insurance coverage for at least one day during the calendar year (for 2000-forward) and CHAMPUS health insurance coverage for at least one day during the calendar year (for 1996-1999). TRICARE (formerly, CHAMPUS) is a Department of Defense regionally-managed health care program that provides comprehensive coverage for active duty US military personnel, military retirees, their dependents and survivors."
    )
    selectedInsurance <- input$filterInsurance
    if (selectedInsurance %in% names(insuranceDescriptions)) {
      return(helpText(insuranceDescriptions[[selectedInsurance]]))
    } else {
      return(helpText("Select an insurance type to see its description."))
    }
  })

  filteredData <- reactive({
    data <- merged_df
    if (input$filterYear != "All") {
      data <- data %>% filter(Year == input$filterYear)
    }
    if (input$filterSex != "All") {
      data <- data %>% filter(Sex == input$filterSex)
    }
    if (input$filterRace != "All") {
      data <- data %>% filter(Race == input$filterRace)
    }
    if (input$filterInsurance != "All") {
      data <- data %>% filter(`Insurance Type` == input$filterInsurance)
    }
    data
  })

  filteredDataForViz <- reactive({
    data <- merged_df
    if ("All" %in% input$vizFilterYear) {
      data <- data
    } else {
      data <- data %>% filter(Year >= input$vizFilterYear[1] & Year <= input$vizFilterYear[2])
    }
    if (input$vizFilterSex != "All") {
      data <- data %>% filter(Sex == input$vizFilterSex)
    }
    if (input$vizFilterRace != "All") {
      data <- data %>% filter(Race == input$vizFilterRace)
    }
    data
  })

  output$table <- renderTable({
    filteredData()
  })

  output$rowCount <- renderText({
    paste("Total Observations:", nrow(filteredData()))
  })

  output$insurancePlot <- renderPlot({
    ggplot(filteredDataForViz(), aes(x = `Insurance Type`)) + 
      geom_bar(fill = "#CD8500", color = "#5C5C5C") +
      theme(axis.text.x = element_text(size = 9, hjust = 0.5, angle = 15)) +
      labs(title = "Count of Insurance Types", 
           x = "Health Insurance Types", y = "Count") +
      scale_y_continuous(labels = scales::number_format(scale = 1))
  })

  output$deathRatePlot <- renderPlot({
    ggplot(filteredDataForViz(), aes(x = `Insurance Type`, y = `Age-Adjusted Death Rate`)) +
      geom_boxplot(fill = "#CD8500", color = "#5C5C5C") +
      theme(axis.text.x = element_text(size = 9, hjust = 0.5, angle = 15)) +
      labs(title = "Age-Adjusted Death Rate by Health Insurance Type", 
           x = "Health Insurance Types", y = "Age-Adjusted Death Rate")
  })

  output$densityPlot <- renderPlot({
    dataForDensityPlot <- merged_df
    
    if (length(input$vizFilterYear) == 2) {
      dataForDensityPlot <- dataForDensityPlot %>% 
        filter(Year >= input$vizFilterYear[1] & Year <= input$vizFilterYear[2])
    }
    if (input$vizFilterSex != "All") {
      dataForDensityPlot <- dataForDensityPlot %>% filter(Sex == input$vizFilterSex)
    }
    if (input$vizFilterRace != "All") {
      dataForDensityPlot <- dataForDensityPlot %>% filter(Race == input$vizFilterRace)
    }
    if (input$densityFilterInsurance != "All") {
      dataForDensityPlot <- dataForDensityPlot %>% filter(`Insurance Type` == input$densityFilterInsurance)
    }
  
    ggplot(dataForDensityPlot, aes(x = `Age-Adjusted Death Rate`)) +
      geom_density(fill = "#8968CD") +
      labs(x = "Age-Adjusted Death Rate", y = "Density") +
      ggtitle("Density Plot of Age-Adjusted Death Rate") +
      theme(plot.title.position = 'plot')
  })
  
  output$deathRateByRacePlot <- renderPlot({
    ggplot(data = merged_df, 
           aes(x = Year, 
               y = `Age-Adjusted Death Rate`, 
               fill = Race)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Age-Adjusted Death Rate Over Time by Race", 
           x = "Year", y = "Age-Adjusted Death Rate", fill = "Race")
  })

  transformedData <- reactive({
    merged_df %>%
      mutate(Sex = as.factor(Sex),
             Race = as.factor(Race),
             `Insurance Type` = as.factor(`Insurance Type`))
  })

  output$deathRateModelSummary <- renderPrint({
    model <- linear_reg() %>%
      set_engine("lm") %>%
      fit(`Age-Adjusted Death Rate` ~ `Insurance Type` + Sex + Age + Race, data = transformedData())
    summary(extract_fit_engine(model))
  })

  output$lifeExpectancyModelSummary <- renderPrint({
    model <- linear_reg() %>%
      set_engine("lm") %>%
      fit(`Average Life Expectancy` ~ `Insurance Type` + Sex + Age + Race, data = transformedData())
    summary(extract_fit_engine(model))
  })
  
  output$deathRateModelDescription <- renderUI({
    HTML("<h2>Age-Adjusted Death Rate Model</h2>
    <p><b>Response Variable</b>: Age-Adjusted Death Rate</p>
    <p><b>Predictor Variables</b>: Insurance Type, Sex, Age, Race</p>
    <p><b>Intercept</b>: 8.498e+02 or 849.8 is a reference point for comparison. Therefore, if all predictors were at their starting point, 
    then the age-adjusted death rate would be approximately 849.8.</p>
    <p><b>Coefficients</b>: Negative cofficient estimate for medicaid/SCHIP and positive coefficient estimates for no insurance, 
    private insurance, and having two insurance indicates their influence on death rate. Thus, a positive measure is typically associated with a higher death rate 
    while a negative measure is associated with a lower death rate.</p>
    <p><b>R-squared</b>: Approximately 81.92% of the model explains a significant portion of the variability in death rate.
         In other words, the model captured a significant portion of the variation in the age-adjusted death rates.</p>")
  })
  
  output$lifeExpectancyModelDescription <- renderUI({
    HTML("<h2>Life Expectancy Model</h2>
         <p><b>Response Variable</b>: Life Expectancy</p>
    <p><b>Predictor Variables</b>: Insurance Type, Sex, Age, Race</p>
    <p><b>Intercept</b>: 76.43 or 76 years of age is the reference point for comparison. Therefore, if all predictors were at their starting point, 
    then the life expectancy would be approximately 76 years old.</p>
    <p><b>Coefficients</b>: Various types of insurance have a small, but significant impact on life expectancy. 
    Positive values suggest a higher life expectancy than the baseline, while negative values suggest a lower expectancy. 
    In the result, it shows that being Caucasian/White leads to higher life expectancy.</p>
    <p><b>R-squared</b>: Approximately 90.44% of the model explains a significant portion of the variability in death rate.
         In other words, a large portio of the variables in the model explains the variability.</p>")
  })
  
  }

shinyApp(ui = ui, server = server)
