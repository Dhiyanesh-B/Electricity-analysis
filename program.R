library(shiny)
library(shinydashboard)
library(readr)
library(data.table)
library(vroom)
library(ggplot2)
library(bslib)
library(dplyr)
library(lubridate)

file_path <- "D:/SEM-3/Books/23PD08,23PD34,23PD38/electricityConsumptionAndProductioction.csv"
dt_data <- fread(file_path)

dt_data$DateTime <- as.POSIXct(dt_data$DateTime, format="%Y-%m-%d %H:%M:%S")
dt_data$Year <- as.numeric(format(dt_data$DateTime, "%Y"))

data_2019 <- subset(dt_data, Year == 2019)
data_2020 <- subset(dt_data, Year == 2020)
data_2021 <- subset(dt_data, Year == 2021)
data_2022 <- subset(dt_data, Year == 2022)
data_2023 <- subset(dt_data, Year == 2023)
data_2024 <- subset(dt_data, Year == 2024)

yearMeanCond <- c(mean(data_2019$Consumption, na.rm = TRUE), 
                  mean(data_2020$Consumption, na.rm = TRUE), 
                  mean(data_2021$Consumption, na.rm = TRUE), 
                  mean(data_2022$Consumption, na.rm = TRUE), 
                  mean(data_2023$Consumption, na.rm = TRUE), 
                  mean(data_2024$Consumption, na.rm = TRUE))

yearMeanProdd <- c(mean(data_2019$Production, na.rm = TRUE), 
                   mean(data_2020$Production, na.rm = TRUE), 
                   mean(data_2021$Production, na.rm = TRUE), 
                   mean(data_2022$Production, na.rm = TRUE), 
                   mean(data_2023$Production, na.rm = TRUE), 
                   mean(data_2024$Production, na.rm = TRUE))

ui <- dashboardPage(
  dashboardHeader(title = "Analysis DashBoard"),
  dashboardSidebar(
    sliderInput("yearRange", "Select Year Range:", 
                min = 2019, max = 2024, 
                value = c(2019, 2024), step = 1, 
                animate = TRUE)
  ),
  dashboardBody(
    page_fluid(
      navset_tab(
        nav_panel("Production Vs Consumption", plotOutput("linePlot")),
        nav_panel("Contribution of Resources", plotOutput("pieChart")),
        nav_panel("Consumption Over Years", plotOutput("consumptionDeviationPlot")),
        nav_panel("Yearly Averages", plotOutput("yearlyAveragesPlot")),
        nav_panel("Renewable vs Non-Renewable", plotOutput("renewablePlot")),
        nav_panel("Correlation Analysis", plotOutput("correlationPlot")),
        nav_panel("Energy Variability", plotOutput("variabilityPlot")),
        nav_panel("Significant Production Hours", plotOutput("significantHoursPlot"))
      )
    )
  )
)

server <- function(input, output) {
  output$linePlot <- renderPlot({
    filtered_data <- dt_data[dt_data$Year >= input$yearRange[1] & dt_data$Year <= input$yearRange[2], ]
    
    ggplot(filtered_data, aes(x = DateTime)) +
      geom_line(aes(y = Consumption, color = "Consumption")) +
      geom_line(aes(y = Production, color = "Production")) +
      labs(title = "Electricity Consumption and Production Over Time",
           x = "Date", y = "Electricity (in units)") +
      scale_color_manual("", breaks = c("Consumption", "Production"), values = c("blue", "orange")) +
      theme_minimal()
  })
  
  output$pieChart <- renderPlot({
    meanPerResource <- c(mean(dt_data$Nuclear, na.rm = TRUE), mean(dt_data$Wind, na.rm = TRUE), 
                         mean(dt_data$Hydroelectric, na.rm = TRUE), mean(dt_data$`Oil and Gas`, na.rm = TRUE), 
                         mean(dt_data$Coal, na.rm = TRUE), mean(dt_data$Solar, na.rm = TRUE), 
                         mean(dt_data$Biomass, na.rm = TRUE))
    
    resourceNames <- c("Nuclear", "Wind", "Hydroelectric", "Oil and Gas", "Coal", "Solar", "Biomass")
    percentages <- round((meanPerResource / sum(meanPerResource)) * 100, 2)
    labels <- paste(resourceNames, "(", percentages, "%)", sep = "")
    
    pie(meanPerResource, labels = labels, col = rainbow(length(meanPerResource)), main = "Electricity Production Across Resources")
  })
  
  output$consumptionDeviationPlot <- renderPlot({
    consum <- (dt_data$Consumption - mean(dt_data$Consumption, na.rm = TRUE)) / sd(dt_data$Consumption, na.rm = TRUE)
    
    plot(dt_data$DateTime, consum, type = "o", col = "purple",
         main = "Standardized Consumption Deviation",
         xlab = "Date", ylab = "Standardized Consumption Deviation",
         ylim = range(consum, na.rm = TRUE))
    abline(h = 0, col = "red", lty = 2)
  })
  output$yearlyAveragesPlot <- renderPlot({
    yr <- 2019:2024
    plot(yr, yearMeanCond, type = 'l', col = 'blue', xlab = 'Year', ylab = 'Mean Electricity (units)', 
         main = 'Yearly Mean Consumption and Production', ylim = c(5500, 8000))
    lines(yr, yearMeanProdd, col = 'red')
    legend("topright", legend = c("Consumption", "Production"), col = c("blue", "red"), lty = 1)
  })
  
  output$renewablePlot <- renderPlot({
    Y <- sapply(list(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024), function(year_data) {
      c(sum(year_data$Biomass + year_data$Wind + year_data$Hydroelectric + year_data$Solar) / sum(year_data$Production),
        sum(year_data$Nuclear + year_data$`Oil and Gas` + year_data$Coal) / sum(year_data$Production))
    })
    barplot(Y, beside = TRUE, col = c("green", "brown"), main = "Renewable vs Non-Renewable Energy (2019-2024)",
            names.arg = 2019:2024, ylab = "Proportion of Total Production", legend.text = c("Renewable", "Non-Renewable"))
  })
  
  output$correlationPlot <- renderPlot({
    correlation <- cor(dt_data$Consumption, dt_data$Production, method = "pearson")
    model <- lm(dt_data$Production ~ dt_data$Consumption)
    
    plot(dt_data$Consumption, dt_data$Production, 
         main = paste("Electricity Consumption vs. Production Levels\nCorrelation Coefficient:", round(correlation, 2)),
         xlab = "Electricity Consumption (in units)", ylab = "Production Levels (in units)", pch = 19, col = "blue")
    
    abline(model, col = "red", lwd = 2)
    intercept <- round(coef(model)[1], 2)
    slope <- round(coef(model)[2], 2)
    equation <- paste("y =", intercept, "+", slope, "* x")
    text(x = mean(dt_data$Consumption), y = max(dt_data$Production), labels = equation, col = "darkgreen", cex = 0.9)
  })
  
  output$variabilityPlot <- renderPlot({
    energy_sources <- c("Nuclear", "Wind", "Hydroelectric", "Oil and Gas", "Coal", "Solar", "Biomass")
    variability <- sapply(energy_sources, function(source) sd(dt_data[[source]], na.rm = TRUE))
    
    variability_df <- data.frame(EnergySource = energy_sources, StandardDeviation = variability)
    ggplot(variability_df, aes(x = EnergySource, y = StandardDeviation, fill = EnergySource)) +
      geom_bar(stat = "identity") +
      labs(title = "Variability of Energy Production by Source", x = "Energy Source", y = "Standard Deviation of Production") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$significantHoursPlot <- renderPlot({
    dt_data <- dt_data %>% mutate(Difference = Production - Consumption)
    
    significant_hours <- dt_data %>%
      filter(Difference > 10)
    
    ggplot(significant_hours, aes(x = DateTime, y = Difference)) +
      geom_line(color = "blue") +
      labs(title = "Hours Where Production Exceeds Consumption Significantly",
           x = "Date and Time", y = "Difference (Production - Consumption)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
