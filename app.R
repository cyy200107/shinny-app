# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(ggplot2) # For plotting
library(shiny) # For creating an interactive app
library(DT) # For displaying data tables
library(dplyr)
library(tidyr)
library(leaflet)
library(forecast) # For ARIMA modeling and forecasting
library(readxl) # For reading Excel files
library(geosphere) # For calculating distances on the map
library(officer)


# Load GDP data
gdp_data <- read.csv("Trinidad_and_Tobago_GDP_Data__in_Billions_.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Ensure correct column names and data types
colnames(gdp_data)[1] <- "Year"
gdp_data$Year <- as.numeric(gdp_data$Year)
gdp_data$GDP <- as.numeric(gdp_data$GDP..Billion.US..)

# Drop missing values
gdp_data <- gdp_data %>% drop_na()

# Calculate first-order differencing
gdp_data <- gdp_data %>%
  mutate(GDP_Difference = c(NA, diff(GDP)))  # Add a new column with differenced GDP

# Load comparison data
comparison_data <- read_excel("Jamaica_Trinidad_Tobago_Comparison_2000_2020 .xlsx", sheet = "Jamaica_Trinidad_Tobago_Compari")

# UI Layout
ui <- navbarPage(
  title = "Exploring Trinidad and Tobago",
  
  # Tab 1: General Introduction
  tabPanel("General Introduction",
           sidebarLayout(
             sidebarPanel(
               h3("Exploring Trinidad and Tobago"),
               p("Interactive analysis of Trinidad and Tobago's economic, geographic, and social data.")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Introduction", h4("Introduction to Trinidad and Tobago"),
                          p("Trinidad and Tobago is a small, twin-island country in the southern section of the Caribbean, just off the coast of Venezuela. Port of Spain, its capital, and other urban centers include San Fernando and Scarborough. It is home to about 1.4 million people of various ethnic groups, including those of African and Indian descent. It is a unitary parliamentary republic with English as the official language."),
                          p("Trinidad and Tobago is among the Caribbean countries with the highest GDP per capita, mainly due to its strong energy industry. The GDP of the country in 2022 was approximately $25 billion USD, and per capita GDP was $17,000 USD. The economy depends on the production and export to foreign countries of oil and natural gas, which provide a hefty share of government revenue and foreign exchange earnings. Trinidad and Tobago is the Western Hemisphere’s fourth-largest natural gas exporter. Other key components of GDP are petrochemicals, LNG, fertilizers, and non-energy sectors like tourism, agriculture, and manufacturing."),
                          p("Trinidad and Tobago pots boiling with color, Carnival the mood, and calypso and soca the music. On the leisure geographic perspective, it comprises stunning beaches, tropical jungles, and distinctive environments like the Caroni Marsh. Trinidad is bigger and more industrialized, while Tobago is more touristy and laid-back. Trinidad and Tobago is a relatively diversified economy, though global energy prices experience fluctuation, thanks to its established energy infrastructure and position in global markets.")),
                 tabPanel("Map", leafletOutput("tt_map")),
                 tabPanel("Photos",
                          h4("Tourist Attractions in Trinidad and Tobago"),
                          img(src = "photo1.png", width = "80%", alt = "Photo 1"),
                          p("Fort King George: Tobago’s historic fort with scenic ocean views."),
                          img(src = "photo2.png", width = "80%", alt = "Photo 2"),
                          p("Maracas Bay: Trinidad's iconic white-sand beach with palm trees."),
                          img(src = "photo3.png", width = "80%", alt = "Photo 3"),
                          p("Pigeon Point: Tobago’s serene turquoise bay and lush hills."),
                          img(src = "photo4.png", width = "80%", alt = "Photo 4"),
                          p("Temple in the Sea: A serene Hindu temple on a tiny island.")
                 )
               )
             )
           )
  ),
  
  # Tab 2: GDP Projection
  tabPanel("GDP Projection",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h3("GDP Projection Analysis"),
                 p("This analysis focuses on the GDP (Billion US$) of Trinidad and Tobago.")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("GDP Data Plot",
                            plotOutput("gdp_plot"),
                            uiOutput("data")
                            ),
                   
                   tabPanel("GDP After Differencing", 
                            plotOutput("gdp_diff_plot"),
                            uiOutput('gdp_diff_analysis_text')
                            ),
                   tabPanel("ACF and PACF", 
                            plotOutput("acf_pacf_plot"),
                            uiOutput("acf_pacf_text")
                   ),
                   tabPanel("ARIMA Forecast",
                            plotOutput("arima_forecast_plot"),
                            uiOutput("arima_forecast_text")
                   )
                 )
               )
             )
           )
  ),
  
  # Tab 3: Data Comparison
  tabPanel("Data Comparison",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h4("Comparison of Jamaica and Trinidad & Tobago"),
                 p("This section compares GDP, GDP per capita, population, life expectancy, and homicide rates.")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Geographic Map: Jamaica to Trinidad & Tobago", 
                            leafletOutput("distance_map")),
                   tabPanel("GDP Comparison", plotOutput("gdp_comparison_plot"),
                            uiOutput("gdp_comparison_analysis_text")),
                  
                   
                   tabPanel("GDP Per Capita", plotOutput("gdp_per_capita_plot"),
                            uiOutput("gdp_per_capita")),
                   tabPanel("Population", plotOutput("population_plot"),
                            uiOutput("Population")),
                   
                   tabPanel("Life Expectancy", plotOutput("life_expectancy_plot"),
                            uiOutput("Life_expectancy")),
                   tabPanel("Homicide Rate", plotOutput("homicide_rate_plot"),
                            uiOutput("Homicide_rate"))
                 )
               )
             )
           )
  ),
  
  # Tab 4: SWOT Analysis
  tabPanel("SWOT Analysis",
           fluidPage(
             titlePanel("SWOT Analysis of Trinidad and Tobago"),
             fluidRow(
               column(12, h3("Strength")),
               column(12, p("• Trinidad and Tobago's GDP continues to grow steadily, driven by its strong petrochemical and LNG sectors.")),
               column(12, p("• High GDP per capita compared to other Caribbean nations reflects economic prosperity.")),
               column(12, p("• Significant investments in renewable energy and infrastructure development."))
             ),
             fluidRow(
               column(12, h3("Weakness")),
               column(12, p("• Overdependence on energy exports makes the economy vulnerable to price fluctuations.")),
               column(12, p("• An aging population increases healthcare costs and reduces the labor force.")),
               column(12, img(src = "photo5.png", width = "80%", alt = "Median Age Chart")),
               column(12, p("The chart above shows Trinidad and Tobago's median age trend, indicating challenges of an aging population."))
             ),
             fluidRow(
               column(12, h3("Opportunities")),
               column(12, p("• Expanding tourism and agriculture sectors can help diversify the economy.")),
               column(12, p("• Leveraging its strategic location to attract foreign investments and trade opportunities."))
             ),
             fluidRow(
               column(12, h3("Threats")),
               column(12, p("• Global warming threatens coastal infrastructure and the tourism industry.")),
               column(12, p("• Declining energy reserves pose long-term economic risks if diversification efforts fail."))
             )
           )
  )
)




  
# Server Logic
server <- function(input, output) {
  # Tab 1: Generate a map for Trinidad and Tobago
  output$tt_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -61.5, lat = 10.6, zoom = 8) %>%
      addMarkers(lng = -61.5, lat = 10.6, popup = "Trinidad and Tobago")
  })
  
  # Tab 2: Plot GDP data
  output$gdp_plot <- renderPlot({
    ggplot(gdp_data, aes(x = Year, y = GDP)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red") +
      labs(
        title = "GDP of Trinidad and Tobago (Billion US$)",
        x = "Year",
        y = "GDP (Billion US$)"
      ) +
      theme_minimal()
  })
  output$data <- renderUI({
    HTML("<p><b>Analysis:</b> The GDP of Trinidad and Tobago has shown steady growth over the years, 
       with significant increases observed between 2000 and 2010. 
       However, fluctuations after 2014 indicate economic instability, likely due to energy price volatility. 
       Overall, the GDP trend reflects strong economic dependence on the petrochemical and LNG sectors.</p>")
  })
  
  
  # Tab 2: Plot differenced GDP
  output$gdp_diff_plot <- renderPlot({
    ggplot(gdp_data, aes(x = Year, y = GDP_Difference)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "blue") +
      labs(
        title = "Differenced GDP of Trinidad and Tobago",
        x = "Year",
        y = "GDP Difference (Billion US$)"
      ) +
      theme_minimal()
  })
  
  output$gdp_diff_analysis_text <- renderUI({
    HTML("<p><b>Analysis:</b> The differenced GDP of Trinidad and Tobago highlights periods of sharp economic fluctuations. 
       Notable drops are observed around 1983, 2009, and 2020, corresponding to global economic crises or energy market volatility. 
       These sharp declines indicate the country's vulnerability to external shocks, particularly in the energy sector. 
       The variability suggests a need for economic diversification to mitigate future risks.</p>")
  })
  
  # Tab 2: Plot ACF and PACF
  output$acf_pacf_plot <- renderPlot({
    par(mfrow = c(1, 2))
    acf(gdp_data$GDP_Difference, na.action = na.pass, lag.max = 20, main = "ACF of Differenced GDP")
    pacf(gdp_data$GDP_Difference, na.action = na.pass, lag.max = 20, main = "PACF of Differenced GDP")
    par(mfrow = c(1, 1))
  })
  
  output$acf_pacf_text <- renderUI({
    HTML("<p><b>Analysis:</b> ACF and PACF show significant spikes at lag 1. The ARIMA(1, 1, 1) model is suggested.</p>")
  })
  
  # Tab 2: ARIMA Forecast
  output$arima_forecast_plot <- renderPlot({
    gdp_ts <- ts(gdp_data$GDP, start = min(gdp_data$Year), frequency = 1)
    arima_model <- auto.arima(gdp_ts)
    gdp_forecast <- forecast(arima_model, h = 15)
    forecast_data <- data.frame(
      Time = c(time(gdp_ts), time(gdp_forecast$mean)),
      Value = c(gdp_ts, gdp_forecast$mean),
      Type = c(rep("Original", length(gdp_ts)), rep("Forecast", length(gdp_forecast$mean)))
    )
    ggplot(forecast_data, aes(x = Time, y = Value, color = Type)) +
      geom_line(size = 1) +
      labs(
        title = "Differenced Time Series and ARIMA Forecast",
        x = "Time",
        y = "Value (Billion US$)",
        color = "Type"
      ) +
      scale_color_manual(values = c("Original" = "cyan", "Forecast" = "red")) +
      theme_minimal()
  })
  
  output$arima_forecast_text <- renderUI({
    HTML("<p><b>Analysis:</b> Based on forecasts, GDP stabilizes around $30 billion over 15 years.</p>")
  })
  
  # Tab 3: Distance Map between Jamaica and Trinidad
  output$distance_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -77.2975, lat = 18.1096, popup = "Jamaica") %>%
      addMarkers(lng = -61.5, lat = 10.6, popup = "Trinidad & Tobago") %>%
      addPolylines(lng = c(-77.2975, -61.5), 
                   lat = c(18.1096, 10.6), 
                   color = "blue", 
                   weight = 3) %>%
      addPopups(mean(c(-77.2975, -61.5)), 
                mean(c(18.1096, 10.6)), 
                popup = paste("Distance: ", 
                              round(distGeo(c(-77.2975, 18.1096), 
                                            c(-61.5, 10.6)) / 1000, 2), 
                              " km"))
  })
  
  
  # Tab 3: GDP Comparison Plot
  output$gdp_comparison_plot <- renderPlot({
    ggplot(comparison_data, aes(x = Year)) +
      geom_line(aes(y = `Jamaica GDP (Billion USD)`, color = "Jamaica GDP")) +
      geom_line(aes(y = `Trinidad and Tobago GDP (Billion USD)`, color = "Trinidad & Tobago GDP")) +
      labs(title = "GDP Comparison", x = "Year", y = "GDP (Billion USD)") +
      scale_color_manual(values = c("Jamaica GDP" = "blue", "Trinidad & Tobago GDP" = "red")) +
      theme_minimal()
  })
  
  output$gdp_comparison_analysis_text <- renderUI({
    HTML("<p><b>Analysis:</b> The GDP comparison between Jamaica and Trinidad & Tobago reveals significant differences. 
       Trinidad & Tobago experienced rapid economic growth from 2000 to 2010, largely driven by its energy sector. 
       However, after 2014, a decline is observed, likely due to energy price volatility and economic reliance on natural resources. 
       In contrast, Jamaica's GDP shows a steady and consistent growth trend, reflecting a more stable but slower economic development.</p>")
  })
  
  # Tab 3: GDP Per Capita Plot
  output$gdp_per_capita_plot <- renderPlot({
    ggplot(comparison_data, aes(x = Year)) +
      geom_line(aes(y = `Jamaica GDP per Capita (USD)`, color = "Jamaica GDP per Capita")) +
      geom_line(aes(y = `Trinidad and Tobago GDP per Capita (USD)`, color = "Trinidad & Tobago GDP per Capita")) +
      labs(title = "GDP Per Capita Comparison (2000-2020)", x = "Year", y = "GDP Per Capita (USD)") +
      scale_color_manual(values = c("Jamaica GDP per Capita" = "blue", "Trinidad & Tobago GDP per Capita" = "red")) +
      theme_minimal()
  })
  output$gdp_per_capita <- renderUI({
    HTML("<p><b>Analysis:</b> Trinidad & Tobago's GDP per capita grew faster and remained significantly higher than Jamaica's from 2000 to 2020. The gap widened over time.</p>")
  
  })
  # Tab 3: Population Plot
  output$population_plot <- renderPlot({
    ggplot(comparison_data, aes(x = Year)) +
      geom_line(aes(y = `Jamaica Population (Million)`, color = "Jamaica Population")) +
      geom_line(aes(y = `Trinidad and Tobago Population (Million)`, color = "Trinidad & Tobago Population")) +
      labs(title = "Population Comparison (2000-2020)", x = "Year", y = "Population (Million)") +
      scale_color_manual(values = c("Jamaica Population" = "blue", "Trinidad & Tobago Population" = "red")) +
      theme_minimal()
  })
  output$Population <- renderUI({
    HTML("<p><b>Analysis:</b> Jamaica's population is consistently higher than Trinidad & Tobago's, with both showing steady growth from 2000 to 2020.</p>")
    
  })
  
  # Tab 3: Life Expectancy Plot
  output$life_expectancy_plot <- renderPlot({
    ggplot(comparison_data, aes(x = Year)) +
      geom_line(aes(y = `Jamaica Life Expectancy (Years)`, color = "Jamaica Life Expectancy")) +
      geom_line(aes(y = `Trinidad and Tobago Life Expectancy (Years)`, color = "Trinidad & Tobago Life Expectancy")) +
      labs(title = "Life Expectancy Comparison (2000-2020)", x = "Year", y = "Life Expectancy (Years)") +
      scale_color_manual(values = c("Jamaica Life Expectancy" = "blue", "Trinidad & Tobago Life Expectancy" = "red")) +
      theme_minimal()
  })
  output$Life_expectancy <- renderUI({
    HTML("<p><b>Analysis:</b> Jamaica consistently had higher life expectancy than Trinidad & Tobago, with both showing steady improvements from 2000 to 2020.</p>")
    
  })
  # Tab 3: Homicide Rate Plot
  output$homicide_rate_plot <- renderPlot({
    ggplot(comparison_data, aes(x = Year)) +
      geom_line(aes(y = `Jamaica Homicide Rate (per 100,000)`, color = "Jamaica Homicide Rate")) +
      geom_line(aes(y = `Trinidad and Tobago Homicide Rate (per 100,000)`, color = "Trinidad & Tobago Homicide Rate")) +
      labs(title = "Homicide Rate Comparison (2000-2020)", x = "Year", y = "Homicide Rate (per 100,000)") +
      scale_color_manual(values = c("Jamaica Homicide Rate" = "blue", "Trinidad & Tobago Homicide Rate" = "red")) +
      theme_minimal()
  })
  output$Homicide_rate <- renderUI({
    HTML("<p><b>Analysis:</b> Jamaica's homicide rate is significantly higher and increases steadily, while Trinidad & Tobago's rate remains much lower with gradual growth.</p>")
    
  })
}




# Run the Shiny app
shinyApp(ui = ui, server = server)
