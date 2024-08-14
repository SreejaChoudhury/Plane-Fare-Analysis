# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Read the dataset
df <- read.csv("Clean_Dataset.csv")

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Airline Ticket Price Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("Plane Fare", tabName = "plane_fare", icon = icon("plane")),
      menuItem("Estimation of Price", tabName = "price_estimation", icon = icon("calculator")),
      menuItem("Comparison based on City", tabName = "city_comparison", icon = icon("bar-chart")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("introduction",
              fluidRow(
                column(12,
                       tags$div(
                         style = "text-align: left;",
                         tags$h2("Embark on a Journey Through the Skies!"),
                         tags$p("Hi! I am Sreeja Choudhury, a student of CMI. Welcome aboard to a captivating exploration of the dynamic world of airline ticket pricing."),
                         tags$h3("Uncover the Secrets of the Skies:"),
                         tags$p("An airline, more than just a carrier of passengers and freight, is a complex symphony of strategies, algorithms, and pricing dynamics."),
                         tags$p("Ever wondered how airlines set ticket prices? The art and science behind it involve juggling various factors such as flight duration, days left for departure, arrival and departure times, and more."),
                         tags$p("Join us as we demystify the skies and delve into a dataset that unveils the hidden patterns shaping the airline industry."),
                         tags$h3("What Lies Ahead:"),
                         tags$p("This analysis promises a panoramic view of the airline landscape, offering insights into pricing trends, unveiling popular flight routes, and deciphering the impact of departure times on ticket costs."),
                         tags$p("Fasten your seatbelts as we navigate through data-rich clouds, unlocking the secrets that influence your journey from takeoff to touchdown.")
                       )
                )
              )
      ),
      tabItem("plane_fare",
              sidebarLayout(
                sidebarPanel(
                  # Dropdown menu for selecting the graph
                  selectInput("graph_type", "Select Graph",
                              choices = c("Boxplot for Price of Different Classes",
                                          "Stacked Bar Diagram for Source City",
                                          "Stacked Bar Diagram for Departure Time",
                                          "Boxplot for Price for Different Airlines")),
                ),
                mainPanel(
                  plotOutput("selected_plot")
                )
              )
      ),
      tabItem("price_estimation",
              sidebarLayout(
                sidebarPanel(
                  # Input for the number of days left for departure
                  numericInput("days_left_input", "Number of Days Left for Departure:", value = 10, min = 1, max = 100),
                  selectInput("airline_input", "Select Airline:",
                              choices = unique(df$airline)),
                  selectInput("source_city_input", "Select Source City:",
                              choices = unique(df$source_city)),
                  selectInput("destination_city_input", "Select Destination City:",
                              choices = unique(df$destination_city)),
                  selectInput("departure_time_input", "Select Departure Time:",
                              choices = unique(df$departure_time)),
                  selectInput("class_input", "Select Class:",
                              choices = unique(df$class)),
                  br(),
                  actionButton("estimate_button", "Estimate Price")
                ),
                mainPanel(
                  tags$style(HTML(".shiny-output-error-validation { color: #008000; font-size: 14px; }")), # Increase font size for error/validation messages
                  textOutput("estimated_price_output")
                )
              )
      ),
      tabItem("city_comparison",
              sidebarLayout(
                sidebarPanel(
                  # Input for selecting airline, source city, and departure time
                  selectInput("airline_comparison_input", "Select Airline:",
                              choices = unique(df$airline)),
                  selectInput("source_city_comparison_input", "Select Source City:",
                              choices = unique(df$source_city)),
                  selectInput("departure_time_comparison_input", "Select Departure Time:",
                              choices = unique(df$departure_time)),
                ),
                mainPanel(
                  tags$style(HTML(".shiny-output-error-validation { color: #008000; font-size: 14px; }")), # Increase font size for error/validation messages
                  textOutput("flights_available_output"),
                  plotOutput("flights_comparison_plot"),
                  plotOutput("departure_time_comparison_plot")
                )
              )
      ),
      tabItem("conclusion",
              fluidRow(
                column(12,
                       tags$div(
                         style = "text-align: left;",
                         tags$h2("Results and Conclusion"),
                         tags$h3("Results:"),
                         tags$p("The main findings from our analysis are that we observed significant price variations among different airlines. Some airlines consistently offered lower fares, while others tended to be more expensive. Passengers may benefit from exploring multiple airline options to find the best deals."),
                         tags$p("Prices varied considerably depending on the specific routes. Some routes were consistently more affordable, while others commanded premium fares. Understanding the pricing dynamics of popular routes can help travelers make informed decisions."),
                         tags$h3("Advanced Booking:"),
                         tags$p("Our analysis confirmed that booking flights well in advance generally resulted in lower ticket prices. Last-minute bookings tended to be more expensive. Travelers seeking cost-effective options should plan their trips and book tickets with sufficient lead time."),
                         tags$p("We noticed that certain airlines employed dynamic pricing strategies, adjusting ticket prices based on factors such as demand, time to departure, and seat availability. These pricing tactics underscore the importance of flexible travel plans for budget-conscious travelers."),
                         tags$p("Ticket prices also exhibited regional variations, with certain destinations consistently offering more affordable airfare options than others. Travelers can leverage this information when selecting their travel destinations.")
                       ),
                       tags$h3("Conclusion:"),
                       tags$p("In conclusion, understanding the factors influencing plane ticket prices is essential for travelers seeking to make informed choices and optimize their travel budgets. By considering the insights gained from this analysis and remaining vigilant for deals and discounts, passengers can enhance their overall travel experience while minimizing expenses.")
                )
              )
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Render the selected plot based on user input
  output$selected_plot <- renderPlot({
    if (input$graph_type == "Boxplot for Price of Different Classes") {
      ggplot(df, aes(x = class, y = price, fill = airline)) +
        geom_boxplot() +
        labs(title = "Boxplot of Price for different classes of different Airlines",
             x = "Class",
             y = "Price") +
        theme_minimal()
    } else if (input$graph_type == "Stacked Bar Diagram for Source City") {
      ggplot(df, aes(x = source_city, fill = airline)) +
        geom_bar(position = "stack") +
        labs(title = "Stacked Bar Diagram for Source City based on Different Airlines",
             x = "Source City",
             y = "Frequency",
             fill = "Airline") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$graph_type == "Stacked Bar Diagram for Departure Time") {
      ggplot(df, aes(x = departure_time, fill = airline)) +
        geom_bar(position = "stack") +
        labs(title = "Stacked Bar Diagram for Departure Time based on Different Airlines",
             x = "Departure Time",
             y = "Frequency",
             fill = "Airline") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$graph_type == "Boxplot for Price for Different Airlines") {
      ggplot(df, aes(x = airline, y = price, fill = airline)) +
        geom_boxplot() +
        labs(title = "Boxplot of Price for Different Airlines",
             x = "Airline Company",
             y = "Price") +
        theme_minimal()
    }
  })
  
  # Render the number of flights available based on user input
  output$flights_available_output <- renderText({
    # Filter the data based on user inputs
    filtered_data <- filter(df,
                            airline == input$airline_comparison_input,
                            source_city == input$source_city_comparison_input,
                            departure_time == input$departure_time_comparison_input)
    
    # Return the number of flights available
    paste("Number of flights available:", nrow(filtered_data))
  })
  
  # Render the flights comparison plot based on user input
  output$flights_comparison_plot <- renderPlot({
    ggplot(df, aes(x = airline, fill = airline)) +
      geom_bar() +
      labs(title = "Bar Diagram of Number of Flights for Various Airlines",
           x = "Airline",
           y = "Number of Flights") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the departure time comparison plot based on user input
  output$departure_time_comparison_plot <- renderPlot({
    ggplot(df, aes(x = departure_time, fill = airline)) +
      geom_bar(position = "stack") +
      labs(title = "Stacked Bar Diagram of Departure Time for Various Airlines",
           x = "Departure Time",
           y = "Frequency",
           fill = "Airline") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Estimation of price based on the linear regression model
  output$estimated_price_output <- renderText({
    req(input$estimate_button)
    
    # Filter the data based on user inputs
    filtered_data <- filter(df,
                            airline == input$airline_input,
                            source_city == input$source_city_input,
                            destination_city == input$destination_city_input,
                            departure_time == input$departure_time_input,
                            class == input$class_input,
                            days_left == input$days_left_input)
    
    # Check if the selected airline is Vistara or Air India
    if (input$class_input == "Business" && !(input$airline_input %in% c("Vistara", "Air India"))) {
      return(paste("Business class is not available for", input$airline_input))
    }
    
    # Perform simple linear regression
    lm_model <- lm(price ~ days_left, data = filtered_data)
    
    # Make predictions based on user input
    new_data <- data.frame(days_left = input$days_left_input)
    predicted_price <- predict(lm_model, newdata = new_data)
    
    # Return the estimated price
    paste("Estimated Price of Plane Ticket:", round(predicted_price, 2))
  })
}

# Run the Shiny app
shinyApp(ui, server)
