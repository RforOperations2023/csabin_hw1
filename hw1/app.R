
### Caroline Sabin
### 94-880 Section A3
### R Shiny for Operations Management
### HW 1: Building a Basic ShinyApp

#### SET-UP ####

    # Load libraries
    library(shiny)
    library(shinythemes)
    library(readr)
    library(dplyr)
    library(ggplot2)
    
    
    # Load data 
    ghg_inventory <- read_csv("~/csabin_hw1/hw1/Greenhouse_Gas_Inventory.csv")

    
#### DATA MANIPULATION ####

    # Change column names
    colnames(ghg_inventory) <- c("inventory", "sector", "source", "year", 
                             "consumption_units","consumption", "emissions", "ID")
    
    # Convert to data frame
    ghg_inventory <- data.frame(ghg_inventory)
    
    # Change certain variables to factors
    ghg_inventory$inventory <- as.factor(ghg_inventory$inventory)
    ghg_inventory$sector <- as.factor(ghg_inventory$sector)
    ghg_inventory$source <- as.factor(ghg_inventory$source)
    ghg_inventory$year <- as.factor(ghg_inventory$year)
    
    
    # Create a subset of dataset containing only observations in "waste" sector
    waste_tons <- ghg_inventory %>% 
                    filter(inventory == "Citywide", sector == "Waste",
                           consumption_units == "Tons") %>%
                    group_by(year) %>%
                    mutate(total_consumption = sum(consumption, na.rm = TRUE),
                           total_emissions = sum(emissions, na.rm = TRUE))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
