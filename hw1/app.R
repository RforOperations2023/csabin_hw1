
### Caroline Sabin
### 94-880 Section A3
### R Shiny for Operations Management
### HW 1: Building a Basic ShinyApp

#### PROJECT SET-UP ####

    # Load libraries
    library(shiny)
    library(shinythemes)
    library(readr)
    library(dplyr)
    library(ggplot2)
    library(scales)
    
    
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


#### USER INTERFACE ####
ui <- fluidPage(

    # Title of application
    titlePanel("Washington D.C. Citywide Greenhouse Gas Emissions: 2006-2020"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
        ),
            
        #### MAIN PANEL ####
        mainPanel(
           fluidRow(
             column(6, plotOutput("annual_emissions")),
              column(6, plotOutput("annual_consumption"))
             )
        )
    )
)

#### SERVER ####
    
server <- function(input, output) {

      # Create a scatterplot of total annual emissions
      output$annual_emissions <- renderPlot({
        ggplot(data = waste_tons, aes(x = year, y = total_emissions)) + 
          geom_col() + 
          labs(title ="Annual Greenhouse Gas Emissions in Washington D.C.",
               x = "Year", y = "Emissions (millions of tons)") + 
          scale_y_continuous(labels = label_number(accuracy = .1, scale = 0.000001)) + 
          theme_classic()
      })
      
      # Create a scatterplot of total annual consumption 
      output$annual_consumption <- renderPlot({
        ggplot(data = waste_tons, aes(x = year, y = total_consumption)) + 
          geom_col() +
          labs(title ="Annual Waste Consumption in Washington D.C.",
               x = "Year", y = "Waste (millions of tons)") + 
          scale_y_continuous(labels = label_number(accuracy = .1, scale = 0.000001)) +
          theme_classic()
      })
}

    
#### RUN APPLICATION ####
shinyApp(ui = ui, server = server)
