
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
        sidebarPanel("Change the displayed by choosing the following settings: ", 
                     width = 4,
          
          # Input: allow user to choose "emission" or "consumption" for y-axis
          selectInput(inputId = 'y', label = 'Y-Axis:',
                      choices = c("Greenhouse Gas Emissions (tons)" = "total_emissions", 
                                  "Waste Consumption (tons)" = "total_consumption"),
                      selected = "total_emissions"),
  
          # Input: allow user to choose whether or not to show data table
          checkboxInput(inputId = 'c', label = 'Show Data Table',
                        value = TRUE)
        ),
            
        #### MAIN PANEL ####
        mainPanel(
           plotOutput("scatterplot"),
           plotOutput("annual_emissions"),
           DT::dataTableOutput(outputId = "showdata")
           )
    )
)

#### SERVER ####
    
server <- function(input, output) {

      # Create an interactive bar graph (user chooses y-axis: emissions or consumption)
      output$annual_emissions <- renderPlot({
        ggplot(data = waste_tons, aes_string(x = "year", y = input$y)) + 
          geom_col() + 
          labs(x = "Year", y = as.character(input$y)) + 
          scale_y_continuous(labels = label_number(accuracy = .1, scale = 0.000001)) + 
          theme_classic() + 
          theme(axis.text.x = element_text(angle = 90))
      })
      
      # Create a scatterplot (total annual emissions ~ total annual consumption)
      output$scatterplot <- renderPlot({
        ggplot(data = waste_tons, aes(x = total_consumption, y = total_emissions)) + 
          geom_point() + 
          geom_smooth(method = 'lm', se = FALSE, color = "green") +
          labs(title ="Relationship between Waste Consumption \n and Greenhouse Gas Emissions",
               x = "Waste (millions of tons)", y = "Emissions (millions of tons)") + 
          scale_x_continuous(labels = label_comma(), limits = c(750000,925000)) +
          scale_y_continuous(labels = label_comma(), limits = c(150000,500000)) +
          theme_classic() + theme(plot.title = element_text(hjust = 0.5))
      })
      
      # Create a data table of waste consumption and emissions when Show Data checked
      waste_display <- data.frame(waste_tons %>%
                                  select(c(year, source, consumption, emissions)) %>%
                                  filter(is.na(consumption) == FALSE
                                         & is.na(emissions) == FALSE))
      
      output$showdata <- DT::renderDataTable(
        if(input$c){DT::datatable(data = waste_display,
                                  options = list(pageLength = 10),
                                  rownames = FALSE)
          }
      )
}

    
#### RUN APPLICATION ####
shinyApp(ui = ui, server = server)
