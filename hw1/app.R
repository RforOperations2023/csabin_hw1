
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
    library(RColorBrewer)
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
        sidebarPanel("Change the displays by manipulating the settings below: ", 
                     width = 3,
          
          # Input: allow user to choose "emission" or "consumption" for y-axis
          selectInput(inputId = 'y', label = 'Y-Axis:',
                      choices = c("Greenhouse Gas Emissions (tons)" = "total_emissions", 
                                  "Waste Consumption (tons)" = "total_consumption"),
                      selected = "total_emissions"),
  
          # Input: allow user to choose whether or not to show data table
          checkboxInput(inputId = 'c', label = 'Show Data Table',
                        value = TRUE),
          
          # Input: allow user to select year for top sector analysis
          radioButtons(inputId = "selected_year", label = "Year",
                       choices = c("2006", "2009", "2010","2011", "2012", 
                                   "2013", "2014", "2015","2016", "2017", 
                                   "2018", "2019", "2020"),
                             selected = "2020")
        ),
            
        #### MAIN PANEL ####
        mainPanel(
          
          # Output: Scatterplot 
          textOutput("scatter_text"),
          fluidRow(
             plotOutput("scatterplot"),
           ),
          
          
          # Output: single row with plot and data table (if shown)
          
          fluidRow(
             column(7, plotOutput("annual_wastetons")),
             column(5, DT::dataTableOutput(outputId = "showdata"))
           ),
          
          # Output: tabset with emission and consumption bar graphs
          tabsetPanel(type = "tabs",
                      tabPanel("Emissions", plotOutput("top5emissions")),
                      tabPanel("Consumption", plotOutput("top5consumption"))
           )
        )
    )
)
    
#### SERVER ####
    
server <- function(input, output) {

      # Create an interactive bar graph (user chooses y-axis: emissions or consumption)
        
          # Reactive graph title
            plot_title <- renderText({ 
              if(input$y == "total_emissions"){
                plot_title = "Total Annual Greenhouse Gas Emissions \n from Waste Sources"
              } else if (input$y == "total_consumption"){
                plot_title = "Total Annual Waste Consumption \n in Washington D.C."
              }
            })
  
          # Reactive y-axis label
            yaxis_title <- reactive({
              req(input$y)
              if (input$y == "total_emissions"){
                yaxis_title <- "Greenhouse Gas Emissions (tons)"
              } else if(input$y == "total_consumption"){
                yaxis_title <- "Waste Consumption (tons)"
              }
            })
        
          # Character Strings matching yaxis_title
            y_strings <- c("Greenhouse Gas Emissions (tons)" = "total_emissions",
                           "Waste Consumption (tons)" = "total_consumption")
  
          # Plot (annual emissions OR annual consumption ~ year)
            output$annual_wastetons <- renderPlot({
              ggplot(data = waste_tons, 
                     aes_string(x = "year", y = input$y, fill = "source")) + 
                geom_col() + 
                ggtitle(req(plot_title())) +
                labs(x = "Year", 
                     y = names(y_strings[which(y_strings == input$y)])) +
                scale_y_continuous(labels = label_comma()) + 
                scale_fill_brewer(palette = "Dark2") +
                theme_classic() + 
                theme(axis.text.x = element_text(angle = 90)) + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(legend.position = "bottom")
            })
        
            # Create text to be shown above graph in App interface
            
            output$scatter_text <- renderText(
              print("Annual Greenhouse Gas Emissions and Consumption \n Originating from Waste Sources")
            )
            
      # Create a scatterplot (total annual emissions ~ total annual consumption)
      output$scatterplot <- renderPlot({
        ggplot(data = waste_tons, aes(x = total_consumption, y = total_emissions)) + 
          geom_point() + 
          geom_smooth(method = 'lm', se = FALSE, color = "#60A3D9") +
          labs(title ="Relationship between Waste Consumption \n and Greenhouse Gas Emissions",
               x = "Waste (tons)", y = "Emissions (tons)") + 
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
                                  options = list(pageLength = 5),
                                  rownames = FALSE)
          }
      )
      
      
      # Create a reactive data frame for *consumption* that filters by selected year
          top5consumption_selectedyear <- reactive({
              req(input$selected_year)
        
              consumption_selectedyear <- ghg_inventory %>% 
                  select (year, sector, source, consumption) %>% 
                  filter(year == input$selected_year) %>%
                  group_by(sector, year) %>%
                  mutate(sector_consumption = sum(consumption, na.rm = TRUE)) %>%
                  arrange(desc(sector_consumption)) %>% 
                  select (year, sector, sector_consumption)
              
              sectorconsumption_selectedyear <- data.frame(unique(consumption_selectedyear))
              
              top5consumption <- sectorconsumption_selectedyear %>%
                arrange(desc(sector_consumption)) %>%
                top_n(sector_consumption, n = 5)

            })
          
          
          # Create an interactive bar graph showing top 5 consumption sectors in chosen year
          output$top5consumption <- renderPlot({
            ggplot(data = top5consumption_selectedyear(), 
                   aes(x = sector, y = sector_consumption, fill = sector)) + 
            geom_col() + 
            labs(title = paste0("Top Five Sectors with Highest\n Energy Consumption in ", input$selected_year)) +
            xlab("Sector") + ylab("Consumption (billions of tons)") + 
            scale_y_continuous(labels = label_number(scale = 0.000000001)) + 
            scale_fill_brewer(palette = "Set2") +
            theme_classic() + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(legend.position = "none")
            })
            
          
      # Create a reactive data frame for *emission* that filters by selected year
          top5emissions_selectedyear <- reactive({
            req(input$selected_year)
            
            emissions_selectedyear <- ghg_inventory %>% 
              select (year, sector, source, emissions) %>% 
              filter(year == input$selected_year) %>%
              group_by(sector, year) %>%
              mutate(sector_emissions = sum(emissions, na.rm = TRUE)) %>%
              arrange(desc(sector_emissions)) %>% 
              select (year, sector, sector_emissions)
            
            sectoremissions_selectedyear <- data.frame(unique(emissions_selectedyear))
            
            top5emissions <- sectoremissions_selectedyear %>%
              arrange(desc(sector_emissions)) %>%
              top_n(sector_emissions, n = 5)
            
          })
          
          # Create an interactive bar graph showing top 5 emissions sectors in chosen year
          output$top5emissions <- renderPlot({
            ggplot(data = top5emissions_selectedyear(), 
                   aes(x = sector, y = sector_emissions, fill = sector)) + 
            geom_col() + 
            labs(title = paste0("Top Five Sectors with Highest\n Greenhouse Gas Emissions in ", input$selected_year)) +
            xlab("Sector") + ylab("Emissions (millions of tons)") + 
            scale_y_continuous(labels = label_number(scale = 0.000001)) + 
            scale_fill_brewer(palette = "Set1") +
            theme_classic() + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            theme(legend.position = "none")
          })
          
          
      
}

    
#### RUN APPLICATION ####
shinyApp(ui = ui, server = server)
