# Homework 1: Building a Basic Shiny App


## Assignment
* Create three (3) different kinds of plots/figures 
* Use DT to create one (1) data table
* Include at least two (2) different types of inputs
* One (1) functioning downloadButton() 
* Inputs must use reactivity in a logical manner with all outputs displayed to users

## Data Source

The data used for this project represents the results of the annual Greenhouse Gas Inventory conducted in Washington D.C. It represents the citywide and government-specific emissions between 2006 and 2020. 

The collection of this data was the result of an initiative by the Department of Energy & Environment (DOEE) of Washington D.C. The DOEE has two specific environmental goals that they have set for the District: 
* reducing emissions 60% by 2030 
* reaching net-zero emissions by 2045
<br> These data represent the Department's efforts to monitor the city's progress toward these two important goals. 

The raw data can be found here: [Open Data DC: Greenhouse Gas Inventory](https://opendata.dc.gov/datasets/DCGIS::greenhouse-gas-inventory/about)

Learn more about the DOEE's emission goals here: [DOEE Greenhouse Gas Inventories](https://doee.dc.gov/service/greenhouse-gas-inventories)

## R Shiny App

I created a web application using R Shiny to depict certain facets of this greenhouse gas inventory data. 

#### Annual Greenhouse Gas Emissions and Consumption Originating from Waste Sources
The top panel represents greenhouse gasses originating from *waste sources* —- compost, landfill, and incineration. The focus on these sectors was due to the public preoccupation with garbage disposal processes, and to imply the need for recycling practices to address these emission sources. The data is displayed by year so as to (hopefully) show progress toward the two DOEE goals.

The interface gives the user options to change the data displayed on the graph, toggling between total annual emissions and total annual consumption (in tons) coming from waste sources. There is also a download button that enables the user to download a subset of the data focused on the waste sector over these years. 

#### Annual Greenhouse Gas Emissions and Consumption Originating from Any Sector
The lower panel expands the scope of the data analysis, focusing not only on emissions from waste but also on those emissions from the full range of sectors. The interface gives the user the ability to change the year for which these data depict. There are two bar graphs between the tabs, showing the top five sectors with the most emissions and consumption values, respectively, in the user-selected year. The annual focus aims to allow the user to "zoom in" on a single year and dive into the sectors with the highest Greenhouse Gas numbers during that year.

#### Application
The web application can be viewed on <ins>shinyapps.io</ins>, at the following link: [Washington D.C. Greenhouse Gas Emissions: 2006 - 2020](https://7bewzr-caroline-sabin.shinyapps.io/CSabin_Greenhouse_Gas_Inventory/)


