#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# Load Library Packages
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)

# Load Data for Shiny Visualization
#file.directory <- "C:/Users/hmdsa/Documents/GitHub/hw1_asanda/Chicago_Energy.csv"
CEnergy <- read.csv("Chicago_Energy.csv")
#View(CEnergy)

# Clean data by removing incomplete reords
na.FindAndRemove <- function(mydata)
{
  # We need to find the number of na's per row. Write code that would loop through each column and find if the number of NAs is equal to the number # of rows and then remove any column with all NA's
  for (i in NCOL(mydata)) 
  {
    if (nrow(mydata[i]) == sum(sapply(mydata[i], is.na))) 
    {
      mydata <- mydata[-i]
    }
  }
  mydata <- na.omit(mydata)
  return(mydata)
}
CEnergy <- na.FindAndRemove(CEnergy)
#View(CEnergy)

#Pull Column names as list to input in dashboard
columns <- as.data.frame(colnames(CEnergy))
#View(columns)

columns.list <- split(columns, seq(nrow(columns)), rownames(columns))


column.names <- c("COMMUNITY.AREA.NAME", "CENSUS.BLOCK", "BUILDING.TYPE", "BUILDING_SUBTYPE", "KWH.JANUARY.2010", "KWH.FEBRUARY.2010", "KWH.MARCH.2010", "KWH.APRIL.2010", "KWH.MAY.2010", "KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010",
                  "KWH.SEPTEMBER.2010", "KWH.OCTOBER.2010", "KWH.NOVEMBER.2010", "KWH.DECEMBER.2010", "TOTAL.KWH", "ELECTRICITY.ACCOUNTS", "ZERO.KWH.ACCOUNTS", "THERM.JANUARY.2010", "THERM.FEBRUARY.2010", "THERM.MARCH.2010", "THERM.APRIL.2010",
                  "THERM.MAY.2010", "THERM.JUNE.2010", "THERM.JULY.2010", "THERM.AUGUST.2010", "THERM.SEPTEMBER.2010", "THERM.OCTOBER.2010", "THERM.NOVEMBER.2010", "THERM.DECEMBER.2010", "TOTAL.THERMS", "GAS.ACCOUNTS", "KWH.TOTAL.SQFT", "THERMS.TOTAL.SQFT",
                  "KWH.MEAN.2010", "KWH.STANDARD.DEVIATION.2010", "KWH.MINIMUM.2010", "KWH.1ST.QUARTILE.2010", "KWH.2ND.QUARTILE.2010", "KWH.3RD.QUARTILE.2010", "KWH.MAXIMUM.2010", "KWH.SQFT.MEAN.2010", "KWH.SQFT.STANDARD.DEVIATION.2010", "KWH.SQFT.MINIMUM.2010",
                  "KWH.SQFT.1ST.QUARTILE.2010", "KWH.SQFT.2ND.QUARTILE.2010", "KWH.SQFT.3RD.QUARTILE.2010", "KWH.SQFT.MAXIMUM.2010", "THERM.MEAN.2010", "THERM.STANDARD.DEVIATION.2010", "THERM.MINIMUM.2010", "THERM.1ST.QUARTILE.2010", "THERM.2ND.QUARTILE.2010",
                  "THERM.3RD.QUARTILE.2010", "THERM.MAXIMUM.2010", "THERMS.SQFT.MEAN.2010", "THERMS.SQFT.STANDARD.DEVIATION.2010", "THERMS.SQFT.MINIMUM.2010", "THERMS.SQFT.1ST.QUARTILE.2010", "THERMS.SQFT.2ND.QUARTILE.2010", "THERMS.SQFT.3RD.QUARTILE.2010",
                  "THERMS.SQFT.MAXIMUM.2010", "TOTAL.POPULATION", "TOTAL.UNITS", "AVERAGE.STORIES", "AVERAGE.BUILDING.AGE", "AVERAGE.HOUSESIZE", "OCCUPIED.UNITS", "OCCUPIED.UNITS.PERCENTAGE", "RENTER.OCCUPIED.HOUSING.UNITS", "RENTER.OCCUPIED.HOUSING.PERCENTAGE", "OCCUPIED.HOUSING.UNITS")
column.names <- as(column.names, "list")



# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  # Title of App
  titlePanel(title = "R Shiny for Ops Management"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = column.names,
                  selected = "KWH.JANUARY.2010"
                  ),
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = column.names,
                  selected = "COMMUNITY.AREA.NAME"
                  ),
      selectInput(inputId = "z", 
                  label = "Color by",
                  choices = column.names,
                  selected = "BUILDING.TYPE"
                  ),
      
      sliderInput(inputId = "alpha" 
                  , label = "Transparency"
                  , min = 0
                  , max = 1
                  , value = 0.5
                  , step = 0.1 ),
      # Enter text for plot title ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as scatter plot title"),
      
      checkboxInput(inputId = "checkbox"
                    , label = "Data Table?"
                    , value = TRUE),
      
      # Select which types of property types to plot ------------------------
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select property type(s):",
                         choices = c("Commercial", "Industrial", "Residential"),
                         selected = "Commercial"),
      
      # Select sample size ----------------------------------------------------
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(CEnergy), 
                   value = 1),
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot")
      # Print number of obs plotted ---------------------------------
      , uiOutput(outputId = "n"),
      br(), br()    # a little bit of visual separation
      , plotOutput(outputId = "histogram")
      , br(), br()    # a little bit of visual separation
      , plotOutput(outputId = "boxplot")
      , DT::dataTableOutput(outputId = 'datatable')
      , br(), br()    # a little bit of visual separation
    )
  )
)

# Define server function required to create Visualizations ---------
server <- function(input, output, session) {
  
  # Convert plot_title toTitleCase ----------------------------------
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })  

  Energy_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(CEnergy, BUILDING.TYPE  %in% input$selected_type)
  })
  
  # Create new df that is n_samp obs from selected type properties ------
  Energy_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(Energy_subset(), input$n_samp)
  })  
  
  # Update the maximum allowed n_samp for selected type movies ------
  observe({
    updateNumericInput(session,
                       inputId = "n_samp",
                       value = min(10, nrow(Energy_subset())),
                       max = nrow(Energy_subset())
    )
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = Energy_sample(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point( alpha = input$alpha) +
      labs(title = pretty_plot_title()
           )
    })
  
  # Print number of energy consumption data points plotted ----------------------------------
  output$n <- renderUI({
    types <- Energy_sample()$selected_type %>% 
      factor(levels = input$selected_type) 
    counts <- nrow(Energy_sample())
    HTML(paste("There are", counts, input$selected_type, "properties in this dataset. <br>"))
  })  
  
  # Create histogram object the plotOutput function is expecting --
  output$boxplot <- renderPlot({
    ggplot(data = Energy_sample(), aes_string(input$x)) +
      geom_histogram(stat = "count") +
      labs(title = pretty_plot_title()
      )
  })  
  
  # Create box plot object the plotOutput function is expecting --
  output$histogram <- renderPlot({
    ggplot(data = Energy_sample(), aes_string(input$x, input$y)) +
      geom_boxplot(varwidth = T, fill = "plum") +
      labs(title = pretty_plot_title()
      )
  })    
  
  output$datatable <- DT::renderDataTable(
    if(input$checkbox){
      DT::datatable(data = Energy_sample(), 
                    options = list(pageLength = 25), 
                    rownames = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = paste(input$selected_type, ".csv", sep = ""),
   # modify to only pull for selected options
     content = function(filename) {
      write.csv(Energy_sample(), filename, row.names = FALSE)
    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

