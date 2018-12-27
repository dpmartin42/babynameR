library(shiny)
library(dplyr)  # for data manipulation
library(lubridate)  # for datetime manipulation
library(ggplot2)  # for plotting
library(babynames)  # for babynames data

ui <- fluidPage(
  
  # Add title page
  titlePanel("babynameR"),
  
  # Begin sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("This is an application to visualize and explore the popularity of babynames provided by the Social Security 
                Administration. Names must be used at least five times in one year in order to be reported. Up to eight (8) 
                names can be plotted at once, sex can either be male or female, and the date can range from 1880 to 2015."),
      
      helpText("Additionally, both male and female names that are similar to the first name in the search bar will be output below the
               graph in a table, sorted by popularity at the time of the latest year selected in the year range.
               When you changing your search criteria, click the \"Update\" button to refresh the results."),
      
      # Create user input for sex
      selectInput("sex", h3("Select sex"),
                  choices = list("Female" = "F", "Male" = "M"),
                  selected = "F"),
    
      # Create user input for name (using selectize to allow autocomplete)
      selectizeInput("names", label = h3("Select name"),
                     choices = sort(unique(babynames[babynames$prop > 0.01, ]$name)),
                     selected = "Mary",
                     multiple = TRUE,
                     options = list(placeholder = "Select up to 8 names",
                                    create = TRUE,
                                    maxItems = 8)),
      
      dateRangeInput("year_range", label = h4("Year range"), start = "1880-01-01", end = "2015-01-01",
                     min = "1880-01-01", max = "2015-01-01", format = "yyyy",
                     startview = "decade",
                     separator = " to "),
      
      submitButton("Update"),
      
      helpText(a("View the code on GitHub", href = "https://github.com/dpmartin42/babynameR", target = "_blank"))
      
      ),
      
      # Create output plot in main panel
      mainPanel(
        plotOutput(outputId = "lineplot"),
        tabsetPanel(tabPanel("Similar Names", dataTableOutput("nametable")))
        )
    
  )
  
)
  
server <- function(input, output) {
  
  # Create a lineplot output the plotOutput function is expecting
  output$lineplot <- renderPlot({
    
    # Quick subset of data
    babynames_sub <- babynames %>% 
      filter(sex == input$sex,
             name %in% input$names,
             year >= year(input$year_range[1]) & year <= year(input$year_range[2]))
    
    # Output error if inputted names result in no data
    validate(
      need(nrow(babynames_sub) != 0,
      "No names that you entered appear in this dataset with the current filtering options. Please try again.")
      )
    
    # Create plot (if there is sufficient data)
    ggplot(data = babynames_sub, aes(x = year, y = prop, color = name)) +
      geom_line() +
      labs(x = "\nYear", y = "Proportion of Names\n", color = "Name") +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 18))

  })
  
  output$nametable <- renderDataTable({
    
    # Add validation if there is an empty string that is passed 
    validate(
      need(input$names[1] != "",
           "No name has been entered. Please try again.")
    )
    
    # Identify close string matched based on the first name input
    total_names <- unique(babynames$name)[agrep(input$names[1], unique(babynames$name), max = list(all = 1))]
    
    table_names <- babynames %>% 
      filter(name %in% total_names & name != input$names[1],
             year == year(input$year_range[2])) %>% 
      mutate(prop = round(prop, 4)) %>% 
      setNames(c("Year", "Sex", "Name", "Count", "Proportion of Names"))
  
    # Include validation in case name is unique and doesn't contain any close matches
    validate(
      need(nrow(table_names) != 0, 
           "I'm sorry, the first name you entered does not have any similar names in the dataset.")
    )
    
    table_names
    
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  
}

shinyApp(ui = ui, server = server)
