# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(scales)


# Load the data
uni <- read.csv("data/universities.csv", stringsAsFactors = FALSE)
health <- read.csv("data/health.csv", stringsAsFactors = FALSE)
schools <- read.csv("data/schools.csv", stringsAsFactors = FALSE)

# Feature 1: Using Shiny dashboard to structure my app. This allows me to create a structured app where I can have different pages.
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Post-Secondary School Data", tabName = "tab_uni"),
      menuItem("Healthcare Agency Data", tabName = "tab_health"),
      menuItem("Public School Data", tabName = "tab_schools")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("Welcome to the BC Public Sector Employee Salaries App"),
                mainPanel(
                  h4("App made by Irvin Ng (STAT545B UBC)"),
                  h3("About"),
                  p("This is a Shiny app that provides insights into the salaries of public sector employees in BC."),
                  p("The Vancouver Sun has collected the names and salaries of workers that earn a yearly salary of more than $75,000 in British Columbia."),
                  p("Please navigate to the tabs on the left to explore the data."),
                  img(src = "VancouverSun.png") # Feature 2: Adding an image to the UI. In this case, I added a screenshot of the Vancouver Sun Article. This 
                                                # provides the user with more context regarding the app.
                )
              )
      ),
      # Feature 3 (and technically 4?): Putting my interactive data table and plots in separate tabs. Here I have interactive tables and plots where you can 
      # filter by the public sector agency (eg. school, health care agency, etc.) or a salary threshold. This allows the user to engage with the tables and plots.
      tabItem(tabName = "tab_uni",
              fluidPage(
                titlePanel("Post-Secondary Schools Data"),
                tabsetPanel(
                  tabPanel("Table", 
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("filter_uni", "Filter by school:",
                                           choices = c("All", unique(uni$Agency))),
                               downloadButton("downloadCSV_uni", "Download CSV")
                             ),
                             mainPanel(
                               DT::dataTableOutput("table_uni"),
                               textOutput("result_count_uni")
                             )
                           )
                  ),
                  tabPanel("Plots",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Select Plot"),
                               radioButtons("plot_type_uni", "Choose a plot:",
                                            choices = c("Average Salary", "Total Salary", "Total Employees")),
                               sliderInput("salary_threshold_uni", "Select Salary Threshold:",
                                           min = 0, max = max(uni$Remuneration, na.rm = TRUE), value = 75000)
                             ),
                             mainPanel(
                               plotOutput("selected_plot_uni")
                             )
                           )
                  )
                )
              )
            ),
      tabItem(tabName = "tab_health",
              fluidPage(
                titlePanel("Healthcare Agencies Data"),
                tabsetPanel(
                  tabPanel("Table", 
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("filter_health", "Filter by Healthcare Agency:",
                                           choices = c("All", unique(health$Agency))),
                               downloadButton("downloadCSV_health", "Download CSV")
                             ),
                             mainPanel(
                               DT::dataTableOutput("table_health"),
                               textOutput("result_count_health")
                             )
                           )
                  ),
                  tabPanel("Plots",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Select Plot"),
                               radioButtons("plot_type_health", "Choose a plot:",
                                            choices = c("Average Salary", "Total Salary", "Total Employees")),
                               sliderInput("salary_threshold_health", "Select Salary Threshold:",
                                           min = 0, max = max(health$Remuneration, na.rm = TRUE), value = 75000)
                             ),
                             mainPanel(
                               plotOutput("selected_plot_health")
                             )
                           )
                  )
                )
              )
      ),
      tabItem(tabName = "tab_schools",
              fluidPage(
                titlePanel("School Districts Data"),
                tabsetPanel(
                  tabPanel("Table", 
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("filter_schools", "Filter by School District:",
                                           choices = c("All", unique(schools$Agency))),
                               downloadButton("downloadCSV_schools", "Download CSV")
                             ),
                             mainPanel(
                               DT::dataTableOutput("table_schools"),
                               textOutput("result_count_schools")
                             )
                           )
                  ),
                  tabPanel("Plots",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Select Plot"),
                               radioButtons("plot_type_schools", "Choose a plot:",
                                            choices = c("Average Salary", "Total Salary", "Total Employees")),
                               sliderInput("salary_threshold_schools", "Select Salary Threshold:",
                                           min = 0, max = max(schools$Remuneration, na.rm = TRUE), value = 75000)
                             ),
                             mainPanel(
                               plotOutput("selected_plot_schools")
                             )
                           )
                  )
                )
              )
      )
      )
    )
  )

  
  server <- function(input, output, session) {
    
    ######### Universities ##########
    # Data summary function
    agency_summary_uni <- function(threshold_uni, filtered_data_uni) {
      filtered_data_uni <- filtered_data_uni()
      
      # Filter data based on the threshold
      filtered_data_uni <- subset(filtered_data_uni, Remuneration > threshold_uni)
      
      # Calculate total salary
      total_salary_uni <- aggregate(Remuneration ~ Agency, data = filtered_data_uni, FUN = sum, na.rm = TRUE)
      
      # Calculate total employees
      total_employees_uni <- aggregate(Remuneration ~ Agency, data = filtered_data_uni, FUN = length)
      
      # Merge total salary and total employees by Agency
      summarized_data_uni <- merge(total_salary_uni, total_employees_uni, by = "Agency")
      
      # Calculate average salary
      summarized_data_uni$AverageSalary <- summarized_data_uni$Remuneration.x / summarized_data_uni$Remuneration.y
      
      # Rename columns
      colnames(summarized_data_uni) <- c("Agency", "TotalSalary", "TotalEmployees", "AverageSalary")
      
      return(summarized_data_uni)
    }
    
    # Filtered data observer
    filtered_data_uni <- reactive({
      filtered_uni <- uni
      
      # Filter by selected Agency
      if (input$filter_uni != "All") {
        filtered_uni <- subset(filtered_uni, Agency == input$filter_uni)
      }
      
      return(filtered_uni)
    })
    
    # Observer for table rendering
    observe({
      output$table_uni <- DT::renderDataTable({
        DT::datatable(filtered_data_uni(), options = list(pageLength = 25))
      })
    })
    
    # Observer for result count
    observe({
      output$result_count_uni <- renderText({
        paste(nrow(filtered_data_uni()), " results found")
      })
    })
    
    # Observer for downloading CSV
    output$downloadCSV_uni <- downloadHandler(
      filename = function() {
        paste("filtered_data_uni.csv")
      },
      content = function(file) {
        filtered_uni <- filtered_data_uni()
        write.csv(filtered_uni, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    observe({
      req(input$downloadCSV_uni)
    })
    
    # Observer for selected plot
    observe({
      output$selected_plot_uni <- renderPlot({
        req(input$plot_type_uni)
        threshold_uni <- input$salary_threshold_uni
        
        if (input$plot_type_uni == "Average Salary") {
          ggplot(data = agency_summary_uni(threshold_uni, filtered_data_uni), aes(x = Agency, y = AverageSalary)) + 
            geom_bar(stat = "identity") + 
            labs(x = "Post-Secondary School", y = "Average Salary", 
                 title = "Average Salary by School", 
                 subtitle = paste("From employees that make above", scales::dollar(threshold_uni))) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
        } else if (input$plot_type_uni == "Total Salary") {
          ggplot(data = agency_summary_uni(threshold_uni, filtered_data_uni), aes(x = Agency, y = TotalSalary)) +
            geom_bar(stat = "identity") + 
            labs(x = "Post-Secondary School", y = "Total Salary", 
                 title = "Total Salary by School", 
                 subtitle = paste("From employees that make above", scales::dollar(threshold_uni))) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
        } else if (input$plot_type_uni == "Total Employees") {
          ggplot(data = agency_summary_uni(threshold_uni, filtered_data_uni), aes(x = Agency, y = TotalEmployees)) +
            geom_bar(stat = "identity") + 
            labs(x = "Post-Secondary School", y = "Total Employees",
                 title = "Number of employees that make above certain salary") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      })
    })

    ######### Healthcare Agencies ##########
    
    # Data summary function
    agency_summary_health <- function(threshold_health, filtered_data_health) {
      filtered_data_health <- filtered_data_health()
      
      # Filter data based on the threshold
      filtered_data_health <- subset(filtered_data_health, Remuneration > threshold_health)
      
      # Calculate total salary
      total_salary_health <- aggregate(Remuneration ~ Agency, data = filtered_data_health, FUN = sum, na.rm = TRUE)
      
      # Calculate total employees
      total_employees_health <- aggregate(Remuneration ~ Agency, data = filtered_data_health, FUN = length)
      
      # Merge total salary and total employees by Agency
      summarized_data_health <- merge(total_salary_health, total_employees_health, by = "Agency")
      
      # Calculate average salary
      summarized_data_health$AverageSalary <- summarized_data_health$Remuneration.x / summarized_data_health$Remuneration.y
      
      # Rename columns
      colnames(summarized_data_health) <- c("Agency", "TotalSalary", "TotalEmployees", "AverageSalary")
      
      return(summarized_data_health)
    }
    
    # Filtered data observer
    filtered_data_health <- reactive({
      filtered_health <- health
      
      # Filter by selected Agency
      if (input$filter_health != "All") {
        filtered_health <- subset(filtered_health, Agency == input$filter_health)
      }
      
      return(filtered_health)
    })
    
    # Observer for table rendering
    observe({
      output$table_health <- DT::renderDataTable({
        DT::datatable(filtered_data_health(), options = list(pageLength = 25))
      })
    })
    
    # Observer for result count
    observe({
      output$result_count_health <- renderText({
        paste(nrow(filtered_data_health()), " results found")
      })
    })
    
    # Observer for downloading CSV
    output$downloadCSV_health <- downloadHandler(
      filename = function() {
        paste("filtered_data_health.csv")
      },
      content = function(file) {
        filtered_health <- filtered_data_health()
        write.csv(filtered_health, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    observe({
      req(input$downloadCSV_health)
    })
    
    # Observer for selected plot
    observe({
      output$selected_plot_health <- renderPlot({
        req(input$plot_type_health)
        threshold_health <- input$salary_threshold_health
        
        if (input$plot_type_health == "Average Salary") {
          ggplot(data = agency_summary_health(threshold_health, filtered_data_health), aes(x = Agency, y = AverageSalary)) + 
            geom_bar(stat = "identity") + 
            labs(x = "Healthcare Agency", y = "Average Salary", 
                 title = "Average Salary by Healthcare Agency", 
                 subtitle = paste("From employees that make above", scales::dollar(threshold_health))) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
        } else if (input$plot_type_health == "Total Salary") {
          ggplot(data = agency_summary_health(threshold_health, filtered_data_health), aes(x = Agency, y = TotalSalary)) +
            geom_bar(stat = "identity") + 
            labs(x = "Healthcare Agency", y = "Total Salary", 
                 title = "Total Salary by Healthcare Agency", 
                 subtitle = paste("From employees that make above", scales::dollar(threshold_health))) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
        } else if (input$plot_type_health == "Total Employees") {
          ggplot(data = agency_summary_health(threshold_health, filtered_data_health), aes(x = Agency, y = TotalEmployees)) +
            geom_bar(stat = "identity") + 
            labs(x = "Healthcare Agency", y = "Total Employees",
                 title = "Number of employees that make above certain salary") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      })
    })
    
    ######### Public Schools ##########
    
    # Data summary function
    agency_summary_schools <- function(threshold_schools, filtered_data_schools) {
      filtered_data_schools <- filtered_data_schools()
      
      # Filter data based on the threshold
      filtered_data_schools <- subset(filtered_data_schools, Remuneration > threshold_schools)
      
      # Calculate total salary
      total_salary_schools <- aggregate(Remuneration ~ Agency, data = filtered_data_schools, FUN = sum, na.rm = TRUE)
      
      # Calculate total employees
      total_employees_schools <- aggregate(Remuneration ~ Agency, data = filtered_data_schools, FUN = length)
      
      # Merge total salary and total employees by Agency
      summarized_data_schools <- merge(total_salary_schools, total_employees_schools, by = "Agency")
      
      # Calculate average salary
      summarized_data_schools$AverageSalary <- summarized_data_schools$Remuneration.x / summarized_data_schools$Remuneration.y
      
      # Rename columns
      colnames(summarized_data_schools) <- c("Agency", "TotalSalary", "TotalEmployees", "AverageSalary")
      
      return(summarized_data_schools)
    }
    
    # Filtered data observer
    filtered_data_schools <- reactive({
      filtered_schools <- schools
      
      # Filter by selected Agency
      if (input$filter_schools != "All") {
        filtered_schools <- subset(filtered_schools, Agency == input$filter_schools)
      }
      
      return(filtered_schools)
    })
    
    # Observer for table rendering
    observe({
      output$table_schools <- DT::renderDataTable({
        DT::datatable(filtered_data_schools(), options = list(pageLength = 25))
      })
    })
    
    # Observer for result count
    observe({
      output$result_count_schools <- renderText({
        paste(nrow(filtered_data_schools()), " results found")
      })
    })
    
    # Observer for downloading CSV
    output$downloadCSV_schools <- downloadHandler(
      filename = function() {
        paste("filtered_data_schools.csv")
      },
      content = function(file) {
        filtered_schools <- filtered_data_schools()
        write.csv(filtered_schools, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    observe({
      req(input$downloadCSV_schools)
    })
    
    # Observer for selected plot
    observe({
      output$selected_plot_schools <- renderPlot({
        req(input$plot_type_schools)
        threshold_schools <- input$salary_threshold_schools
        
        if (input$plot_type_schools == "Average Salary") {
          ggplot(data = agency_summary_schools(threshold_schools, filtered_data_schools), aes(x = Agency, y = AverageSalary)) + 
            geom_bar(stat = "identity") + 
            labs(x = "School District", y = "Average Salary", 
                 title = "Average Salary by School District", 
                 subtitle = paste("From employees that make above", scales::dollar(threshold_schools))) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
        } else if (input$plot_type_schools == "Total Salary") {
          ggplot(data = agency_summary_schools(threshold_schools, filtered_data_schools), aes(x = Agency, y = TotalSalary)) +
            geom_bar(stat = "identity") + 
            labs(x = "School District", y = "Total Salary", 
                 title = "Total Salary by School District", 
                 subtitle = paste("From employees that make above", scales::dollar(threshold_schools))) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
        } else if (input$plot_type_schools == "Total Employees") {
          ggplot(data = agency_summary_schools(threshold_schools, filtered_data_schools), aes(x = Agency, y = TotalEmployees)) +
            geom_bar(stat = "identity") + 
            labs(x = "School District", y = "Total Employees",
                 title = "Number of employees that make above certain salary") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      })
    })
    
}
  

# Run the application
  shinyApp(ui = ui, server = server)
