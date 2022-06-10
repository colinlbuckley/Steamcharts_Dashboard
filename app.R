library(tidyverse)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(DT)
library(ggthemes)
library(thematic)

# Import Data
source("Cleaning.R")

# Define UI
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Top Steam Games"),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("> Dashboard", tabName = "dashboard"),
            menuItem("> Raw Data Explorer", tabName = "rawdata")
        ),
        
        selectizeInput(
            inputId = "gameInput",
            label = "Game[s] (select up to 8):",
            choices = unique(ts_cleaned$Name),
            multiple = TRUE,
            selected = unique(ts_cleaned$Name)[1:3],
            options = list(maxItems = 8)
        ),
        
        selectInput(
            inputId = "dv",
            label = "Player Count Metric",
            choices = c("Average", "Peak"),
            selected = "Average"
        ),
        
        dateRangeInput(
            inputId = "daterange",
            label = "Adjust date range:",
            start = min(ts_cleaned$Date),
            end = max(ts_cleaned$Date),
            min = min(ts_cleaned$Date),
            max = max(ts_cleaned$Date)
        )
        

        
    ),
    
    dashboardBody(
        
        shinyDashboardThemes(theme = "grey_dark"),
        tabItems(
            tabItem("dashboard",
                    fillPage(fluidRow(
                        box(
                            width = 9,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Monthly Player Count by Game",
                            footer = "Source: https://steamcharts.com/",
                            plotlyOutput("comparePlot")
                        ),
                        box(
                            width = 3,
                            height = "100%",
                            status = "info",
                            title = "Top Games by Player Count",
                            tableOutput(outputId = "rankTable"),
                            style = 'height:500px;overflow-y: scroll;'
                        )
                    ))), 
        tabItem("rawdata",
                box(DTOutput(outputId = "rawDataTable"),
                    width = 12, height="500", style="overflow-y: scroll;")
        )
    ))
)


server <- function(input, output) {
    
    source("Cleaning.R")
    
    output$comparePlot <- renderPlotly({
        
        validate(
            need(input$gameInput, "Please select at least one game.")
        )
        
        plot_y_name <- paste0(input$dv, "_Players")
        
        p1 <- ts_cleaned %>%
            filter(Name %in% input$gameInput) %>%
            filter(between(as.Date(Date), input$daterange[1], input$daterange[2])) %>%
            rename(plot_y = plot_y_name) %>%
            ggplot() +
            geom_line(aes(
                x = Date,
                y = plot_y,
                color = Name
            )) +
            scale_x_datetime(
                date_labels = "%b %Y",
                date_breaks = "3 month",
                date_minor_breaks = "month"
            ) +
            labs(
                x = "Date",
                y = paste(input$dv, "Players"),
                color = "Name:"
            ) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1),
                  text = element_text(family = "Arial"))
        
        plotly1 <-
            ggplotly(p1) %>% layout(legend = list(orientation = 'h',
                                                  y = -0.3))
        plotly1
    })
    
    output$rankTable <- renderTable({
        ranked <- ts_cleaned %>%
            group_by(Rank, Name) %>%
            summarise()
        
        ranked
    },
    striped = TRUE,
    align = "cl",
    spacing = "xs",
    digits = 0)
    
    output$rawDataTable <- renderDT({
        ts_cleaned %>%
            filter(Name %in% input$gameInput) %>%
            filter(between(as.Date(Date), input$daterange[1], input$daterange[2])) %>%
            select(1:3, paste0(input$dv, "_Players"), ends_with(input$dv))
    },
    rownames = FALSE)
}

# # Global ggplot theme with {thematic}
ggplot2::theme_set(ggthemes::theme_solarized())
thematic_shiny(font = "auto")

# Run the application
shinyApp(ui = ui, server = server)
