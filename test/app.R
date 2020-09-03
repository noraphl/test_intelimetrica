library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Overview by relevant variables"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the random distribution type ----
            radioButtons("gvar", "Select grouping variable:",
                         c("Country" = "country",
                           "Decade" = "decade",
                           "Month" = "month",
                           "Shape" = "shape",
                           "Year (1970-2014)" = "recent")),
            
            # br() element to introduce extra vertical spacing ----
            br()
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Table", tableOutput("table"))
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    var <- reactive({
        gvar <- switch(input$gvar,
                       country = by_country(),
                       decade = by_decade(),
                       month = by_month(),
                       shape = by_shape(),
                       recent = recent())
        
        
    })
    
    plot <- reactive({
        switch(input$gvar,
                        country = by_country_plot(),
                        decade = by_decade_plot(),
                        month = by_month_plot(),
                        shape = by_shape_plot(),
                        recent = recent_plot())
    })
    
    by_country <- reactive({
        #library(reshape2, quietly = T, verbose = F)
        #total
        ufo.data %>% group_by(country) %>%
            summarize(freq = n()) %>%
            arrange(desc(freq), na.rm = T) %>%
            mutate(prop = prop.table(freq)*100,
                   cumulative = cumsum(prop))
    })
    
    by_country_plot <- reactive({
        by_country() %>%
            filter(freq>300) %>%
            arrange(desc(freq), na.rm = T) %>%
            ggplot(aes(reorder(country, -freq), freq)) +
            geom_col(color = "white", fill= "#75AADB")
    })
    
    by_decade <- reactive({
        library(lubridate, quietly = T, verbose = F)
        ufo.data %>% 
            mutate(decade = year(floor_date(datetime, years(10)))) %>%
            group_by(decade) %>%
            summarise(freq = n()) %>%
            arrange(desc(freq)) %>%
            mutate(prop = prop.table(freq)*100,
                   cumulative = cumsum(prop)) 
    })
    by_decade_plot <- reactive({
        by_decade() %>%
            filter(freq>300) %>%
            arrange(desc(freq), na.rm = T) %>%
            ggplot(aes(reorder(decade, -freq), freq)) +
            geom_col(color = "white", fill= "#75AADB")
    })
    
    by_month <- reactive({
        ufo.data %>% 
            mutate(month = factor(months(datetime, abbreviate = F), ordered = T)) %>%
            group_by(month) %>%
            summarise(freq = n()) %>%
            arrange(match(month, month.name)) %>%
            mutate(prop = prop.table(freq)*100,
                   cumulative = cumsum(prop)) 
    })
    by_month_plot <- reactive({
        by_month() %>%
            mutate(month = factor(month, month)) %>%
            ggplot(aes(month, freq)) +
            #geom_bar(stat = "identity")
            geom_col(color = "white", fill= "#75AADB") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                             size=9))
    })
    
    by_shape <- reactive({
        ufo.data %>% 
            group_by(shape) %>%
            summarise(freq = n()) %>%
            arrange(desc(freq)) %>%
            mutate(prop = prop.table(freq)*100,
                   cumulative = cumsum(prop))
    })
    
    by_shape_plot <- reactive({
        by_shape() %>%
            mutate(shape = factor(shape, shape)) %>%
            ggplot(aes(shape, freq)) +
            #geom_bar(stat = "identity")
            geom_col(color = "white", fill= "#75AADB") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                             size=9))
    })
    
    recent <- reactive({
        ufo.data %>% 
            mutate(year = year(datetime)) %>%
            group_by(year) %>%
            filter(year >= 1970) %>%
            summarise(freq = n()) %>%
            mutate(prop = prop.table(freq)*100,
                   cumulative = cumsum(prop)) %>%
            arrange(desc(year)) 
    })
    
    recent_plot <- reactive({
        recent() %>%
            ggplot(aes(year, freq)) +
            #geom_bar(stat = "identity")
            geom_col(color = "white", fill= "#75AADB") 
    })
    
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$plot <- renderPlot({
        gvar <- input$gvar
        n <- input$n
        
        
        plot()
        
        #var() %>%
        #    ggplot(aes(country)) +
        #    geom_bar()
        
        #hist(var()$freq,
        #     main = paste("By", gvar, sep = ""),
        #     col = "#75AADB", border = "white")
    })
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
        summary(var())
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        var()
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)
