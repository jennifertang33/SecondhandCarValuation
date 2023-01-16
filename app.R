library(shinydashboard)
library(RColorBrewer)
library(randomForest)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = "Car-Dollar", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("car")),
      menuItem("Data Visualization", tabName = "dataviz", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("Price Valuation", tabName = "pricepredict", icon = icon("comment-dollar"))
    )
  ),
  dashboardBody(

    tabItems(
      
      # First tab content
      tabItem(tabName = "welcome", h2("Welcome to Car-Dollar"),
        br(),
        imageOutput("car_pic"),
        div(style = "margin-top: -80px"),
        h4(strong("Project Description")),
        p(style="text-align: justify; font-size = 40px",
          "Car-Dollar is a R Shiny application created for second-hand car price valuation.
           It displays interactive data visualization of the average car price versus brand, model, year and fuel type.
           Also, it provides car price valuation service supported by Random Forest Regressor,
           one of the remarkable machine learning algorithms."),
        p(style="text-align: justify; font-size = 40px",
          "The dataset contains 100,000 UK used cars information of brand, model, price, transmission, mileage, year,
           fuel type, road tax, miles per gallon (mpg) and engine size. Please visit",
           a(href = "https://www.kaggle.com/datasets/adityadesai13/used-car-dataset-ford-and-mercedes", "Used-car Dataset"),
           "for more details."),
        br(),
        p(style="text-align: justify; font-size = 40px",
          "Jennifer Tang", br(), "January, 2023")
      ),
      
      # Second tab content
      tabItem(tabName = "dataviz", h2("Data Visualization"),
        fluidRow(
          box(plotlyOutput("plot1"), title="Average Price vs Brand", width=4, solidHeader=TRUE, status="primary"),
          box(plotlyOutput("plot2"),title="Average Price vs Manufactured Year", width=4, solidHeader=TRUE, status="warning"),
          box(plotlyOutput("plot3"), title="Average Price vs Fuel Type", width=4, solidHeader=TRUE, status="success")
          ),
        fluidRow(
          box(plotlyOutput("plot4"), title="Average Price vs Model", width=9, solidHeader = TRUE, status="danger"),
          box(uiOutput("select_bar"), width=3)
        )
      ),
             
      # Third tab content
      tabItem(tabName = "datatable", h2("Data Table"),
        fluidRow(
          box(DT::dataTableOutput('page3'), width=12))
      ),
      
      # Forth tab content
      tabItem(tabName = "pricepredict", h2("Price Valuation"),
        fluidRow(infoBoxOutput("prediction")),
        fluidRow(box(uiOutput("select_brand"), width=4),
                 box(uiOutput("select_model"), width=4),
                 box(uiOutput("select_year"), width=4)),
        fluidRow(box(uiOutput("select_transmission"), width=4),
                 box(uiOutput("select_fueltype"), width=4),
                 box(uiOutput("slider_tax"), width=4)),
        fluidRow(box(uiOutput("slider_mileage"), width=4),
                 box(uiOutput("slider_mpg"), width=4),
                 box(uiOutput("slider_enginesize"), width=4))
      )

    ) #close for tabItems
  ) #close for dashboardBody
) #close for ui


server <- function(input, output) {
  
  ### tab1 - Welcome
  output$car_pic <- renderImage({
    list(src = "usedcar.jpg", width="465", height="279")}, deleteFile = F)
  
  ## Data Visualization
  data1<-read.csv('cleaned_data.csv')
  
  ### tab2 - plot 1
  df_avg_brand <- data1 %>%
    group_by(brand) %>% 
    summarise(avg_price=mean(price)) %>% as.data.frame()

  output$plot1 <- renderPlotly({
    cols <- brewer.pal(9, "GnBu")
    plot_ly(df_avg_brand, x=~avg_price, y=~brand, type='bar', orientation='h',
            marker = list(color=cols),
            hoverinfo='text',
            textposition='none',
            text = ~paste('</br> Brand: ', brand, 
                          '</br> Average Price: ', round(avg_price, digits=0))) %>%
      layout(xaxis=list(title="Brand"), yaxis=list(title="Average Price"), bargap=0.8)})
  
  ### tab2 - plot 2
  df_avg_year <- data1 %>%
    group_by(year) %>% 
    summarise(avg_price=mean(price)) %>% as.data.frame()
  
  output$plot2 <- renderPlotly({
    plot_ly(df_avg_year, x=~year, y=~avg_price, type='scatter', mode='lines+markers',
            marker = list(color='#8B4513'),
            line=list(color='#F4A460'),
            hoverinfo='text',
            text = ~paste('</br> Year: ', year, 
                          '</br> Average Price: ', round(avg_price, digits=0))) %>%
    layout(xaxis=list(title="Year"), yaxis=list(title="Average Price"))})
  
  ### tab2 - plot 3
  df_avg_fuelType <- data1 %>%
    group_by(fuelType) %>% 
    summarise(avg_price=mean(price)) %>% as.data.frame()
  
  output$plot3 <- renderPlotly({
    colors <- c('#556B2F', '#008B8B', '#6B8E23', '#8FBC8B', '#CD853F')
    plot_ly(df_avg_fuelType, labels = ~fuelType, values = ~avg_price, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('average price $', round(avg_price, digits=0)),
            marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE)})

  ### tab2 - plot 4
  output$plot4 <- renderPlotly({
    df_avg_model <- data1 %>%
      filter(brand %in% input$select_bar) %>%
      group_by(model) %>% 
      summarise(avg_price=mean(price)) %>% as.data.frame()

    plot_ly(df_avg_model, x=~model, y=~avg_price, type='bar', color=~model, 
            hoverinfo = 'text',
            text = ~paste('</br> Model: ', model, 
                          '</br> Average Price: ', round(avg_price, digits=0))) %>%
            layout(title=~paste(toupper(input$select_bar)), xaxis=list(title="Model"), yaxis=list(title="Average Price"))})

  ### tab2 - select bar  
  output$select_bar <- renderUI({
    selectInput("select_bar",
                label=("you can select the following brands"),
                choices = unique(data1$brand))})
  
  ### tab3 - Data Table
  options(DT.options = list(pageLength = 20))
  output$page3 = DT::renderDataTable(
    datatable(data1,
      options = list(scrollX=TRUE, columnDefs=list(list(className='dt-center', targets="_all")))))
  
  ## Machine Learning
  ### tab4 - select and slider
  output$select_brand <- renderUI({
    selectInput("select_brand", label="Brand", unique(data1$brand))})
  
  output$select_model <- renderUI({
    data2 <- data1 %>% filter(brand %in% input$select_brand)
    selectInput(inputId='select_model', label='Model', unique(data2$model))})
  
  output$select_year <- renderUI({
    selectInput(inputId='select_year', label='Year', unique(data1$year))})
  
  output$select_transmission <- renderUI({
    selectInput(inputId='select_transmission', label='Transmission', unique(data1$transmission))})
  
  output$select_fueltype <- renderUI({
    selectInput(inputId='select_fueltype', label='Fuel Type', unique(data1$fuelType))})
  
  output$slider_tax <- renderUI({
    sliderInput("slider_tax", "Tax", value = 240, step = 5,
                min = min(data1$tax, na.rm = TRUE), 
                max = max(data1$tax, na.rm = TRUE))})
  
  output$slider_mileage <- renderUI({
    sliderInput("slider_mileage", "Mileage", value = 100000, step = 1000,
                min = min(data1$mileage, na.rm = TRUE), 
                max = max(data1$mileage, na.rm = TRUE))})
  
  output$slider_mpg <- renderUI({
    sliderInput("slider_mpg", "MPG", value = 39.5, step = 0.1,
                min = min(data1$mpg, na.rm = TRUE), 
                max = max(data1$mpg, na.rm = TRUE))})
  
  output$slider_enginesize <-renderUI({
    sliderInput("slider_enginesize", "Engine Size", value = 1.5, step = 0.1,
                min = min(data1$engineSize, na.rm = TRUE), 
                max = max(data1$engineSize, na.rm = TRUE))})
  
  ### tab4 - load model and predict for user's input
  rf_model <- readRDS("rf_model.rds")
  
  userinputdata <- reactive({
    req(input$select_brand)
    req(input$select_model)
    req(input$select_year)
    req(input$select_transmission)
    req(input$select_fueltype)
    req(input$slider_tax)
    req(input$slider_mileage)
    req(input$slider_mpg)
    req(input$slider_enginesize)
    data.frame(brand=as.character(input$select_brand),
               model=as.character(input$select_model),
               year=as.integer(input$select_year),
               transmission=as.character(input$select_transmission),
               fuelType=as.character(input$select_fueltype),
               tax=as.integer(input$slider_tax),
               mileage=as.integer(input$slider_mileage),
               mpg=as.numeric(input$slider_mpg),
               engineSize=as.numeric(input$slider_enginesize)
    )
  })
  
  pred <- reactive({
    estimated_price <- predict(rf_model, userinputdata())
    result <- round(estimated_price, digits=0)
    result
  })
  
  output$prediction <- renderInfoBox({
    infoBox(
      "Estimated Price",
      renderText(paste0("£ ",pred())),
      icon = icon("car-side", lib = "font-awesome"),
      color = "yellow"
    )
  })
  

}

shinyApp(ui, server)