# ==============================================
# BMW Dashboard v3.1 (Fixed Dynamic Filters)
# Path: /Volumes/NOMAN/PGD-DSBA/projects/BMW_Dashboard/app.R
# ==============================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(readr)
library(shinycssloaders)
library(RColorBrewer)
library(rsconnect)
# ---- Configuration ----
csv_file <- file.path("data", "BMW sales data (2010-2024) (1).csv")
logo_dir  <- file.path( "www", "bmw_logo.png")


# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "bmw_logo.png", height = "36px", style = "vertical-align:middle; margin-right:8px;"),
      tags$span("BMW Sales Dashboard", style = "font-weight:700;")
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(id = "tabs",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
                menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
                menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    hr(),
    # dynamic filter UIs
    uiOutput("ui_year"),
    uiOutput("ui_region"),
    uiOutput("ui_fuel"),
    uiOutput("ui_transmission"),
    uiOutput("ui_classification"),
    uiOutput("ui_model"),
    br(),
    sliderInput("top_n", "Top N models to show:", min = 3, max = 20, value = 10),
    actionButton("reset_filters", "🔄 Reset filters", class = "btn-warning"),
    br(), br(),
    downloadButton("download_filtered", "📥 Export Filtered CSV", style = "width:100%")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body { background: #f7f8fb; }
      .banner { text-align:center; background: linear-gradient(90deg,#0a1a2f,#1D428A); padding:12px; border-radius:6px; margin:10px 12px; color: #fff; }
      .kpi-box { background:#fff; border-radius:8px; padding:18px; box-shadow:0 2px 6px rgba(0,0,0,0.06); text-align:center; }
      .kpi-title { font-size:13px; color:#666; margin-bottom:6px; }
      .kpi-value { font-size:22px; font-weight:700; color:#1D428A; }
      .content-wrapper { padding: 10px 14px; }
    "))),
    div(class = "banner",
        tags$img(src = "bmw_logo.png", height = "64px", style = "vertical-align:middle; margin-right:10px;"),
        tags$span(style = "font-size:20px; font-weight:700;", "BMW Sales Dashboard 2010–2024")
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4,
                       div(class = "kpi-box",
                           div(class = "kpi-title", "Total Sales (filtered)"),
                           div(class = "kpi-value", textOutput("total_sales")),
                           div(style="font-size:12px;color:#888;margin-top:6px;", "Sum of Sales_Volume")
                       )
                ),
                column(width = 4,
                       div(class = "kpi-box",
                           div(class = "kpi-title", "Average Price (USD)"),
                           div(class = "kpi-value", textOutput("avg_price")),
                           div(style="font-size:12px;color:#888;margin-top:6px;", "Mean of Price_USD")
                       )
                ),
                column(width = 4,
                       div(class = "kpi-box",
                           div(class = "kpi-title", "Top Selling Model"),
                           div(class = "kpi-value", textOutput("top_model")),
                           div(style="font-size:12px;color:#888;margin-top:6px;", "Model with highest total sales")
                       )
                )
              ),
              br(),
              fluidRow(
                box(width = 6, title = "Top Models", status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("top_models_plot", height = "420px"), type = 6)),
                box(width = 6, title = "Fuel Type Share", status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("fuel_plot", height = "420px"), type = 6))
              ),
              br(),
              fluidRow(
                box(width = 12, title = "Sales by Region", status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("region_plot", height = "420px"), type = 6))
              )
      ),
      
      tabItem(tabName = "trends",
              fluidRow(
                box(width = 12, title = "Sales Trend (Top N Models)", status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("trend_plot", height = "560px"), type = 6))
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Filtered Data", status = "primary", solidHeader = TRUE,
                    DTOutput("data_table"))
              ),
              fluidRow(
                box(width = 12, title = "Export / Save", status = "info", solidHeader = TRUE,
                    p("Use the table buttons (Copy / CSV / Excel / PDF / Print) to export the filtered view."),
                    actionButton("download_csv", "Download CSV", icon = icon("file-csv")),
                    downloadButton("download_filtered", "Download filtered CSV", style = "margin-left:10px;")
                )
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Load and clean data ----
  req(file.exists(csv_file))
  df_base <- read_csv(csv_file, show_col_types = FALSE) %>%
    mutate(
      Model = as.character(Model),
      Region = as.character(Region),
      Color = as.character(Color),
      Fuel_Type = ifelse(is.na(Fuel_Type) | Fuel_Type == "", "Unknown", as.character(Fuel_Type)),
      Transmission = as.character(Transmission),
      Sales_Classification = as.character(Sales_Classification),
      Year = as.integer(Year),
      Mileage_KM = as.numeric(gsub("[^0-9.-]", "", as.character(Mileage_KM))),
      Price_USD = as.numeric(gsub("[^0-9.-]", "", as.character(Price_USD))),
      Sales_Volume = as.numeric(gsub("[^0-9.-]", "", as.character(Sales_Volume)))
    )
  
  # ---- Reactive available choices (dynamic dropdowns) ----
  available_choices <- reactive({
    df <- df_base
    if (!is.null(input$year) && input$year != "All") df <- df %>% filter(Year == as.integer(input$year))
    if (!is.null(input$region) && input$region != "All") df <- df %>% filter(Region == input$region)
    if (!is.null(input$fuel) && input$fuel != "All") df <- df %>% filter(Fuel_Type == input$fuel)
    if (!is.null(input$transmission) && input$transmission != "All") df <- df %>% filter(Transmission == input$transmission)
    if (!is.null(input$classification) && input$classification != "All") df <- df %>% filter(Sales_Classification == input$classification)
    if (!is.null(input$model) && input$model != "All") df <- df %>% filter(Model == input$model)
    
    list(
      year = c("All", sort(unique(df_base$Year))),
      region = c("All", sort(unique(df$Region))),
      fuel = c("All", sort(unique(df$Fuel_Type))),
      transmission = c("All", sort(unique(df$Transmission))),
      classification = c("All", sort(unique(df$Sales_Classification))),
      model = c("All", sort(unique(df$Model)))
    )
  })
  
  # ---- Dynamic dropdowns ----
  output$ui_year <- renderUI({
    choices <- available_choices()$year
    sel <- if (!is.null(input$year) && input$year %in% choices) input$year else "All"
    selectInput("year", "Year", choices = choices, selected = sel, selectize = TRUE)
  })
  
  output$ui_region <- renderUI({
    choices <- available_choices()$region
    sel <- if (!is.null(input$region) && input$region %in% choices) input$region else "All"
    selectInput("region", "Region", choices = choices, selected = sel, selectize = TRUE)
  })
  
  output$ui_fuel <- renderUI({
    choices <- available_choices()$fuel
    sel <- if (!is.null(input$fuel) && input$fuel %in% choices) input$fuel else "All"
    selectInput("fuel", "Fuel Type", choices = choices, selected = sel, selectize = TRUE)
  })
  
  output$ui_transmission <- renderUI({
    choices <- available_choices()$transmission
    sel <- if (!is.null(input$transmission) && input$transmission %in% choices) input$transmission else "All"
    selectInput("transmission", "Transmission", choices = choices, selected = sel, selectize = TRUE)
  })
  
  output$ui_classification <- renderUI({
    choices <- available_choices()$classification
    sel <- if (!is.null(input$classification) && input$classification %in% choices) input$classification else "All"
    selectInput("classification", "Sales Classification", choices = choices, selected = sel, selectize = TRUE)
  })
  
  output$ui_model <- renderUI({
    choices <- available_choices()$model
    sel <- if (!is.null(input$model) && input$model %in% choices) input$model else "All"
    selectInput("model", "Model", choices = choices, selected = sel, selectize = TRUE)
  })
  
  # ---- Filtered dataset ----
  filtered <- reactive({
    df <- df_base
    if (!is.null(input$year) && input$year != "All") df <- df %>% filter(Year == as.integer(input$year))
    if (!is.null(input$region) && input$region != "All") df <- df %>% filter(Region == input$region)
    if (!is.null(input$fuel) && input$fuel != "All") df <- df %>% filter(Fuel_Type == input$fuel)
    if (!is.null(input$transmission) && input$transmission != "All") df <- df %>% filter(Transmission == input$transmission)
    if (!is.null(input$classification) && input$classification != "All") df <- df %>% filter(Sales_Classification == input$classification)
    if (!is.null(input$model) && input$model != "All") df <- df %>% filter(Model == input$model)
    df
  })
  
  # ---- KPIs ----
  output$total_sales <- renderText({
    total <- sum(filtered()$Sales_Volume, na.rm = TRUE)
    formatC(ifelse(is.na(total), 0, total), format = "d", big.mark = ",")
  })
  
  output$avg_price <- renderText({
    avg <- mean(filtered()$Price_USD, na.rm = TRUE)
    if (is.na(avg)) return("-")
    paste0("$", formatC(round(avg, 2), format = "f", big.mark = ","))
  })
  
  output$top_model <- renderText({
    by_model <- filtered() %>%
      group_by(Model) %>%
      summarise(Total = sum(Sales_Volume, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total))
    if (nrow(by_model) == 0) return("-")
    paste0(by_model$Model[1], " (", formatC(by_model$Total[1], format = "d", big.mark = ","), ")")
  })
  
  # ---- Plots ----
  output$top_models_plot <- renderPlotly({
    df <- filtered() %>%
      group_by(Model) %>%
      summarise(Total_Sales = sum(Sales_Volume, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Sales)) %>%
      slice_head(n = input$top_n)
    
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, x = ~Total_Sales, y = ~reorder(Model, Total_Sales),
            type = "bar", orientation = "h", marker = list(color = "#1C69D4"),
            text = ~paste0(formatC(Total_Sales, big.mark = ","))) %>%
      layout(title = paste("Top", input$top_n, "Models (filtered)"),
             xaxis = list(title = "Total Sales"),
             yaxis = list(title = "Model"))
  })
  
  output$fuel_plot <- renderPlotly({
    df <- filtered() %>%
      group_by(Fuel_Type) %>%
      summarise(Total_Sales = sum(Sales_Volume, na.rm = TRUE), .groups = "drop")
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, labels = ~Fuel_Type, values = ~Total_Sales, type = "pie",
            textinfo = "label+percent", hoverinfo = "label+value") %>%
      layout(title = "Fuel Type Share (filtered)")
  })
  
  output$region_plot <- renderPlotly({
    df <- filtered() %>%
      group_by(Region) %>%
      summarise(Total_Sales = sum(Sales_Volume, na.rm = TRUE), .groups = "drop")
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, x = ~Total_Sales, y = ~reorder(Region, Total_Sales),
            type = "bar", orientation = "h", marker = list(color = "#60AFFF")) %>%
      layout(title = "Sales by Region (filtered)",
             xaxis = list(title = "Total Sales"), yaxis = list(title = "Region"))
  })
  
  output$trend_plot <- renderPlotly({
    df <- filtered()
    top_models <- df %>%
      group_by(Model) %>%
      summarise(Total = sum(Sales_Volume, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice_head(n = input$top_n)
    trend_df <- df %>%
      filter(Model %in% top_models$Model) %>%
      group_by(Model, Year) %>%
      summarise(Yearly_Sales = sum(Sales_Volume, na.rm = TRUE), .groups = "drop")
    if (nrow(trend_df) == 0) return(plotly_empty())
    
    fig <- plot_ly()
    models <- unique(trend_df$Model)
    cols <- brewer.pal(min(max(3, length(models)), 12), "Set2")
    for (i in seq_along(models)) {
      m <- models[i]
      dfm <- trend_df %>% filter(Model == m)
      fig <- add_trace(fig, x = dfm$Year, y = dfm$Yearly_Sales,
                       type = "scatter", mode = "lines+markers", name = m,
                       line = list(color = cols[(i-1) %% length(cols) + 1], width = 2))
    }
    fig %>% layout(title = "Sales Trend (Top Models, filtered)",
                   xaxis = list(title = "Year"), yaxis = list(title = "Sales Volume"))
  })
  
  # ---- Data Table ----
  output$data_table <- renderDT({
    datatable(filtered(), extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             pageLength = 15, scrollX = TRUE))
  })
  
  # ---- CSV Download ----
  output$download_filtered <- downloadHandler(
    filename = function() paste0("BMW_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(filtered(), file)
    }
  )
  
  # ---- Reset Filters ----
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "year", selected = "All")
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "fuel", selected = "All")
    updateSelectInput(session, "transmission", selected = "All")
    updateSelectInput(session, "classification", selected = "All")
    updateSelectInput(session, "model", selected = "All")
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
