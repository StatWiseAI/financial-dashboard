# Financial Reporting Dashboard - R/Shiny Application
# Author: [Your Name]
# Description: IFRS-compliant financial reporting dashboard with P&L, Balance Sheet, and Cash Flow analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)
library(openxlsx)
library(scales)

# Generate sample financial data (simulating database connection)
generate_sample_data <- function() {
  # Set seed for reproducibility
  set.seed(123)
  
  # Generate monthly data for 2 years
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "month")
  
  # P&L Data
  pnl_data <- data.frame(
    date = dates,
    revenue = round(runif(length(dates), 800000, 1200000), 0),
    cost_of_goods = round(runif(length(dates), 300000, 500000), 0),
    operating_expenses = round(runif(length(dates), 200000, 350000), 0),
    depreciation = round(runif(length(dates), 25000, 40000), 0),
    interest_expense = round(runif(length(dates), 5000, 15000), 0),
    tax_rate = 0.25
  ) %>%
    mutate(
      gross_profit = revenue - cost_of_goods,
      ebitda = gross_profit - operating_expenses,
      ebit = ebitda - depreciation,
      ebt = ebit - interest_expense,
      net_income = ebt * (1 - tax_rate),
      year = year(date),
      month = month(date, label = TRUE),
      quarter = paste0("Q", quarter(date))
    )
  
  # Balance Sheet Data
  balance_sheet_data <- data.frame(
    date = dates,
    cash = round(runif(length(dates), 100000, 500000), 0),
    accounts_receivable = round(runif(length(dates), 150000, 300000), 0),
    inventory = round(runif(length(dates), 200000, 400000), 0),
    ppe_gross = round(runif(length(dates), 800000, 1200000), 0),
    accumulated_depreciation = cumsum(round(runif(length(dates), 25000, 40000), 0)),
    accounts_payable = round(runif(length(dates), 80000, 200000), 0),
    short_term_debt = round(runif(length(dates), 50000, 150000), 0),
    long_term_debt = round(runif(length(dates), 300000, 600000), 0)
  ) %>%
    mutate(
      ppe_net = ppe_gross - accumulated_depreciation,
      total_assets = cash + accounts_receivable + inventory + ppe_net,
      total_liabilities = accounts_payable + short_term_debt + long_term_debt,
      equity = total_assets - total_liabilities,
      year = year(date),
      quarter = paste0("Q", quarter(date))
    )
  
  # Cash Flow Data
  cash_flow_data <- pnl_data %>%
    left_join(balance_sheet_data, by = c("date", "year", "quarter")) %>%
    mutate(
      operating_cash_flow = net_income + depreciation + 
        lag(accounts_receivable, default = 0) - accounts_receivable +
        lag(inventory, default = 0) - inventory +
        accounts_payable - lag(accounts_payable, default = 0),
      investing_cash_flow = -round(runif(length(dates), 20000, 80000), 0),
      financing_cash_flow = -round(runif(length(dates), 10000, 50000), 0),
      net_cash_flow = operating_cash_flow + investing_cash_flow + financing_cash_flow
    )
  
  return(list(
    pnl = pnl_data,
    balance_sheet = balance_sheet_data,
    cash_flow = cash_flow_data
  ))
}

# Generate the sample data
financial_data <- generate_sample_data()

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Financial Reporting Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Executive Summary", tabName = "summary", icon = icon("chart-line")),
      menuItem("P&L Statement", tabName = "pnl", icon = icon("dollar-sign")),
      menuItem("Balance Sheet", tabName = "balance", icon = icon("balance-scale")),
      menuItem("Cash Flow", tabName = "cashflow", icon = icon("money-bill-wave")),
      menuItem("Key Ratios", tabName = "ratios", icon = icon("calculator")),
      menuItem("Export Reports", tabName = "export", icon = icon("download"))
    ),
    
    # Sidebar filters
    hr(),
    h4("Filters", style = "margin-left: 15px;"),
    
    selectInput("year_filter", "Select Year:",
                choices = c("All", unique(financial_data$pnl$year)),
                selected = "All"),
    
    selectInput("quarter_filter", "Select Quarter:",
                choices = c("All", "Q1", "Q2", "Q3", "Q4"),
                selected = "All"),
    
    dateRangeInput("date_range", "Date Range:",
                   start = min(financial_data$pnl$date),
                   end = max(financial_data$pnl$date),
                   min = min(financial_data$pnl$date),
                   max = max(financial_data$pnl$date))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
      "))
    ),
    
    tabItems(
      # Executive Summary Tab
      tabItem(tabName = "summary",
        fluidRow(
          valueBoxOutput("total_revenue"),
          valueBoxOutput("net_income"),
          valueBoxOutput("total_assets")
        ),
        
        fluidRow(
          box(
            title = "Revenue Trend", status = "primary", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("revenue_trend")
          ),
          box(
            title = "Profitability Analysis", status = "success", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("profitability_chart")
          )
        ),
        
        fluidRow(
          box(
            title = "Key Performance Indicators", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("kpi_table")
          )
        )
      ),
      
      # P&L Statement Tab
      tabItem(tabName = "pnl",
        fluidRow(
          box(
            title = "Profit & Loss Statement", status = "primary", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("pnl_table")
          )
        ),
        
        fluidRow(
          box(
            title = "P&L Waterfall Chart", status = "warning", solidHeader = TRUE,
            width = 12, height = 500,
            plotlyOutput("pnl_waterfall")
          )
        )
      ),
      
      # Balance Sheet Tab
      tabItem(tabName = "balance",
        fluidRow(
          box(
            title = "Balance Sheet", status = "primary", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("balance_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Asset Composition", status = "success", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("asset_composition")
          ),
          box(
            title = "Equity vs Debt", status = "info", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("equity_debt_chart")
          )
        )
      ),
      
      # Cash Flow Tab
      tabItem(tabName = "cashflow",
        fluidRow(
          box(
            title = "Cash Flow Statement", status = "primary", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("cashflow_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Cash Flow Trend", status = "warning", solidHeader = TRUE,
            width = 12, height = 500,
            plotlyOutput("cashflow_trend")
          )
        )
      ),
      
      # Key Ratios Tab
      tabItem(tabName = "ratios",
        fluidRow(
          valueBoxOutput("current_ratio"),
          valueBoxOutput("debt_to_equity"),
          valueBoxOutput("roa")
        ),
        
        fluidRow(
          box(
            title = "Financial Ratios Over Time", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            plotlyOutput("ratios_trend")
          )
        ),
        
        fluidRow(
          box(
            title = "Ratio Analysis Table", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("ratios_table")
          )
        )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
        fluidRow(
          box(
            title = "Export Financial Reports", status = "primary", solidHeader = TRUE,
            width = 12,
            h4("Download Options"),
            br(),
            downloadButton("download_pnl", "Download P&L Statement", class = "btn-primary"),
            br(), br(),
            downloadButton("download_balance", "Download Balance Sheet", class = "btn-success"),
            br(), br(),
            downloadButton("download_cashflow", "Download Cash Flow", class = "btn-info"),
            br(), br(),
            downloadButton("download_all", "Download Complete Report", class = "btn-warning")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- financial_data
    
    # Filter by date range
    data$pnl <- data$pnl %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    data$balance_sheet <- data$balance_sheet %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    data$cash_flow <- data$cash_flow %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    # Filter by year
    if (input$year_filter != "All") {
      year_val <- as.numeric(input$year_filter)
      data$pnl <- data$pnl %>% filter(year == year_val)
      data$balance_sheet <- data$balance_sheet %>% filter(year == year_val)
      data$cash_flow <- data$cash_flow %>% filter(year == year_val)
    }
    
    # Filter by quarter
    if (input$quarter_filter != "All") {
      data$pnl <- data$pnl %>% filter(quarter == input$quarter_filter)
      data$balance_sheet <- data$balance_sheet %>% filter(quarter == input$quarter_filter)
      data$cash_flow <- data$cash_flow %>% filter(quarter == input$quarter_filter)
    }
    
    return(data)
  })
  
  # Value boxes for summary
  output$total_revenue <- renderValueBox({
    total_rev <- sum(filtered_data()$pnl$revenue, na.rm = TRUE)
    valueBox(
      value = dollar(total_rev),
      subtitle = "Total Revenue",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$net_income <- renderValueBox({
    total_ni <- sum(filtered_data()$pnl$net_income, na.rm = TRUE)
    valueBox(
      value = dollar(total_ni),
      subtitle = "Net Income",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$total_assets <- renderValueBox({
    latest_assets <- tail(filtered_data()$balance_sheet$total_assets, 1)
    valueBox(
      value = dollar(latest_assets),
      subtitle = "Total Assets",
      icon = icon("building"),
      color = "yellow"
    )
  })
  
  # Charts
  output$revenue_trend <- renderPlotly({
    p <- filtered_data()$pnl %>%
      plot_ly(x = ~date, y = ~revenue, type = 'scatter', mode = 'lines+markers',
              name = 'Revenue', line = list(color = 'blue')) %>%
      layout(title = "Monthly Revenue Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Revenue ($)"))
    p
  })
  
  output$profitability_chart <- renderPlotly({
    p <- filtered_data()$pnl %>%
      plot_ly(x = ~date) %>%
      add_lines(y = ~gross_profit, name = 'Gross Profit', line = list(color = 'green')) %>%
      add_lines(y = ~ebitda, name = 'EBITDA', line = list(color = 'orange')) %>%
      add_lines(y = ~net_income, name = 'Net Income', line = list(color = 'red')) %>%
      layout(title = "Profitability Metrics",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Amount ($)"))
    p
  })
  
  # Tables
  output$pnl_table <- DT::renderDataTable({
    pnl_display <- filtered_data()$pnl %>%
      select(date, revenue, cost_of_goods, gross_profit, operating_expenses, 
             ebitda, ebit, net_income) %>%
      mutate(across(where(is.numeric), ~ dollar(.)))
    
    DT::datatable(pnl_display, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  caption = "Profit & Loss Statement (IFRS Compliant)")
  })
  
  output$balance_table <- DT::renderDataTable({
    balance_display <- filtered_data()$balance_sheet %>%
      select(date, cash, accounts_receivable, inventory, ppe_net, 
             total_assets, accounts_payable, total_liabilities, equity) %>%
      mutate(across(where(is.numeric), ~ dollar(.)))
    
    DT::datatable(balance_display, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  caption = "Balance Sheet (IFRS Compliant)")
  })
  
  output$cashflow_table <- DT::renderDataTable({
    cf_display <- filtered_data()$cash_flow %>%
      select(date, operating_cash_flow, investing_cash_flow, 
             financing_cash_flow, net_cash_flow) %>%
      mutate(across(where(is.numeric), ~ dollar(.)))
    
    DT::datatable(cf_display, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  caption = "Cash Flow Statement")
  })
  
  # KPI Table
  output$kpi_table <- DT::renderDataTable({
    latest_data <- tail(filtered_data()$balance_sheet, 1)
    latest_pnl <- tail(filtered_data()$pnl, 1)
    
    kpis <- data.frame(
      Metric = c("Current Ratio", "Debt-to-Equity", "ROA", "ROE", "Gross Margin", "Net Margin"),
      Value = c(
        round((latest_data$cash + latest_data$accounts_receivable) / 
              (latest_data$accounts_payable + latest_data$short_term_debt), 2),
        round(latest_data$total_liabilities / latest_data$equity, 2),
        round(latest_pnl$net_income / latest_data$total_assets * 100, 2),
        round(latest_pnl$net_income / latest_data$equity * 100, 2),
        round(latest_pnl$gross_profit / latest_pnl$revenue * 100, 2),
        round(latest_pnl$net_income / latest_pnl$revenue * 100, 2)
      ),
      Benchmark = c("2.0", "0.5", "5%", "15%", "40%", "10%")
    )
    
    DT::datatable(kpis, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Download handlers
  output$download_pnl <- downloadHandler(
    filename = function() {
      paste("PnL_Statement_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filtered_data()$pnl, file)
    }
  )
  
  output$download_balance <- downloadHandler(
    filename = function() {
      paste("Balance_Sheet_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filtered_data()$balance_sheet, file)
    }
  )
  
  output$download_cashflow <- downloadHandler(
    filename = function() {
      paste("Cash_Flow_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filtered_data()$cash_flow, file)
    }
  )
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste("Financial_Report_Complete_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "P&L Statement")
      addWorksheet(wb, "Balance Sheet")
      addWorksheet(wb, "Cash Flow")
      
      writeData(wb, "P&L Statement", filtered_data()$pnl)
      writeData(wb, "Balance Sheet", filtered_data()$balance_sheet)
      writeData(wb, "Cash Flow", filtered_data()$cash_flow)
      
      saveWorkbook(wb, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
