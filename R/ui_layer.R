# UI Layer Module
# This module contains all UI components and definitions for the HSE MWI SME Group application

# ==============================================================================
# Header Components
# ==============================================================================

#' Create Application Header
#'
#' @description Creates the header component for the application with title and navigation
#'
#' @param title Character string for the application title
#' @param subtitle Character string for the application subtitle (optional)
#'
#' @return A list containing header UI elements
#'
#' @examples
#' create_header("HSE MWI SME Group", "Data Analysis Dashboard")
#'
#' @export
create_header <- function(title, subtitle = NULL) {
  header_ui <- list(
    h1(title, class = "app-title"),
    if (!is.null(subtitle)) {
      p(subtitle, class = "app-subtitle")
    }
  )
  return(header_ui)
}

# ==============================================================================
# Navigation Components
# ==============================================================================

#' Create Navigation Bar
#'
#' @description Creates a navigation bar with menu items
#'
#' @param ... Navigation items (navbarMenu or navbarPage items)
#'
#' @return A navbarPage UI element
#'
#' @export
create_navbar <- function(...) {
  navbarPage(
    title = "HSE MWI SME Group",
    theme = bslib::bs_theme(version = 5),
    ...
  )
}

#' Create Sidebar Navigation
#'
#' @description Creates a sidebar panel with navigation links
#'
#' @return A sidebarPanel UI element
#'
#' @export
create_sidebar_nav <- function() {
  sidebarPanel(
    width = 3,
    h3("Navigation"),
    navlistPanel(
      tabPanel(
        "Dashboard",
        icon = icon("gauge"),
        p("View key metrics and summaries")
      ),
      tabPanel(
        "Data Management",
        icon = icon("database"),
        p("Upload and manage data")
      ),
      tabPanel(
        "Analysis",
        icon = icon("chart-bar"),
        p("Perform statistical analysis")
      ),
      tabPanel(
        "Settings",
        icon = icon("gear"),
        p("Configure application settings")
      ),
      tabPanel(
        "Help",
        icon = icon("question-circle"),
        p("User documentation and support")
      )
    )
  )
}

# ==============================================================================
# Form Components
# ==============================================================================

#' Create Data Upload Form
#'
#' @description Creates a form for uploading CSV or Excel files
#'
#' @return A div containing file upload UI elements
#'
#' @export
create_upload_form <- function() {
  div(
    class = "upload-form",
    h3("Upload Data File"),
    fileInput(
      inputId = "file_upload",
      label = "Choose CSV or Excel file",
      accept = c(".csv", ".xlsx", ".xls"),
      multiple = FALSE
    ),
    div(
      class = "form-actions",
      actionButton("btn_upload", "Upload", class = "btn-primary"),
      actionButton("btn_clear", "Clear", class = "btn-secondary")
    ),
    hr(),
    div(
      id = "upload_status",
      class = "alert",
      hidden(
        div(
          id = "upload_success",
          class = "alert alert-success",
          p("File uploaded successfully!")
        )
      ),
      hidden(
        div(
          id = "upload_error",
          class = "alert alert-danger",
          p("Error uploading file. Please try again.")
        )
      )
    )
  )
}

#' Create Filter Form
#'
#' @description Creates a form for filtering data with common filter options
#'
#' @return A div containing filter UI elements
#'
#' @export
create_filter_form <- function() {
  div(
    class = "filter-form",
    h3("Filter Options"),
    fluidRow(
      column(
        6,
        textInput(
          inputId = "filter_search",
          label = "Search",
          placeholder = "Enter search term..."
        )
      ),
      column(
        6,
        selectInput(
          inputId = "filter_category",
          label = "Category",
          choices = c("All", "Category 1", "Category 2", "Category 3")
        )
      )
    ),
    fluidRow(
      column(
        6,
        dateRangeInput(
          inputId = "filter_daterange",
          label = "Date Range",
          start = Sys.Date() - 30,
          end = Sys.Date()
        )
      ),
      column(
        6,
        checkboxGroupInput(
          inputId = "filter_status",
          label = "Status",
          choices = c("Active", "Inactive", "Pending"),
          selected = "Active"
        )
      )
    ),
    div(
      class = "form-actions",
      actionButton("btn_apply_filters", "Apply Filters", class = "btn-primary"),
      actionButton("btn_reset_filters", "Reset", class = "btn-secondary")
    )
  )
}

# ==============================================================================
# Display Components
# ==============================================================================

#' Create Data Table Display
#'
#' @description Creates a DataTable for displaying data
#'
#' @param id Character string for the element ID
#'
#' @return A div containing DT::dataTableOutput
#'
#' @export
create_data_table <- function(id) {
  div(
    class = "data-table-container",
    h3("Data Table"),
    DT::dataTableOutput(id)
  )
}

#' Create Metrics Display Panel
#'
#' @description Creates a panel displaying key metrics
#'
#' @param metric_title Character string for metric title
#' @param metric_value Character string or number for metric value
#' @param metric_icon Character string for icon (fontawesome)
#' @param metric_color Character string for color class
#'
#' @return A div containing the metric display
#'
#' @export
create_metric_card <- function(metric_title, metric_value, metric_icon = NULL, metric_color = "primary") {
  div(
    class = paste("metric-card", "bg-", metric_color, sep = ""),
    div(
      class = "metric-icon",
      if (!is.null(metric_icon)) icon(metric_icon)
    ),
    div(
      class = "metric-content",
      p(metric_title, class = "metric-title"),
      h2(metric_value, class = "metric-value")
    )
  )
}

#' Create Dashboard Summary
#'
#' @description Creates a summary dashboard with multiple metric cards
#'
#' @return A fluidRow containing metric cards
#'
#' @export
create_dashboard_summary <- function() {
  fluidRow(
    column(
      3,
      create_metric_card(
        "Total Records",
        "1,234",
        "list",
        "primary"
      )
    ),
    column(
      3,
      create_metric_card(
        "Active Users",
        "456",
        "users",
        "success"
      )
    ),
    column(
      3,
      create_metric_card(
        "Pending Items",
        "89",
        "hourglass-half",
        "warning"
      )
    ),
    column(
      3,
      create_metric_card(
        "System Status",
        "Healthy",
        "check-circle",
        "info"
      )
    )
  )
}

# ==============================================================================
# Chart Components
# ==============================================================================

#' Create Plot Output Container
#'
#' @description Creates a container for displaying plots
#'
#' @param id Character string for the element ID
#' @param title Character string for the plot title
#'
#' @return A div containing the plot output
#'
#' @export
create_plot_container <- function(id, title = NULL) {
  div(
    class = "plot-container",
    if (!is.null(title)) h3(title),
    plotOutput(id, height = "500px")
  )
}

#' Create Tabbed Plot Display
#'
#' @description Creates tabbed interface for multiple plots
#'
#' @param ... Tab panels with plots
#'
#' @return A tabsetPanel with multiple plot tabs
#'
#' @export
create_plot_tabs <- function(...) {
  tabsetPanel(
    type = "tabs",
    ...
  )
}

# ==============================================================================
# Modal Components
# ==============================================================================

#' Create Confirmation Modal
#'
#' @description Creates a modal dialog for user confirmation
#'
#' @param id Character string for the modal ID
#' @param title Character string for modal title
#' @param message Character string for confirmation message
#'
#' @return A modalDialog
#'
#' @export
create_confirmation_modal <- function(id, title, message) {
  modalDialog(
    title = title,
    message,
    footer = tagList(
      actionButton(paste0(id, "_cancel"), "Cancel", class = "btn-secondary"),
      actionButton(paste0(id, "_confirm"), "Confirm", class = "btn-primary")
    ),
    easyClose = FALSE,
    size = "m"
  )
}

#' Create Information Modal
#'
#' @description Creates an informational modal dialog
#'
#' @param title Character string for modal title
#' @param content Character string or HTML content for modal body
#'
#' @return A modalDialog
#'
#' @export
create_info_modal <- function(title, content) {
  modalDialog(
    title = title,
    content,
    footer = tagList(
      actionButton("modal_close", "Close", class = "btn-primary")
    ),
    easyClose = TRUE,
    size = "m"
  )
}

# ==============================================================================
# Alert Components
# ==============================================================================

#' Create Alert Message
#'
#' @description Creates an alert message div
#'
#' @param type Character string: "success", "info", "warning", or "danger"
#' @param message Character string for alert message
#' @param dismissible Logical, whether alert can be dismissed
#'
#' @return A div containing the alert
#'
#' @export
create_alert <- function(type = "info", message, dismissible = TRUE) {
  div(
    class = paste("alert alert-", type, if (dismissible) " alert-dismissible fade show"),
    role = "alert",
    message,
    if (dismissible) {
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "alert",
        `aria-label` = "Close"
      )
    }
  )
}

# ==============================================================================
# Footer Components
# ==============================================================================

#' Create Application Footer
#'
#' @description Creates a footer for the application
#'
#' @param company Character string for company name
#' @param year Numeric or character for copyright year
#'
#' @return A div containing footer content
#'
#' @export
create_footer <- function(company = "HSE MWI SME Group", year = format(Sys.Date(), "%Y")) {
  div(
    class = "app-footer",
    hr(),
    p(
      sprintf("&copy; %s %s. All rights reserved.", year, company),
      br(),
      "For support, please contact: support@example.com",
      align = "center"
    )
  )
}

# ==============================================================================
# Layout Components
# ==============================================================================

#' Create Main Application Layout
#'
#' @description Creates the main layout structure for the application
#'
#' @param sidebar_content UI elements for sidebar
#' @param main_content UI elements for main content area
#'
#' @return A fluidPage with sidebar and main content layout
#'
#' @export
create_main_layout <- function(sidebar_content, main_content) {
  fluidPage(
    fluidRow(
      column(3, sidebar_content),
      column(9, main_content)
    )
  )
}

#' Create Card Component
#'
#' @description Creates a styled card panel
#'
#' @param title Character string for card title
#' @param content UI elements for card body
#' @param footer UI elements for card footer (optional)
#'
#' @return A div containing the card
#'
#' @export
create_card <- function(title, content, footer = NULL) {
  div(
    class = "card",
    div(
      class = "card-header",
      h5(title)
    ),
    div(
      class = "card-body",
      content
    ),
    if (!is.null(footer)) {
      div(
        class = "card-footer",
        footer
      )
    }
  )
}

# ==============================================================================
# Button Components
# ==============================================================================

#' Create Action Button Set
#'
#' @description Creates a set of action buttons
#'
#' @param ... Individual actionButton calls
#'
#' @return A div containing multiple action buttons
#'
#' @export
create_button_group <- function(...) {
  div(
    class = "button-group btn-group",
    role = "group",
    ...
  )
}

#' Create Primary Action Button
#'
#' @param id Character string for button ID
#' @param label Character string for button label
#'
#' @return An actionButton with primary styling
#'
#' @export
create_primary_button <- function(id, label) {
  actionButton(id, label, class = "btn btn-primary btn-lg")
}

#' Create Secondary Action Button
#'
#' @param id Character string for button ID
#' @param label Character string for button label
#'
#' @return An actionButton with secondary styling
#'
#' @export
create_secondary_button <- function(id, label) {
  actionButton(id, label, class = "btn btn-secondary btn-lg")
}

# ==============================================================================
# Help and Documentation Components
# ==============================================================================

#' Create Help Panel
#'
#' @description Creates a help section with documentation
#'
#' @return A div containing help content
#'
#' @export
create_help_panel <- function() {
  div(
    class = "help-panel",
    h2("Help & Documentation"),
    h4("Getting Started"),
    p("Welcome to the HSE MWI SME Group application. Follow these steps to get started:"),
    tags$ol(
      tags$li("Upload your data using the Data Management section"),
      tags$li("Configure filters and options as needed"),
      tags$li("View analysis results in the Analysis tab"),
      tags$li("Export or share your findings")
    ),
    h4("FAQ"),
    details(
      summary("How do I upload data?"),
      p("Navigate to Data Management > Upload Data File and select your CSV or Excel file.")
    ),
    details(
      summary("What file formats are supported?"),
      p("The application supports CSV, XLSX, and XLS file formats.")
    ),
    details(
      summary("How do I reset filters?"),
      p("Click the Reset button in the Filter Options panel to clear all active filters.")
    )
  )
}

# ==============================================================================
# Utility Functions
# ==============================================================================

#' Create Custom Theme
#'
#' @description Creates a custom Bootstrap theme for the application
#'
#' @return A bslib theme object
#'
#' @export
create_app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#007bff",
    secondary = "#6c757d",
    success = "#28a745",
    danger = "#dc3545",
    warning = "#ffc107",
    info = "#17a2b8",
    light = "#f8f9fa",
    dark = "#343a40"
  )
}

#' Add Custom CSS
#'
#' @description Adds custom CSS styling for the application
#'
#' @return A tags$style object with custom CSS
#'
#' @export
add_custom_css <- function() {
  tags$style(HTML("
    /* Application Styles */
    .app-title {
      color: #007bff;
      font-weight: bold;
      margin-bottom: 10px;
    }
    
    .app-subtitle {
      color: #6c757d;
      font-size: 1.1em;
      margin-bottom: 20px;
    }
    
    .metric-card {
      padding: 20px;
      border-radius: 8px;
      margin-bottom: 15px;
      color: white;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    .metric-card .metric-title {
      font-size: 0.9em;
      margin-bottom: 10px;
    }
    
    .metric-card .metric-value {
      font-size: 1.8em;
      font-weight: bold;
    }
    
    .metric-card .metric-icon {
      font-size: 2.5em;
      opacity: 0.8;
    }
    
    .card {
      margin-bottom: 20px;
      border: 1px solid #dee2e6;
      border-radius: 8px;
    }
    
    .card-header {
      background-color: #f8f9fa;
      border-bottom: 1px solid #dee2e6;
      padding: 15px;
    }
    
    .card-body {
      padding: 20px;
    }
    
    .card-footer {
      background-color: #f8f9fa;
      padding: 15px;
      border-top: 1px solid #dee2e6;
    }
    
    .button-group {
      display: flex;
      gap: 10px;
      margin: 10px 0;
    }
    
    .form-actions {
      margin-top: 15px;
      display: flex;
      gap: 10px;
    }
    
    .app-footer {
      margin-top: 40px;
      padding: 20px;
      background-color: #f8f9fa;
      border-top: 1px solid #dee2e6;
      color: #6c757d;
      font-size: 0.9em;
    }
  "))
}

#' Create Complete UI
#'
#' Assembles all UI components into the final application UI
#'
#' @param config Application configuration list
#' @return A Shiny UI definition
#' @export
create_ui <- function(config) {
  # 1. Define theme
  theme <- create_app_theme()
  
  # 2. Assemble the Dashboard
  # We use the components you already defined in ui_layer.R
  
  # Define Sidebar
  sidebar <- create_sidebar_nav()
  
  # Define Main Body (Example structure)
  body <- tagList(
    add_custom_css(),
    shinyjs::useShinyjs(), # Required for shinyjs to work
    
    # Header
    div(class = "container-fluid",
        create_header(title = config$app_name, subtitle = config$app_description)
    ),
    
    # Dashboard Content
    tabsetPanel(
      id = "main_tabs",
      type = "hidden", # Hidden tabs controlled by sidebar
      
      # Dashboard Tab
      tabPanelBody("panel_Dashboard",
        create_dashboard_summary(),
        hr(),
        create_card("System Overview", p("Welcome to the HSE MWI SME Group Dashboard."))
      ),
      
      # Data Management Tab
      tabPanelBody("panel_Data Management",
        create_upload_form(),
        hr(),
        create_data_table("raw_data_table")
      ),
      
      # Analysis Tab
      tabPanelBody("panel_Analysis",
        create_filter_form(),
        hr(),
        create_plot_container("main_plot", "Analysis Results")
      )
    ),
    
    # Footer
    create_footer()
  )
  
  # 3. Return the Layout
  # Using fluidPage as the base container
  return(
    fluidPage(
      theme = theme,
      sidebarLayout(
        sidebarPanel = sidebar,
        mainPanel = body
      )
    )
  )
}
