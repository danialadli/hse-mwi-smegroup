# ============================================================================
# HSE MWI SME Group Application - Modular Architecture
# ============================================================================
# This is the main application orchestration file that coordinates
# the modular layer architecture (data, logic, UI, and server layers)
# ============================================================================

# ============================================================================
# 1. LOAD REQUIRED PACKAGES WITH ERROR HANDLING
# ============================================================================

# Vector of required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinyWidgets",
  "ggplot2",
  "plotly",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "DT",
  "readxl",
  "openxlsx",
  "scales",
  "RColorBrewer",
  "formattable"
)

# Function to safely load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      tryCatch(
        {
          install.packages(pkg, dependencies = TRUE)
          library(pkg, character.only = TRUE, quietly = TRUE)
          cat(sprintf("✓ Installed and loaded: %s\n", pkg))
        },
        error = function(e) {
          warning(sprintf("✗ Failed to load package '%s': %s", pkg, conditionMessage(e)))
        }
      )
    } else {
      cat(sprintf("✓ Loaded: %s\n", pkg))
    }
  }
}

# Load all required packages
cat("Loading required packages...\n")
load_packages(required_packages)
cat("Package loading complete.\n\n")

# ============================================================================
# 2. SOURCE MODULAR LAYER FILES FROM R/ DIRECTORY
# ============================================================================

cat("Sourcing modular layer files...\n")

layers <- c(
  "data_layer.R",
  "logic_layer.R",
  "ui_layer.R",
  "server_layer.R"
)

for (layer in layers) {
  layer_path <- file.path("R", layer)
  tryCatch(
    {
      source(layer_path, local = FALSE)
      cat(sprintf("✓ Sourced: %s\n", layer))
    },
    error = function(e) {
      stop(sprintf("✗ Failed to source '%s': %s", layer_path, conditionMessage(e)))
    }
  )
}

cat("Layer sourcing complete.\n\n")

# ============================================================================
# 3. LOAD APPLICATION CONFIGURATION
# ============================================================================

cat("Loading application configuration...\n")

# Define application settings
app_config <- list(
  app_name = "HSE MWI SME Group",
  app_version = "2.0.0",
  app_description = "Multi-sector, multi-workforce initiative group management application",
  
  # Data settings
  data_path = "data/",
  
  # UI settings
  theme = "bootstrap",
  
  # Server settings
  debug_mode = Sys.getenv("DEBUG", "FALSE") == "TRUE",
  
  # Feature flags
  features = list(
    enable_export = TRUE,
    enable_import = TRUE,
    enable_analytics = TRUE,
    enable_audit_logs = TRUE
  ),
  
  # Display settings
  table_rows_per_page = 10,
  max_upload_size = 50 * 1024^2,  # 50 MB
  
  # Color scheme
  primary_color = "#0072B2",
  success_color = "#009E73",
  warning_color = "#E69F00",
  error_color = "#D55E00"
)

cat(sprintf("✓ Configuration loaded: %s v%s\n", 
            app_config$app_name, app_config$app_version))
cat("\n")

# ============================================================================
# 4. INITIALIZE DATA USING DATA LAYER FUNCTIONS
# ============================================================================

cat("Initializing application data...\n")

# Initialize global data environment
app_data <- new.env()

tryCatch(
  {
    # Load all required datasets using data layer functions
    # These functions should be defined in data_layer.R
    
    if (exists("initialize_data_layer", mode = "function")) {
      app_data$datasets <- initialize_data_layer(app_config$data_path)
      cat("✓ Data layer initialized\n")
    } else {
      warning("initialize_data_layer function not found in data_layer.R")
    }
    
    cat("Data initialization complete.\n\n")
  },
  error = function(e) {
    warning(sprintf("Error during data initialization: %s", conditionMessage(e)))
  }
)

# ============================================================================
# 5. SET UP STYLING AND RESOURCES
# ============================================================================

cat("Setting up styling and resources...\n")

# Initialize shinyjs for dynamic client-side interactions
shinyjs::useShinyjs()

# Define custom CSS and resources
# These can be extended in www/ directory as needed
app_resources <- list(
  # Custom CSS
  custom_css = "
    .main-container {
      padding: 20px;
    }
    .section-title {
      color: #0072B2;
      border-bottom: 2px solid #0072B2;
      padding-bottom: 10px;
      margin-top: 20px;
      margin-bottom: 15px;
    }
    .stat-box {
      background-color: #f5f5f5;
      border-left: 4px solid #0072B2;
      padding: 15px;
      margin-bottom: 10px;
    }
    .error-message {
      color: #D55E00;
      font-weight: bold;
    }
    .success-message {
      color: #009E73;
      font-weight: bold;
    }
  "
)

cat("✓ Styling configured\n")
cat("✓ Resources initialized\n\n")

# ============================================================================
# 6. DEFINE THE USER INTERFACE USING UI LAYER FUNCTIONS
# ============================================================================

cat("Constructing user interface...\n")

# Create UI using functions from ui_layer.R
# The ui_layer should export a function that returns the complete UI
ui <- tryCatch(
  {
    if (exists("create_ui", mode = "function")) {
      create_ui(app_config)
    } else if (exists("ui_builder", mode = "function")) {
      ui_builder(app_config)
    } else {
      stop("No UI creation function found in ui_layer.R. Expected 'create_ui' or 'ui_builder'.")
    }
  },
  error = function(e) {
    stop(sprintf("Failed to create UI: %s", conditionMessage(e)))
  }
)

cat("✓ User interface constructed\n\n")

# ============================================================================
# 7. DEFINE THE SERVER USING SERVER LAYER FUNCTIONS
# ============================================================================

cat("Setting up server logic...\n")

# Create server function using functions from server_layer.R
server <- function(input, output, session) {
  
  # Initialize shinyjs on client
  shinyjs::runjs(sprintf("document.title = '%s';", app_config$app_name))
  
  # Create a reactive environment for session-level data and state
  session_state <- reactiveValues(
    data = app_data$datasets,
    config = app_config,
    user_messages = ""
  )
  
  tryCatch(
    {
      # Call the main server setup function from server_layer.R
      if (exists("initialize_server", mode = "function")) {
        initialize_server(input, output, session, session_state, app_config)
      } else if (exists("setup_server", mode = "function")) {
        setup_server(input, output, session, session_state, app_config)
      } else if (exists("server_logic", mode = "function")) {
        server_logic(input, output, session, session_state, app_config)
      } else {
        warning("No server initialization function found in server_layer.R. Expected 'initialize_server', 'setup_server', or 'server_logic'.")
      }
      
      cat("✓ Server logic initialized for session\n")
    },
    error = function(e) {
      cat(sprintf("✗ Server initialization error: %s\n", conditionMessage(e)))
    }
  )
}

cat("✓ Server logic set up\n\n")

# ============================================================================
# 8. RUN SHINY APPLICATION
# ============================================================================

cat("Starting Shiny application...\n")
cat(sprintf("Application: %s v%s\n", app_config$app_name, app_config$app_version))
cat("="*60, "\n\n")

# Run the Shiny application
shinyApp(ui = ui, server = server)
