# ui.R

fluidPage(
  div(titlePanel(title="", windowTitle=HTML(paste0("Mental Wellness Index™ Tool"))), style="display:none"),
  
  # --- INTERNATIONALIZATION UI ---
  div(style = "float: right; padding: 10px;",
      selectInput('selected_language',
                  i18n$t("Change Language"),
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation())
  ),
  # -------------------------------
  
  navbarPage(
    collapsible = T,
    title= if (show_mitre){
      div(a(href="https://www.mitre.org/", img(src="media/MITRE_logo.png", height="30"), target="blank"), HTML(paste0(i18n$t("Mental Wellness Index™ Tool"))))
    } else {
      div(HTML(paste0(i18n$t("Mental Wellness Index™ Tool"))), "style" = "padding-top:5px")
    },
    theme="stylesheets/app.css",
    
    # explore states ----
    tabPanel(
      title = div(i18n$t("Explore States"), class="explore"),
      class = "explore-panel",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "overflow-y: auto; max-height: 90vh;",
          bsCollapse(
            multiple = T,
            open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),
            bsCollapsePanel("Exploration Options",
              HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> Select any of the options below to get started. <b>If you would like to focus on a specific ZIP Code*, click on it in the map to the right or select it from the list below.</b><p>"),
              radioButtons("idx_type", "Which population's MWI do you want to view?", choiceValues = unname(index_types), choiceNames = c("Overall", "Black"), inline = T),
              selectInput("st_focus", "Which state would you like to focus on?", choices = c(unname(f_st), "All"), selected = "Virginia"),
              selectInput("us_map_fill", "What would you like to explore?", choices = overall$avail_meas_list[["pop"]]),
              textInput("zip_choose", "Which ZIP Code would you like to focus on in the selected state?", placeholder = "e.g. 35004, 00501, 20041, etc."),
              actionButton("reset_zcta_click", "Reset ZIP Code Focus")
            ),
            bsCollapsePanel("Custom MWI Upload",
              tagList(
                HTML("<font size = '2'><p>To create the necessary custom Mental Wellness Index file...<i>NOTE: file upload is currently experiencing issues...</i></p></font>"),
                fileInput("custom_data_st", label = "Upload Custom Mental Wellness Index (.RData)", accept = ".RData"),
                actionButton("custom_data_load_st", "Run Custom MWI"),
                actionButton("custom_data_reset_st", "Reset")
              )
            ),
            bsCollapsePanel("About the Mental Wellness Index",
              HTML("<center>"), img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", style = "max-width: 100%; height: auto; width: 90%;"), HTML("</center>"),
              HTML("<font size = '2'>The Mental Wellness Index is the weighted sum of 28 measure values...</font>")
            )
          )
        ),
        mainPanel(
          width = 9,
          tags$head(tags$script(src = "msg_api.js")),
          tags$head(tags$script(src = "web_content.js")),
          fluidRow(
            column(width = 8, uiOutput("us_map_legend"), HTML("<br>"), withSpinner(leafletOutput("us_map", height = "85vh"), type = 8, color = "#005B94", hide.ui = F)),
            column(width = 4, uiOutput("us_distr_title"), withSpinner(plotlyOutput("us_distr", height = "40vh"), type = 8, color = "#005B94", hide.ui = F),
              bsCollapse(multiple = T, open = c("Measure Interpretation", "About Selected Measure"),
                bsCollapsePanel("Measure Interpretation", conditionalPanel(condition = "!output.focus_on", uiOutput("us_map_expl")), conditionalPanel(condition = "output.focus_on", uiOutput("us_info"))),
                bsCollapsePanel("About Selected Measure", uiOutput("data_info"), HTML("<font size = '2'>For more information on data and overall methodology, please see the \"MWI Toolkit\" page.</font>"))
              )
            )
          )
        )
      )
    ),
    
    # explore ZIP codes ----
    tabPanel(
      title = div("Explore ZIP Codes", class="explore"),
      class = "explore-panel",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "overflow-y: auto; max-height: 90vh;",
          bsCollapse(
            multiple = T,
            open = c("Exploration Options", "About Selected Measure", "About the Mental Wellness Index"),
            bsCollapsePanel("Exploration Options",
              HTML("<b>Welcome to the Mental Wellness Index (MWI) Tool!</b> To explore your community's outcomes, enter a specific ZIP Code* to get started.<p>"),
              radioButtons("idx_type_com", "Which population's MWI do you want to view?", choiceValues = unname(index_types), choiceNames = c("Overall", "Black"), inline = T),
              selectInput("com_map_fill", "What would you like to explore?", choices = overall$avail_meas_list[["pop"]]),
              textInput("zip_choose_com", "Which ZIP Code would you like to focus on?", placeholder = "e.g. 35004, 00501, 20041, etc.")
            ),
            bsCollapsePanel("Custom MWI Upload",
              tagList(HTML("<font size = '2'><p>...</p></font>"), fileInput("custom_data_com", label = "Upload Custom Mental Wellness Index (.RData)", accept = ".RData"), actionButton("custom_data_load_com", "Run Custom MWI"), actionButton("custom_data_reset_com", "Reset"))
            ),
            bsCollapsePanel("About the Mental Wellness Index",
              HTML("<center>"), img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", style = "max-width: 100%; height: auto; width: 90%;"), HTML("</center>"), HTML("<font size = '2'>...</font>")
            )
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel("Explore ZCTA Maps", p(), fluidRow(column(width = 8, uiOutput("com_map_legend"), HTML("<br>"), withSpinner(leafletOutput("com_map", height = "85vh"), type = 8, color = "#005B94", hide.ui = F)), column(width = 4, bsCollapse(multiple = T, open = c("ZCTA Measure Results", "Selected Measure Interpretation", "About Selected Measure"), bsCollapsePanel("Selected Measure Interpretation", uiOutput("com_map_expl")), bsCollapsePanel("About Selected Measure", uiOutput("data_info_com"), HTML("<font size = '2'>For more information...</font>")), bsCollapsePanel("ZCTA Measure Results", uiOutput("com_map_report_card")))))),
            tabPanel("Explore ZCTA Measures", p(), bsCollapse(open = c("ZCTA Measure Results"), bsCollapsePanel("ZCTA Measure Results", HTML("<p><i>...</i></p>"), uiOutput("com_report_card_table_mwi"), HTML("<p></p>"), DTOutput("com_report_card_table"))))
          )
        )
      )
    ),
    
    # create your own MWI ----
    navbarMenu("Create Your Own MWI",
      tabPanel(title = div("Adjust MWI Weights and ZIP Codes", class = "explore"),
        fluidRow(column(width = 2), column(width = 8, HTML("<center><h2>Change weights...</h2></center>"), HTML("..."), hr(), HTML("<center>"), tabsetPanel(tabPanel("Adjust MWI Weights", DTOutput("custom_mwi_weights")), tabPanel("Subset Zip Codes/ZCTAs", br(), textAreaInput("custom_mwi_zips", "Enter ZIP Codes...", height = "200px", width = "100%"), switchInput("custom_mwi_zip_choice", onLabel = "ZIP Code", offLabel = "ZCTA", value = T, onStatus = "secondary", offStatus = "secondary"))), HTML("<br><br>"), actionButton("custom_mwi_go_weights", "Create Custom MWI"), downloadButton("download_custom_mwi_weights", "Download Custom MWI"), HTML("<br><br>"), verbatimTextOutput("custom_error_weights"), HTML("</center>"), column(width = 2)))
      ),
      tabPanel(title = div("Add Local Data to MWI", class = "explore"),
        fluidRow(column(width = 12, class = "col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2"), column(width = 12, class = "col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2", HTML("<center><h2>Add Local Data...</h2></center>"), HTML("..."), tagList(hr(), HTML("<center>"), downloadButton("download_metadata", "Download Metadata.xlsx"), HTML("<br><br>"), fileInput("custom_zip", "Upload Custom Data...", accept = c(".xlsx", ".csv"), multiple=T), actionButton("custom_mwi_go", "Create Custom MWI"), downloadButton("download_custom_mwi", "Download Custom MWI"), HTML("<br><br>"), verbatimTextOutput("custom_error"), HTML("</center>"))))
      ),
      tabPanel(title = div("Add Local Data to MWI on Your Computer", class = "explore"),
        fluidRow(column(width = 12, class = "col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2"), column(width = 12, class = "col-sm-10 col-sm-offset-1 col-md-8 col-md-offset-2", HTML("<center><h2>Add Local Data... On Your Computer</h2></center>"), HTML("..."), tagList(hr(), HTML("<center>"), downloadButton("download_metadata_comp", "Download Metadata.xlsx"), HTML("<br><br>"), fileInput("custom_zip_comp", "Upload Custom Data...", accept = c(".xlsx", ".csv"), multiple=T), actionButton("custom_mwi_go_comp", "Create Custom MWI"), downloadButton("download_custom_mwi_comp", "Download Custom MWI"), HTML("<br><br>"), verbatimTextOutput("custom_error_comp"), HTML("</center>"))))
      )
    ),
    
    # toolkit ----
    do.call(navbarMenu, c(menuName = "toolkit", title = "MWI Toolkit",
      lapply(mwi_toolkit_order, function(x){
        tabPanel(id = tolower(x), title = div(gsub("_", " ", x), class = "about"),
          htmltools::tags$iframe(src = paste0("mwi-toolkit/", x, ".html"), class = "about-panel", frameborder = 0, scrolling = "auto"))
      })
    ))
  ),
  
  if (show_mitre){
    HTML(paste0("<span class = 'copyright-footer'>&copy; ", format(Sys.Date(), "%Y"), ", The MITRE Corporation</span>"))
  }
)
