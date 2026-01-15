get_main_ui <- function(i18n, selected_lang = "en", index_types, f_st, overall, mwi_toolkit_order, show_mitre) {
  tagList(
    navbarPage(
      collapsible = T,
      title=
        if (show_mitre){
          div(
            a(
              href="https://www.mitre.org/",
              img(src="media/MITRE_logo.png", height="30"),
              target="blank",
            ),
            HTML(paste0(i18n$t("Mental Wellness Index™ Tool"))) # Wrapped in translate
          )
        } else {
          div(
            HTML(paste0(i18n$t("Mental Wellness Index™ Tool"))), 
            "style" = "padding-top:5px"
          )
        },
      theme="stylesheets/app.css",
      
      # explore states ----
      tabPanel(
        title = div(i18n$t("Explore States"), class="explore"),
        class = "explore-panel",
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # Language Selector
            selectInput('selected_language',
                        i18n$t("Change Language"),
                        choices = i18n$get_languages(),
                        selected = selected_lang),
            hr(),
            bsCollapse(
              multiple = T,
              open = c(i18n$t("Exploration Options"), i18n$t("About Selected Measure"), i18n$t("About the Mental Wellness Index")),
              bsCollapsePanel(
                i18n$t("Exploration Options"),
                HTML(paste0("<b>", i18n$t("Welcome to the Mental Wellness Index (MWI) Tool!"), "</b> ", i18n$t("Select any of the options below to get started."), " <b>", i18n$t("If you would like to focus on a specific ZIP Code*, click on it in the map to the right or select it from the list below."), "</b><p>")),
                radioButtons(
                  inputId = "idx_type",
                  label = i18n$t("Which population's MWI do you want to view?"),
                  choiceValues = unname(index_types),
                  choiceNames = c(i18n$t("Overall"), i18n$t("Black")),
                  inline = T
                ),
                # TODO: UPDATE THESE BASED ON POPULATION SELECTED
                selectInput(
                  "st_focus",
                  i18n$t("Which state would you like to focus on?"),
                  choices = c(unname(f_st), i18n$t("All")),
                  selected = "Virginia"
                ),
                selectInput(
                  "us_map_fill",
                  i18n$t("What would you like to explore?"),
                  choices = overall$avail_meas_list[["pop"]]
                ),
                textInput(
                  "zip_choose",
                  label = i18n$t("Which ZIP Code would you like to focus on in the selected state?"),
                  placeholder = "e.g. 35004, 00501, 20041, etc."
                ),
                actionButton("reset_zcta_click", i18n$t("Reset ZIP Code Focus"))
              ),
              bsCollapsePanel(
                i18n$t("Custom MWI Upload"),
                tagList(
                  HTML("<font size = '2'><p>"),
                  i18n$t("To create the necessary custom Mental Wellness Index file, please see the \"Create Your Own MWI\" tab. Note that data uploaded to this application is not kept -- it is deleted once you leave the page. However, if you would like to keep your data on your computer while viewing the MWI, please see the \"Add Local Data to MWI on Your Computer\" section."),
                  HTML(paste0("<i>", i18n$t("NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\"."), "</i>")),
                  HTML("<p></font>"),
                  fileInput(
                    "custom_data_st",
                    label = i18n$t("Upload Custom Mental Wellness Index (.RData)"),
                    accept = ".RData"
                  ),
                  actionButton(
                    "custom_data_load_st",
                    i18n$t("Run Custom MWI")
                  ),
                  actionButton(
                    "custom_data_reset_st",
                    i18n$t("Reset")
                  )
                )
              ),
              bsCollapsePanel(
                i18n$t("About the Mental Wellness Index"),
                HTML("<center>"),
                img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", width = "90%"),
                HTML("</center>"),
                HTML("<font size = '2'>"),
                HTML(paste0(i18n$t("The Mental Wellness Index is the weighted sum of 28 measure values, which quantify facilitators and barriers to mental wellness. For more information about the Mental Wellness Index, please see the 'MWI Toolkit' page."), "<p></p>"
                )),
                HTML(paste0(
                  i18n$t("All states are included. Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points."), "<p></p>")),
                HTML(paste0(i18n$t("* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see"), " <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>", i18n$t("census.gov"), "</a>.<p></p>"
                )),
                HTML("</font>")
              )
            )
          ),
          mainPanel(
            width = 9,
            tags$head(tags$script(src = "msg_api.js")),
            tags$head(tags$script(src = "web_content.js")),
            column(
              width = 8,
              uiOutput("us_map_legend"),
              HTML("<br>"),
              withSpinner(leafletOutput("us_map", height = 850),
                          type = 8, color = "#005B94", hide.ui = F)
            ),
            column(
              width = 4,
              # hr(),
              uiOutput("us_distr_title"),
              withSpinner(plotlyOutput("us_distr", height = 400),
                          type = 8, color = "#005B94", hide.ui = F),
              # hr(),
              bsCollapse(
                multiple = T,
                open = c(i18n$t("Measure Interpretation"), i18n$t("About Selected Measure")),
                bsCollapsePanel(
                  i18n$t("Measure Interpretation"),
                  conditionalPanel(
                    condition = "!output.focus_on",
                    # tableOutput("us_quantile"),
                    uiOutput("us_map_expl")
                  ),
                  conditionalPanel(
                    condition = "output.focus_on",
                    uiOutput("us_info")
                  )
                ),
                bsCollapsePanel(
                  i18n$t("About Selected Measure"),
                  uiOutput("data_info"),
                  HTML(paste0(
                    "<font size = '2'>",
                    
                    i18n$t("For more information on data and overall methodology, please see the \"MWI Toolkit\" page."),
                    
                    "</font>"
                  ))
                )
              )
            )
          )
        )
      ),
      
      # explore ZIP codes ----
      tabPanel(
        title = div(i18n$t("Explore ZIP Codes"), class="explore"),
        class = "explore-panel",
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            bsCollapse(
              multiple = T,
              open = c(i18n$t("Exploration Options"), i18n$t("About Selected Measure"), i18n$t("About the Mental Wellness Index")),
              bsCollapsePanel(
                i18n$t("Exploration Options"),
                HTML(paste0("<b>", i18n$t("Welcome to the Mental Wellness Index (MWI) Tool!"), "</b> ", i18n$t("To explore your community's outcomes, enter a specific ZIP Code* to get started."), "<p>")),
                radioButtons(
                  inputId = "idx_type_com",
                  label = i18n$t("Which population's MWI do you want to view?"),
                  choiceValues = unname(index_types),
                  choiceNames = c(i18n$t("Overall"), i18n$t("Black")),
                  inline = T
                ),
                selectInput(
                  "com_map_fill",
                  i18n$t("What would you like to explore?"),
                  choices = overall$avail_meas_list[["pop"]]
                ),
                textInput(
                  "zip_choose_com",
                  label = i18n$t("Which ZIP Code would you like to focus on?"),
                  placeholder = "e.g. 35004, 00501, 20041, etc."
                )
              ),
              bsCollapsePanel(
                "Custom MWI Upload",
                tagList(
                  HTML("<font size = '2'><p>"),
                  "To create the necessary custom Mental Wellness Index file, please see the \"Create Your Own MWI\" tab. Note that data uploaded to this application is not kept -- it is deleted once you leave the page. However, if you would like to keep your data on your computer while viewing the MWI, please see the \"Add Local Data to MWI on Your Computer\" section.",
                  HTML("<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>"),
                  HTML("</p></font>"),
                  fileInput(
                    "custom_data_com",
                    label = "Upload Custom Mental Wellness Index (.RData)",
                    accept = ".RData"
                  ),
                  actionButton(
                    "custom_data_load_com",
                    "Run Custom MWI"
                  ),
                  actionButton(
                    "custom_data_reset_com",
                    "Reset"
                  )
                )
              ),
              bsCollapsePanel(
                "About the Mental Wellness Index",
                HTML("<center>"),
                img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", width = "90%"),
                HTML("</center>"),
                HTML("<font size = '2'>"),
                HTML(paste0("The Mental Wellness Index is the weighted sum of 28 measure values, which quantify facilitators and barriers to mental wellness. For more information about the Mental Wellness Index, please see the 'MWI Toolkit' page.<p></p>"
                )),
                HTML(paste0(
                  "All states are included.",
                  " Selecting \"All\" will show all included states. Note that this is slower to render and will show ZCTAs as points.<p></p>")),
                HTML(paste0("* ZCTAs are used in the Mental Wellness Index and are represented in maps and plots. ZIP codes are analgous to ZCTAs. When ZIP Codes are entered above, they are mapped to ZCTAs. For more information on ZCTAs, please see <a href='https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html' target = '_blank'>census.gov</a>.<p></p>"
                )),
                HTML("</font>")
              )
            )
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Explore ZCTA Maps",
                p(),
                column(
                  width = 8,
                  uiOutput("com_map_legend"),
                  HTML("<br>"),
                  withSpinner(leafletOutput("com_map", height = 850),
                              type = 8, color = "#005B94", hide.ui = F)
                ),
                column(
                  width = 4,
                  bsCollapse(
                    multiple = T,
                    open = c("ZCTA Measure Results", "Selected Measure Interpretation", "About Selected Measure"),
                    bsCollapsePanel(
                      "Selected Measure Interpretation",
                      uiOutput("com_map_expl")
                    ),
                    bsCollapsePanel(
                      "About Selected Measure",
                      uiOutput("data_info_com"),
                      HTML(paste0(
                        "<font size = '2'>",
                        "For more information on data and overall methodology, please see the \"MWI Toolkit\" page.",
                        "</font>"
                      ))
                    ),
                    bsCollapsePanel(
                      "ZCTA Measure Results",
                      uiOutput("com_map_report_card")
                    )
                  )
                )
              ),
              tabPanel(
                "Explore ZCTA Measures",
                p(),
                bsCollapse(
                  open = c("ZCTA Measure Results"),
                  bsCollapsePanel(
                    "ZCTA Measure Results",
                    HTML("<p><i>Measures have ranks from 0 to 100. Measures with a higher rank (closer to 100) indicate more community-level <b>assets</b> or <b>obstacles</b> to mental wellness, based on their respective directionality. Measure value corrsponds to the exact value in the data, corresponding to the measure description. For more information, please see `MWI Measures and Data` in the MWI Toolkit.</i></p>"),
                    uiOutput("com_report_card_table_mwi"),
                    HTML("<p></p>"),
                    DTOutput("com_report_card_table")
                  )
                )
              )
            )
          )
        )
      ),
      
      # upload data ----
      
      navbarMenu(
        "Create Your Own MWI",
        tabPanel(
          title = div("Adjust MWI Weights and ZIP Codes", class = "explore"),
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              HTML("<center><h2>Change weights and ZIP Codes to create your own Mental Wellness Index (MWI)!</h2></center>"),
              HTML(paste0(
                "<p align = 'justify'>",
                "Weights used in the Mental Wellness Index control the relative influence each measure has on the total MWI; higher numbers inidcate a higher influence. If you adjust the weights to 0, a measure has no influence on the MWI. You can also use this page to rank a subset of ZIP Codes against each other.",
                "<br><br>",
                "To adjust the weights or change the ZIP Codes used in the Mental Wellness Index, follow the instructions below. If you want to add your own data to the MWI, go to the \"Add Local Data to MWI\" section. Note that data uploaded to this application is not kept -- it is deleted once you leave the page, including any processing done to it.",
                "<ol>",
                "<li>To update weights for each measure, click the \"Adjust MWI Weights\" tab below. Then update the table as desired by doubleclicking the 'Updated Weights' column, then editing the measure to the desired amount (0 or a positive number). Click outside of that edited entry to lock it in. Note that weights do not need to add to 100 (they will be normalized, and have georgraphy/race stratification penalties applied, when the Custom MWI is calculated).</li>",
                "<br>",
                "<li>To rank a subset of ZIP Codes or ZCTAs against each other, click the \"Subset Zip Codes/ZCTAs\" tab below. There, you will enter the ZIP Codes you want to create the MWI for in the text box, with each ZIP Code or ZCTA on a separate line. Note that these all must be either ZIP Codes or ZCTAs -- you cannot submit a mix. Use the switch below the text box to indicate whether the entries are ZIP Codes or ZCTAs. Note: If no values are entered, all ZCTAs will be used. Entering ZCTAs is more accurate. Comparing at least 10 ZCTAs/ZIP Codes is recommended.</li>",
                "<br>",
                "<li>Click 'Create Custom MWI' below. This will take some time.</li>",
                "<br>",
                "<li>Once the custom MWI creation is complete, click 'Download Custom MWI' to download an .RData file with all of the needed information to view your MWI in this tool. <b>Note: if you navigate away from this page, all processing will be lost! Nothing is stored within this application.</b></li>",
                "<br>",
                "<li>To view your MWI, click the 'Custom MWI Upload' box under 'Explore States' or 'Explore ZIP Codes' and upload the downloaded '.RData' file. Once the file is fully loaded, click 'Upload' to see your Custom MWI results.</li>",
                "</ol>",
                "</p>"
              )),
              hr(),
              HTML("<center>"),
              tabsetPanel(
                tabPanel(
                  "Adjust MWI Weights",
                  DTOutput("custom_mwi_weights")
                ),
                tabPanel(
                  "Subset Zip Codes/ZCTAs",
                  br(),
                  textAreaInput(
                    "custom_mwi_zips",
                    "Enter ZIP Codes or ZCTAs to subset the MWI to (line separated):",
                    placeholder = "12345\n10034\n19567\n...",
                    height = "200px",
                    width = "400px"
                  ),
                  switchInput(
                    "custom_mwi_zip_choice",
                    onLabel = "ZIP Code",
                    offLabel = "ZCTA",
                    value = T, # zips shown
                    onStatus = "secondary",
                    offStatus = "secondary"
                  )
                )
              ),
              HTML("<br><br>"),
              actionButton("custom_mwi_go_weights", "Create Custom MWI"),
              downloadButton("download_custom_mwi_weights", "Download Custom MWI"),
              HTML("<br><br>"),
              verbatimTextOutput("custom_error_weights"),
              HTML("</center>")
            ),
            column(width = 2)
          )
        ),
        tabPanel(
          title = div("Add Local Data to MWI", class = "explore"),
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              HTML("<center><h2>Add Local Data to Mental Wellness Index (MWI)</h2></center>"),
              HTML(paste0(
                "<p align = 'justify'>",
                "To create your own Mental Wellness Index with your own local data, follow the instructions below to create your own MWI for your community below by adjusting weights and adding your own data and metadata. If you only want to adjust the weights in the MWI, go to the \"Adjust MWI Weights\" section. Note that data uploaded to this application is not kept -- it is deleted once you leave the page, including any processing done to it. However, if you would like to keep your data on your computer while creating the MWI, please see the \"Add Local Data to MWI on Your Computer\" section. ",
                "<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>",
                "<ol>",
                "<li> Put each of your datasets in a CSV (comma separated value) format, with one column corresponding to the geographical ID of the data, a column corresponding to the numerator of the data, and another column corresponding to the denominator (if needed).</li>",
                "<ul>",
                "<li>Accepted geographical ID types are always numeric and include the following:</li>",
                "<ul>",
                "<li>ZCTA: 5 digit ZCTA (example: 35406)</li>",
                "<li>County: 5 digit County FIPS Code (2 digits state code and 3 digit county code, example: 01001)</li>",
                "<li>ZIP Code: US Postal Service ZIP Code (example: 35051)</li>",
                "<li>Census Tract: 11 digit Census Tract FIPS Code (2 digits state code, 3 digit county code, and 6 digit tract code, example: 01001020100)</li>",
                "</ul>",
                "<li>If a denominator column is provided, the final input to the MWI will be the numerator divided by the denominator, multiplied by the scaling number (specified in the metadata file, see next step).</li>",
                "<li>Numerators and denominators must be numeric columns.</li>",
                "<li>Missing data should have cells left blank.</li>",
                "<li>If race stratified, there should be two columns: one ending in '_pop' corresponding to the overall population measure, and one ending in '_black' corresponding to the black populations measure. In the Metadata.xlsx file edit, that row's 'Preprocessed' column should be set to TRUE.</li>",
                "</ul>",
                "<br>",
                "<li> Download Metadata.xlsx with the button below. If adding custom data, add a row and fill in information for each measure you want to add to the Mental Wellness Index. Descriptions for each column can be found in the 'Column Descriptions' sheet of the Metadata.xlsx. Note that <b>all</b> column names, with the exception of 'denominator', must be filled out.</li>",
                "<ul>",
                "<li>If you have multiple measures in one file, add a row for each measure and its qualities, but specify the same file name.</li>",
                "<li>If you would like to remove a measure in your MWI, either delete the measure row or set its weight to 0.</li>",
                "<li>If you would only like to adjust weights, change only the weight column to the desired values. Note that penalties for race stratifications and geographic granularity are still applied and total weights are scaled to sum to 100.</li>",
                "</ul>",
                "<br>",
                "<li>Upload your Metadata.xlsx and custom data files (if using) and click 'Create Custom MWI' below. Select multiple files by pressing ctrl/cmd and then clicking on each file in the file explorer box. Note: Do not have files open while uploading. This will take some time, depending on the amount of measures included.</li>",
                "<br>",
                "<li>Once the custom MWI creation is complete, click 'Download Custom MWI' to download an .RData file with all of the needed information to view your MWI in this tool. <b>Note: if you navigate away from this page, all processing and data will be lost! Nothing is stored within this application.</b></li>",
                "<br>",
                "<li>To view your MWI, click the 'Custom MWI Upload' box under 'Explore States' or 'Explore ZIP Codes' and upload the downloaded '.RData' file. Once the file is fully loaded, click 'Upload' to see your Custom MWI results.</li>",
                "</ol>",
                "</p>"
              )),
              tagList(
                hr(),
                HTML("<center>"),
                downloadButton("download_metadata", "Download Metadata.xlsx"),
                HTML("<br><br>"),
                fileInput(
                  "custom_zip", 
                  "Upload Custom Data Files (.xlsx, .csv) (do not have files open):",
                  accept = c(".xlsx", ".csv"),
                  multiple = T
                ),
                actionButton("custom_mwi_go", "Create Custom MWI"),
                downloadButton("download_custom_mwi", "Download Custom MWI"),
                HTML("<br><br>"),
                verbatimTextOutput("custom_error"),
                HTML("</center>")
              )
            ),
            column(width = 2)
          )
        ),
        tabPanel(
          title = div("Add Local Data to MWI on Your Computer", class = "explore"),
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              HTML("<center><h2>Add Local Data to Mental Wellness Index (MWI) On Your Computer</h2></center>"),
              HTML(paste0(
                "<p align = 'justify'>",
                "If you want to keep your data on your computer, follow the instructions below to create your own MWI for your community by adjusting weights and adding your own data and metadata.",
                "<ol>",
                tagList(HTML(paste0(
                  "<li> Download free versions of <a href = 'https://www.r-project.org/' target = '_blank'>R</a> and <a href = 'https://www.rstudio.com/products/rstudio/download/' target = '_blank'>RStudio</a>. Download a modern browser (Firefox, Chrome, Edge, etc.) and make that your default browser if you haven't already.</li>",
                  "<br>",
                  "<li> Go to the <a href = 'https://github.com/mitre/hse-mwi' target = '_blank'>Mental Wellness Index GitHub page</a> and download the repository by clicking \"Code\" in the top right corner, then clicking \"Download ZIP\" from the dropdown menu. This should download a ZIP file of the MWI repository into your downloads folder, called \"hse-mwi-main.zip\".</li>",
                  "<br>",
                  "<li> Unzip \"hse-mwi-main.zip\".</li>",
                  "<br>",
                  "<li> In the unzipped folder, open \"app.R\" in RStudio. This should open RStudio and the \"app.R\" script in the top left hand corner of the application.</li>",
                  "<br>",
                  "<li> In the console window, which is in the bottom left hand corner, enter the following line and answer \"yes\" to all prompts in the console as you install these packages:</li>",
                  "<ul>",
                  "<li>install.packages('readxl', 'writexl', 'htmltools', 'shiny', 'tigris', 'leaflet', 'RColorBrewer', 'sf', 'plotly', 'ggbeeswarm', 'shinyWidgets', 'sass', 'shinycssloaders', 'shinyBS', 'DT', 'dplyr')</li>",
                  "</ul>",
                  "<br>",
                  "<li> In the top right hand corner of the \"app.R\" window, you should see \"Run App\". Click the small downward arrow to the right of that and click \"Run External\". Then click \"Run App\".</li>",
                  "<br>",
                  "<li> After a delay (this will be slow the first time, then quicker after that), the Mental Wellness Index Tool should open in your browser. Click on the \"Create Your Own MWI\" tab and follow the remaining steps to create your own MWI.</li>",
                  "<br>"
                ))),
                "<li> If you are only adjusting weights for included data, skip the next step.</li>",
                "<br>",
                "<li> Put each of your datasets in a CSV (comma separated value) format, with one column corresponding to the geographical ID of the data, a column corresponding to the numerator of the data, and another column corresponding to the denominator (if needed).</li>",
                "<ul>",
                "<li>Accepted geographical ID types are always numeric and include the following:</li>",
                "<ul>",
                "<li>ZCTA: 5 digit ZCTA (example: 35406)</li>",
                "<li>County: 5 digit County FIPS Code (2 digits state code and 3 digit county code, example: 01001)</li>",
                "<li>ZIP Code: US Postal Service ZIP Code (example: 35051)</li>",
                "<li>Census Tract: 11 digit Census Tract FIPS Code (2 digits state code, 3 digit county code, and 6 digit tract code, example: 01001020100)</li>",
                "</ul>",
                "<li>If a denominator column is provided, the final input to the MWI will be the numerator divided by the denominator, multiplied by the scaling number (specified in the metadata file, see next step).</li>",
                "<li>Numerators and denominators must be numeric columns.</li>",
                "<li>Missing data should have cells left blank.</li>",
                "<li>If race stratified, there should be two columns: one ending in '_pop' corresponding to the overall population measure, and one ending in '_black' corresponding to the black populations measure. In the Metadata.xlsx file edit, that row's 'Preprocessed' column should be set to TRUE.</li>",
                "</ul>",
                "<br>",
                "<li> Download Metadata.xlsx with the button below. If adding custom data, add a row and fill in information for each measure you want to add to the Mental Wellness Index. Descriptions for each column can be found in the 'Column Descriptions' sheet of the Metadata.xlsx. Note that <b>all</b> column names, with the exception of 'denominator', must be filled out.</li>",
                "<ul>",
                "<li>If you have multiple measures in one file, add a row for each measure and its qualities, but specify the same file name.</li>",
                "<li>If you would like to remove a measure in your MWI, either delete the measure row or set its weight to 0.</li>",
                "<li>If you would only like to adjust weights, change only the weight column to the desired values. Note that penalties for race stratifications and geographic granularity are still applied and total weights are scaled to sum to 100.</li>",
                "</ul>",
                "<br>",
                "<li>Upload your Metadata.xlsx and custom data files (if using) and click 'Create Custom MWI' below. Select multiple files by pressing ctrl/cmd and then clicking on each file in the file explorer box. Note: Do not have files open while uploading. This will take some time, depending on the amount of measures included.",
                "<i>NOTE: file upload is currently experiencing issues on the website. In the meantime, you can explore your custom MWI on your local computer by following steps 1 - 7 on the \"Add Local Data to Mental Wellness Index (MWI) On Your Computer\" page under \"Create Your Own MWI\".</i>",
                "</li>",
                "<br>",
                "<li>Once the custom MWI creation is complete, click 'Download Custom MWI' to download an .RData file with all of the needed information to view your MWI in this tool. <b>Note: if you navigate away from this page, all processing and data will be lost! Nothing is stored within this application.</b></li>",
                "<br>",
                "<li>To view your MWI, click the 'Custom MWI Upload' box under 'Explore States' or 'Explore ZIP Codes' and upload the downloaded '.RData' file. Once the file is fully loaded, click 'Upload' to see your Custom MWI results.</li>",
                "</ol>",
                "</p>"
              )),
              tagList(
                hr(),
                HTML("<center>"),
                downloadButton("download_metadata_comp", "Download Metadata.xlsx"),
                HTML("<br><br>"),
                fileInput(
                  "custom_zip_comp", 
                  "Upload Custom Data Files (.xlsx, .csv) (do not have files open):",
                  accept = c(".xlsx", ".csv"),
                  multiple = T
                ),
                actionButton("custom_mwi_go_comp", "Create Custom MWI"),
                downloadButton("download_custom_mwi_comp", "Download Custom MWI"),
                HTML("<br><br>"),
                verbatimTextOutput("custom_error_comp"),
                HTML("</center>")
              )
            ),
            column(width = 2)
          )
        )
      ),
      
      
      # mwi toolkit ----
      
      # add toolkit pages dynamically since there are a lot of them
      do.call(
        navbarMenu,
        c(menuName = "toolkit",
          title = "MWI Toolkit",
          lapply(
            mwi_toolkit_order,
            function(x){
              tabPanel(
                id = tolower(x),
                title = div(gsub("_", " ", x), class = "about"),
                htmltools::tags$iframe(
                  src = paste0("mwi-toolkit/", x, ".html"),
                  class = "about-panel",
                  frameborder = 0,
                  scrolling = "auto")
                
              )
            }
          )
        )
      )
    ),
    
    # Copyright footer
    if (show_mitre){
      HTML(paste0(
        "<span class = 'copyright-footer'>&copy; ",
        format(Sys.Date(), "%Y"),
        
        ", The MITRE Corporation",
        "</span>"
      ))
    }
  )
}
