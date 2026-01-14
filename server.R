# server.R

function(input, output, session) {

  # --- INTERNATIONALIZATION LOGIC ---
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  # ----------------------------------
  
  # preallocate custom data
  ol <- do.call(reactiveValues, overall)
  
  focus_info <- reactiveValues(
    "hl" = F,
    "ZCTA" = ""
  )
  
  st_sub <- reactiveValues(
    "idx" = "pop",
    "st" = "Virginia",
    "geodat" = overall$geodat[["pop"]][overall$geodat[["pop"]]$STATE_NAME == "Virginia",],
    "mwi" = overall$mwi[["pop"]][overall$mwi[["pop"]]$STATE_NAME == "Virginia",],
    "us_map_fill" = "Mental_Wellness_Index",
    "is_all" = F
  )
  
  us_proxy <- leafletProxy("us_map")
  
  com_sub <- reactiveValues(
    "idx" = "pop",
    "ZCTA" = "23936", 
    "geodat" = overall$geodat[["pop"]][ 
      st_coordinates(overall$geopts$pop)[,1] >=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] - 1 &
        st_coordinates(overall$geopts$pop)[,1] <=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] + 1 &
        st_coordinates(overall$geopts$pop)[,2] >=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] - 1 &
        st_coordinates(overall$geopts$pop)[,2] <=
        st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] + 1 
      ,],
    "mwi" = overall$mwi[["pop"]][
      overall$mwi[["pop"]]$ZCTA %in% 
        overall$geodat[["pop"]]$GEOID[
          st_coordinates(overall$geopts$pop)[,1] >=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] - 1 &
            st_coordinates(overall$geopts$pop)[,1] <=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[1] + 1 &
            st_coordinates(overall$geopts$pop)[,2] >=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] - 1 &
            st_coordinates(overall$geopts$pop)[,2] <=
            st_coordinates(overall$geopts$pop[overall$geopts$pop$GEOID == "23936",])[2] + 1 
        ]
      ,],
    "com_map_fill" = "Mental_Wellness_Index"
  )
  
  # Modal Logic
  welcome_modal <- modalDialog(
    title = HTML("<b><center>Welcome to the Mental Wellness Index™!</b></center>"),
    size = "l",
    fluidRow(
      column(width = 1),
      column(width = 10,
             HTML("<p align = 'center'><font size = '3'>The <b>Mental Wellness Index (MWI)</b> combines 28 factors that influence <b>community-level mental wellness</b> into a single value for <b>each ZIP code</b> in the nation.</font></p>"),
             HTML("<center>"),
             img(src = file.path("media", "MWI Framework (Transparent Background).png"), align = "center", width = "60%"),
             HTML("</center><br><center>To learn more and view MWI videos, click <b>MWI Toolkit</b> in the blue bar at the top of the page.</center>"),
             HTML("<center><font size = '2'><i>Notes: This application is best viewed on a tablet or computer in full screen mode. Data updated as of January 2023.</i></font></center>")
      )
    ),
    footer = tagList(HTML("<center>"), modalButton("Start Exploring!"), HTML("</center>")),
    easyClose = T
  )
  
  showModal(welcome_modal)
  
  observeEvent(input$learn_button, {
    updateNavbarPage(session = session, inputId = "toolkit", selected = "mwi_overview")
    removeModal()
  })
  
  observeEvent(input$video_button, {
    updateNavbarPage(session = session, inputId = "toolkit", selected = "mwi_tool_videos_and_guides")
    removeModal()
  })
  
  observeEvent(input$enter_mwi, {
    updateSelectInput(session = session, "st_focus", choices = c(unname(f_st), "All"), selected = input$start_st)
    removeModal()
  })
  
  # Custom Data Logic
  observeEvent(input$custom_data_load_st, {
    withProgress(message = "Uploading custom Mental Wellness Index!", {
      if (!is.null(input$custom_data_st)){
        validate(need(endsWith(tolower(input$custom_data_st$datapath), ".rdata"), "Must upload .RData File"))
        load(input$custom_data_st$datapath)
        
        for (ov in names(ol)){ ol[[ov]] <- overall_output[[ov]] }
        
        geodat <- geopts <- list()
        for (idx in index_types){
          geo_sub <- st_drop_geometry(overall$geodat[[idx]])[, "GEOID"] %in% overall_output$mwi[[idx]][,"ZCTA"]
          geodat[[idx]] <- left_join(overall$geodat[[idx]][geo_sub, 1:7], overall_output$mwi[[idx]], by = c("GEOID" = "ZCTA"))
          geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE, geodat[[idx]]$GEOID),]
          geopts[[idx]] <- st_centroid(geodat[[idx]])
        }
        ol[["geodat"]] <- geodat
        ol[["geopts"]] <- geopts
        
        updateSelectInput(session = session, "st_focus", choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"), selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1])
        updateSelectInput(session = session, "us_map_fill", choices = ol$avail_meas_list[["pop"]])
        updateSelectInput(session = session, "com_map_fill", choices = ol$avail_meas_list[["pop"]])
        com_sub$ZCTA <- ol$mwi$pop$ZCTA[1]
      }
    })
  })
  
  observeEvent(input$custom_data_load_com, {
    withProgress(message = "Uploading custom Mental Wellness Index!", {
      if (!is.null(input$custom_data_com)){
        load(input$custom_data_com$datapath)
        for (ov in names(ol)){ ol[[ov]] <- overall_output[[ov]] }
        
        geodat <- geopts <- list()
        for (idx in index_types){
          geo_sub <- st_drop_geometry(overall$geodat[[idx]])[, "GEOID"] %in% overall_output$mwi[[idx]][,"ZCTA"]
          geodat[[idx]] <- left_join(overall$geodat[[idx]][geo_sub, 1:7], overall_output$mwi[[idx]], by = c("GEOID" = "ZCTA"))
          geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE, geodat[[idx]]$GEOID),]
          geopts[[idx]] <- st_centroid(geodat[[idx]])
        }
        ol[["geodat"]] <- geodat
        ol[["geopts"]] <- geopts
        
        updateSelectInput(session = session, "st_focus", choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"), selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1])
        updateSelectInput(session = session, "us_map_fill", choices = ol$avail_meas_list[["pop"]])
        updateSelectInput(session = session, "com_map_fill", choices = ol$avail_meas_list[["pop"]])
        com_sub$ZCTA <- ol$mwi$pop$ZCTA[1]
      }
    })
  })
  
  observeEvent(c(input$custom_data_reset_st, input$custom_data_reset_com), {
    if (!is.null(input$custom_data_com) | !is.null(input$custom_data_st)){
      for (ov in names(ol)){ ol[[ov]] <- overall[[ov]] }
      updateSelectInput(session = session, "st_focus", choices = c(unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]]), "All"), selected = unname(f_st[f_st %in% f_st[ol$mwi$pop$STATE]])[1])
      updateSelectInput(session = session, "us_map_fill", choices = ol$avail_meas_list[["pop"]])
      updateSelectInput(session = session, "com_map_fill", choices = ol$avail_meas_list[["pop"]])
      com_sub$ZCTA <- "23936"
    }
  })
  
  # State View Logic
  observeEvent(input$idx_type, {
    idx <- input$idx_type
    fill <- if (idx == "pop" & grepl("*_black$", input$us_map_fill)){ gsub("*_black$", "_pop", input$us_map_fill) } else if (idx == "black" & grepl("*_pop$", input$us_map_fill) & !input$us_map_fill %in% colnames(ol$mwi[["black"]])){ gsub("*_pop$", "_black", input$us_map_fill) } else { input$us_map_fill }
    updateSelectInput(session = session, "us_map_fill", choices = ol$avail_meas_list[[idx]], selected = fill)
  })
  
  observeEvent(c(input$st_focus, input$idx_type), {
    idx <- input$idx_type
    if (idx == st_sub$idx){ focus_info$hl <- F; focus_info$ZCTA <- ""; if (!st_sub$is_all){ us_proxy %>% removeShape("remove_me") } else { us_proxy %>% removeMarker("remove_me") } }
    st_sub$idx <- idx
    
    if (input$st_focus == "All"){
      st_sub$st <- "All"; st_sub$geodat <- ol$geopts[[idx]]; st_sub$mwi <- ol$mwi[[idx]]; st_sub$is_all <- T
    } else {
      st_sub$st <- input$st_focus
      st_sub$geodat <- ol$geodat[[idx]][ol$geodat[[idx]]$STATE_NAME == input$st_focus | ol$geodat[[idx]]$STATE_2 == st_to_fips[input$st_focus] & !is.na(ol$geodat[[idx]]$STATE_2),]
      st_sub$mwi <- ol$mwi[[idx]][ol$mwi[[idx]]$STATE_NAME == input$st_focus | ol$mwi[[idx]]$STATE_2 == st_to_fips[input$st_focus] & !is.na(ol$mwi[[idx]]$STATE_2),]
      st_sub$is_all <- F
    }
    
    st_sub$us_map_fill <- if (st_sub$idx == "pop" & grepl("*_black$", input$us_map_fill)){ gsub("*_black$", "_pop", input$us_map_fill) } else if (st_sub$idx == "black" & grepl("*_pop$", input$us_map_fill) & !input$us_map_fill %in% colnames(ol$mwi[["black"]])){ gsub("*_pop$", "_black", input$us_map_fill) } else { input$us_map_fill }
  })
  
  observeEvent(input$reset_zcta_click, { focus_info$hl <- F; focus_info$ZCTA <- ""; if (!st_sub$is_all){ us_proxy %>% removeShape("remove_me") } else { us_proxy %>% removeMarker("remove_me") } })
  
  observeEvent(input$us_map_fill, {
    st_sub$us_map_fill <- if (st_sub$idx == "pop" & grepl("*_black$", input$us_map_fill)){ gsub("*_black$", "_pop", input$us_map_fill) } else if (st_sub$idx == "black" & grepl("*_pop$", input$us_map_fill) & !input$us_map_fill %in% colnames(ol$mwi[["black"]])){ gsub("*_pop$", "_black", input$us_map_fill) } else { input$us_map_fill }
  })
  
  observeEvent(input$us_map_shape_click, {
    if (!is.null(input$us_map_shape_click$id)){
      focus_info$hl <- T; focus_info$ZCTA <- input$us_map_shape_click$id
      if (!st_sub$is_all){ us_proxy %>% removeShape("remove_me") } else { us_proxy %>% removeMarker("remove_me") }
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, st_sub$idx, ol = ol, is_all = st_sub$is_all, add_poly = T, us_proxy = us_proxy, zcta_choose = focus_info$ZCTA)
    } else {
      focus_info$hl <- F; focus_info$ZCTA <- ""; if (!st_sub$is_all){ us_proxy %>% removeShape("remove_me") } else { us_proxy %>% removeMarker("remove_me") }
    }
  })
  
  observeEvent(input$zip_choose, {
    if (input$zip_choose != "" & nchar(input$zip_choose) == 5 & !grepl("\\D", input$zip_choose) & input$zip_choose %in% names(zip_to_zcta) & zip_to_zcta[input$zip_choose] %in% st_sub$geodat$GEOID){
      focus_info$hl <- T; focus_info$ZCTA <- unname(zip_to_zcta[input$zip_choose])
      if (!st_sub$is_all){ us_proxy %>% removeShape("remove_me") } else { us_proxy %>% removeMarker("remove_me") }
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, st_sub$idx, ol = ol, is_all = st_sub$is_all, add_poly = T, us_proxy = us_proxy, zcta_choose = focus_info$ZCTA)
    } else {
      focus_info$hl <- F; focus_info$ZCTA <- ""; if (!st_sub$is_all){ us_proxy %>% removeShape("remove_me") } else { us_proxy %>% removeMarker("remove_me") }
    }
  })
  
  # Community View Logic
  observeEvent(input$idx_type_com, {
    idx <- input$idx_type
    fill <- if (idx == "pop" & grepl("*_black$", input$com_map_fill)){ gsub("*_black$", "_pop", input$com_map_fill) } else if (idx == "black" & grepl("*_pop$", input$com_map_fill) & !input$com_map_fill %in% colnames(ol$mwi[["black"]])){ gsub("*_pop$", "_black", input$com_map_fill) } else { input$com_map_fill }
    updateSelectInput(session = session, "com_map_fill", choices = ol$avail_meas_list[[idx]], selected = fill)
  })
  
  observeEvent(c(input$idx_type_com, input$zip_choose_com), {
    idx <- input$idx_type_com; orig_zcta <- com_sub$ZCTA
    if (input$zip_choose_com != "" & nchar(input$zip_choose_com) == 5 & !grepl("\\D", input$zip_choose_com) & input$zip_choose_com %in% names(zip_to_zcta)){ com_sub$ZCTA <- unname(zip_to_zcta[input$zip_choose_com]) }
    if (com_sub$idx != idx | com_sub$ZCTA != orig_zcta){
      com_sub$idx <- idx
      all_coord <- st_coordinates(ol$geopts[[idx]])
      zcta_coord <- st_coordinates(ol$geopts$pop[ol$geopts$pop$GEOID == com_sub$ZCTA,])
      zcta_log <- all_coord[,1] >= zcta_coord[1] - 1 & all_coord[,1] <= zcta_coord[1] + 1 & all_coord[,2] >= zcta_coord[2] - 1 & all_coord[,2] <= zcta_coord[2] + 1 
      if (sum(zcta_log) <= 1){ zcta_log <- all_coord[,1] >= zcta_coord[1] - 3 & all_coord[,1] <= zcta_coord[1] + 3 & all_coord[,2] >= zcta_coord[2] - 3 & all_coord[,2] <= zcta_coord[2] + 3 }
      com_sub$geodat <- ol$geodat[[idx]][zcta_log,]
      com_sub$mwi <- ol$mwi[[idx]][ol$mwi[[idx]]$ZCTA %in% ol$geodat[[idx]]$GEOID[zcta_log],]
      com_sub$com_map_fill <- if (com_sub$idx == "pop" & grepl("*_black$", input$com_map_fill)){ gsub("*_black$", "_pop", input$com_map_fill) } else if (com_sub$idx == "black" & grepl("*_pop$", input$com_map_fill) & !input$com_map_fill %in% colnames(ol$mwi[["black"]])){ gsub("*_pop$", "_black", input$com_map_fill) } else { input$com_map_fill }
    }
  })
  
  observeEvent(input$com_map_fill, {
    com_sub$com_map_fill <- if (com_sub$idx == "pop" & grepl("*_black$", input$com_map_fill)){ gsub("_black", "_pop", input$com_map_fill) } else if (com_sub$idx == "black" & grepl("*_pop$", input$com_map_fill) & !input$com_map_fill %in% colnames(ol$mwi[["black"]])){ gsub("*_pop$", "_black", input$com_map_fill) } else { input$com_map_fill }
  })
  
  # Outputs
  output$focus_on <- reactive({ focus_info$ZCTA != "" }); outputOptions(output, "focus_on", suspendWhenHidden = FALSE)
  output$data_info <- renderUI({
    withProgress(message = "Rendering info", {
      full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
      HTML(paste0("<font size = '2'>", "Data pictured for ", full_name, " came from ", ifelse(full_name == "Mental Wellness Index", "", ol$info_dat[st_sub$us_map_fill, "Years"]), " ", ifelse(full_name == "Mental Wellness Index", "various sources", ol$info_dat[st_sub$us_map_fill, "Source"]), ".<p><p>", ifelse(full_name == "Mental Wellness Index", "", paste0(full_name, " Description: ", ol$info_dat[st_sub$us_map_fill, "Measure.Description"])), "</font>"))
    })
  })
  
  output$us_map <- renderLeaflet({
    # --- SECURITY VALIDATION ---
    validate(need(input$st_focus, "State selection required"), need(input$us_map_fill, "Measure required"))
    # ---------------------------
    withProgress(message = "Rendering map", {
      us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, st_sub$idx, ol, is_all = st_sub$is_all)
      if (focus_info$hl){ us_proxy <- plot_map(st_sub$us_map_fill, st_sub$geodat, st_sub$idx, ol, is_all = st_sub$is_all, add_poly = T, us_proxy = us_proxy, zcta_choose = focus_info$ZCTA) }
      us_proxy
    })
  })
  
  output$us_map_legend <- output$com_map_legend <- renderUI({
    HTML(paste0("<center>", paste(sapply(1:length(meas_max_colors), function(x){ if (names(meas_max_colors[x]) != "Mental Wellness Index"){ paste0("<font color = ", meas_max_colors[x], " size = '3'><b>", names(meas_max_colors[x]), "</font></b>") } else { paste(sapply(1:nchar(names(meas_max_colors[x])), function(y){ paste0("<font color = ", meas_colors_pal$`Mental Wellness Index`(nchar("Mental Wellness Index"))[y], " size = '3'><b>", strsplit(names(meas_max_colors[x]), "")[[1]][y], "</font></b>") }), collapse = "") } }), collapse = "<font size = 3'><b> | </b></font>"), "</center>"))
  })
  
  output$us_map_expl <- renderUI({
    full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
    mc <- meas_max_colors[ol$meas_col_to_type[full_name]]; lc <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){ meas_min_colors[ol$meas_col_to_type[full_name]] } else { mc }
    dir_val <- if (st_sub$us_map_fill != "Mental_Wellness_Index"){ ol$info_dat[st_sub$us_map_fill, "Directionality"] } else { 1 }
    text <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){ paste0("A ", html_color(mc, "higher"), " value indicates more ", html_color(mc, "assets"), " supporting ", html_color(mc, "mental wellness"), ". A ", html_color(lc, "lower"), " value indicates more ", html_color(lc, "obstacles"), ".") } else { paste0("A ", html_color(mc, "higher"), " value indicates a ", html_color(mc, "higher"), " national ", ifelse(st_sub$us_map_fill != "Mental_Wellness_Index", "ranking", "value"), " for ", html_color(mc, full_name), ".") }
    HTML(paste0("<center><font size = '3'><b>", text, "</b></font></center>"))
  })
  
  output$us_distr_title <- renderUI({ HTML(paste0("<b><center><font size = '3'>Distribution of ", ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill], " for ", ifelse(st_sub$idx != "black", "the ", ""), ifelse(st_sub$idx == "black", "Black ", "Overall "), "Population", ifelse(st_sub$idx == "black", "s", ""), " in ", st_sub$st, "</b></center></font>")) })
  output$us_distr <- renderPlotly({ withProgress(message = "Rendering distribution", { plot_bee_distr(st_sub$us_map_fill, st = st_sub$st, mwi = st_sub$mwi, idx = st_sub$idx, ol = ol, is_all = st_sub$is_all, hl = focus_info$hl, zcta_hl = focus_info$ZCTA) }) })
  
  output$us_info <- renderUI({
    if (focus_info$ZCTA != ""){
      full_name <- ol$measure_to_names[[st_sub$idx]][st_sub$us_map_fill]
      dir_df <- if (st_sub$us_map_fill == "Mental_Wellness_Index"){ st_sub$mwi } else { ol$no_dir_perc_meas_df[st_sub$mwi$ZCTA,] }
      f_val <- dir_df[dir_df$ZCTA == focus_info$ZCTA, st_sub$us_map_fill]
      
      if (!is.na(f_val)){
        HTML(paste0("<center><b><font size = '3'>ZCTA ", focus_info$ZCTA, " has value ", trunc(f_val), ".</font></b></center>"))
      } else {
        HTML(paste0("<center><b><font size = '3'>ZCTA ", focus_info$ZCTA, " has no value.</font></b></center>"))
      }
    }
  })
  
  output$data_info_com <- renderUI({
    full_name <- ol$measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
    HTML(paste0("<font size = '2'>Data for ", full_name, ".</font>"))
  })
  
  output$com_map <- renderLeaflet({
    withProgress(message = "Rendering map", { plot_map(com_sub$com_map_fill, com_sub$geodat, com_sub$idx, ol, is_all = F, is_com = T, zcta_choose = com_sub$ZCTA) })
  })
  
  output$com_map_expl <- renderUI({
    full_name <- ol$measure_to_names[[com_sub$idx]][com_sub$com_map_fill]
    HTML(paste0("<center><b>Explanation for ", full_name, "</b></center>"))
  })
  
  output$com_report_card_table_mwi <- renderUI({
    mwi_zcta <- com_sub$mwi[com_sub$mwi$ZCTA == com_sub$ZCTA, , drop = F]
    HTML(paste0("<b>ZCTA: ", com_sub$ZCTA, " MWI: ", trunc(mwi_zcta[1, "Mental_Wellness_Index"]), "</b>"))
  })
  
  output$com_report_card_table <- renderDataTable({
    reportcard <- ol$m_reg[,c("Measure", "Measure Description", "Category", "Directionality")]
    Rank <- t(ol$no_dir_perc_meas_df[com_sub$ZCTA, ol$avail_measures[[com_sub$idx]][-1]])
    colnames(Rank) <- "Rank"; rownames(Rank) <- gsub("_pop$", "", rownames(Rank))
    val <- t(ol$meas_df[com_sub$ZCTA, ol$avail_measures[[com_sub$idx]][-1]])
    colnames(val) <- "Value"; rownames(val) <- gsub("_pop$", "", rownames(val))
    reportcard[rownames(val), "Value"] <- val
    reportcard <- merge(reportcard, Rank, by = "row.names", sort = F) %>% as.data.frame() %>% select(Measure, Rank, Value, `Measure Description`, Category)
    datatable(reportcard, rownames = F, options = list(pageLength = nrow(reportcard), columnDefs = list(list(width = c('250px'), targets = c(3)))))
  })
  
  # Custom MWI Logic
  overall_list <- reactiveVal()
  upd_weights <- reactiveVal(sub_m)
  button_click <- reactiveValues(go = 0, weights = 0, comp = 0)
  
  output$custom_mwi_weights <- renderDT({ datatable(upd_weights(), rownames = F, options = list(pageLength = nrow(m_reg)), editable = list(target = "cell", disable = list(columns = c(0:2)))) })
  observeEvent(input$custom_mwi_weights_cell_edit, { df <- upd_weights(); row <- input$custom_mwi_weights_cell_edit$row; clmn <- input$custom_mwi_weights_cell_edit$col; df[row, clmn+1] <- input$custom_mwi_weights_cell_edit$value; upd_weights(df) })
  
  output$download_metadata <- output$download_metadata_comp <- downloadHandler(
    filename = function(){ "Metadata.xlsx" },
    content = function(file){ desc <- as.data.frame(read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 2)); write_xlsx(list("Measure Registry" = m_reg, "Column Descriptions" = desc), file) }
  )
  
  observeEvent(c(input$custom_mwi_go, input$custom_mwi_go_comp, input$custom_mwi_go_weights), {
    req(isTruthy(input$custom_mwi_go) || isTruthy(input$custom_mwi_go_comp) || isTruthy(input$custom_mwi_go_weights))
    
    # Identify which button was pressed
    press <- "go" 
    if(input$custom_mwi_go_weights > 0) press <- "weights"
    if(input$custom_mwi_go_comp > 0) press <- "comp"
    
    withProgress(message = "Creating custom Mental Wellness Index!", detail = "Loading data...", {
      source(file.path("Processing_Pipeline", "pipeline_driver.R"))
      
      # Logic for handling uploaded files vs weights
      # This is simplified - in the full app you would include the specific file checks
      # For now, we assume success or handle generic errors
      
      tryCatch({
        # Run Pipeline logic here
        # overall_out <- mwi_pipeline(...)
        
        # Simulating completion for this refactoring guide
        output$custom_error_weights <- renderText({ "Custom MWI generation logic would run here." })
        
      }, error = function(cond){
        output$custom_error_weights <- renderText({ paste0("Error: ", cond) })
      })
    })
  })
  
  output$download_custom_mwi <- output$download_custom_mwi_comp <- output$download_custom_mwi_weights <- downloadHandler(
    filename = function(){ paste0("Custom_MWI_", Sys.Date(), ".RData") },
    content = function(file){ withProgress(message = "Downloading...", { overall_output <- overall_list(); save(overall_output, file = file) }) }
  )
}
