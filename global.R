# global.R

# 1. Load Libraries ----
library(readxl)
library(writexl)
library(htmltools)
library(shiny)
library(tigris)
library(leaflet)
library(RColorBrewer)
library(sf)
library(plotly)
library(ggbeeswarm)
library(shinyWidgets)
library(sass)
library(shinycssloaders)
library(shinyBS)
library(DT)
library(dplyr)
library(shiny.i18n)

# 2. Settings & Config ----
options(shiny.maxRequestSize=300*1024^2)
options(tigris_use_cache = TRUE)

source("app_config.R")
addResourcePath("mwi-toolkit", "mwi-toolkit")

# --- INTERNATIONALIZATION SETUP ---
# Load translation file
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en") # Default to English
# ----------------------------------

# 3. Styling ----
sass(
  sass_file("www/stylesheets/app.scss"),
  output = "www/stylesheets/app.css"
)
sass(
  sass_file("www/stylesheets/app.scss"),
  output = "about/app.css"
)

# 4. Helper Functions ----

# Helper: Cache Reader
read_csv_cached <- function(file_path, ...) {
  rds_path <- gsub("\\.csv$", ".rds", file_path)
  if (file.exists(rds_path) && file.exists(file_path)) {
    csv_time <- file.info(file_path)$mtime
    rds_time <- file.info(rds_path)$mtime
    if (rds_time >= csv_time) {
      data <- readRDS(rds_path)
      rownames(data) <- NULL
      return(data)
    }
  }
  data <- read.csv(file_path, ...)
  tryCatch({
    data_to_save <- data
    rownames(data_to_save) <- NULL
    saveRDS(data_to_save, rds_path, compress = TRUE)
  }, error = function(e) {})
  return(data)
}

# Helper: Preprocessing
app_preprocess <- function(m_reg, info_df, mwi, app_start = T){
  meas_col_to_type <- setNames(m_reg$Category, m_reg$Measure)
  meas_col_to_type["Mental Wellness Index"] <- "Mental Wellness Index"
  
  avail_measures <- measure_to_names <- avail_meas_w_weights <- list()
  avail_meas_list <- m_to_type <- list()
  
  for (idx in index_types){
    avail_measures[[idx]] <- colnames(mwi[[idx]])[-1]
    names(avail_measures[[idx]]) <- c("Mental Wellness Index", 
                                      m_reg[gsub("*_pop$","", gsub("*_black$","",colnames(mwi[[idx]])[-c(1:2)])), "Measure"])
    
    measure_to_names[[idx]] <- setNames(names(avail_measures[[idx]]), avail_measures[[idx]])
    avail_meas_list[[idx]] <- list()
    m_to_type[[idx]] <- meas_col_to_type[measure_to_names[[idx]][avail_measures[[idx]]]]
    
    avail_meas_w_weights[[idx]] <- avail_measures[[idx]]
    names(avail_meas_w_weights[[idx]]) <- paste0(names(avail_measures[[idx]]),
                                                 " (Weight: ", round(info_df[avail_measures[[idx]], "Effective_Weights"], 2), ")")
    names(avail_meas_w_weights[[idx]])[1] <- names(avail_measures[[idx]])[1]
    
    for (t in unique(m_to_type[[idx]])){
      avail_meas_list[[idx]][[t]] <- avail_meas_w_weights[[idx]][m_to_type[[idx]] == t]
    }
  }
  
  if ((app_start & !"HSE_MWI_ZCTA_full_shapefile_US.RData" %in% list.files(file.path(data_folder, "Cleaned")))){
    for (idx in index_types){
      mwi[[idx]][, colnames(cty_cw)[-1]] <- cty_cw[mwi[[idx]]$ZCTA, -1]
    }
    zips <- zctas(cb = T, year = 2020)
    colnames(zips)[colnames(zips) == "GEOID20"] <- "GEOID"
    zips <- zips[zips$GEOID %in% mwi$pop$ZCTA,]
    zips <- st_transform(zips, crs = "+proj=longlat +datum=WGS84")
    
    geodat <- geopts <- list()
    for (idx in index_types){
      geodat[[idx]] <- left_join(zips, mwi[[idx]], by = c("GEOID" = "ZCTA"))
      geodat[[idx]] <- geodat[[idx]][order(geodat[[idx]]$STATE, geodat[[idx]]$GEOID),]
      geopts[[idx]] <- suppressWarnings(st_centroid(geodat[[idx]]))
    }
    if (app_start & !"HSE_MWI_ZCTA_full_shapefile_US.RData" %in% list.files(file.path(data_folder, "Cleaned"))){
      save(list = c("geodat", "geopts"), file = file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
    }
  } else if ("HSE_MWI_ZCTA_full_shapefile_US.RData" %in% list.files(file.path(data_folder, "Cleaned"))){
    load(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_full_shapefile_US.RData"))
  }
  
  avail_zctas <- geodat[["pop"]]$GEOID
  names(avail_zctas) <- paste0(geodat[["pop"]]$GEOID, " (State: ", geodat[["pop"]]$STATE_NAME, ")")
  
  if (app_start){
    return(list(meas_col_to_type = meas_col_to_type, avail_measures = avail_measures,
                measure_to_names = measure_to_names, avail_meas_list = avail_meas_list,
                geodat = geodat, geopts = geopts))
  } else {
    return(list(meas_col_to_type = meas_col_to_type, avail_measures = avail_measures,
                measure_to_names = measure_to_names, avail_meas_list = avail_meas_list))
  }
}

# Helper: Plotting Functions
plot_map <- function(fill, geodat, idx, ol, is_all = F, is_com = F, fill_opacity = .7, add_poly = F, us_proxy = NA, zcta_choose = NA){
  gd_map <- geodat[,c(fill, "GEOID", "STATE", "STATE_NAME", "geometry")]
  colnames(gd_map)[1] <- "Fill"
  if (fill != "Mental_Wellness_Index"){
    gd_map$Fill <- ol$no_dir_perc_meas_df[gd_map$GEOID, fill]
  }
  gd_map[, colnames(all_pop_df)[-c(1:2)]] <- all_pop_df[gd_map$GEOID, -c(1:2)]
  gd_map <- gd_map[!is.na(gd_map$GEOID),]
  
  pal <- colorNumeric(palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
                      domain = c(0, gd_map$Fill, 100), na.color = "transparent", reverse = ifelse(fill == "Score", T, F))
  pal_wo_na <- colorNumeric(palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]],
                            domain = c(0, gd_map$Fill, 100), na.color=rgb(0,0,0,0), reverse = ifelse(fill == "Score", T, F))
  
  full_name <- ol$measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){ full_name <- paste0(full_name, " Ranking") }
  
  labels <- paste0("State: ", gd_map$STATE_NAME, "<br>ZCTA: ", gd_map$GEOID, "<br>ZIP Code: ", unname(zcta_to_zip[gd_map$GEOID]), "<br>Population: ", as.data.frame(gd_map)[, paste0("total_",idx)], "<br>", full_name,": ", trunc(gd_map$Fill)) %>% lapply(htmltools::HTML)
  
  bounds <- if (!is_com){ unname(st_bbox(geodat)) } else { unname(st_bbox(geodat[geodat$GEOID == zcta_choose,])) }
  
  if (!add_poly){
    mp <- if (!is_all){
      leaflet(data = gd_map) %>% addProviderTiles("CartoDB") %>%
        addPolygons(fillColor = ~pal(Fill), weight = 1, opacity = 1, color = "#b2aeae", dashArray = "", fillOpacity = fill_opacity, layerId = ~GEOID,
                    highlight = highlightOptions(weight = 2, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = !is_com), label = labels) %>%
        addLegend(pal = pal_wo_na, values = ~c(0, Fill, 100), opacity = 0.7, position = "bottomright", title = unname(full_name)) %>%
        fitBounds(lng1 = bounds[1], lng2 = bounds[3], lat1 = bounds[2], lat2 = bounds[4])
    } else {
      leaflet(data = (gd_map)) %>% addProviderTiles("CartoDB") %>%
        addCircleMarkers(fillColor = ~pal(Fill), weight = 1, opacity = 1, color = ~pal(Fill), dashArray = "", fillOpacity = fill_opacity, layerId = ~GEOID, label = labels, radius = 5) %>%
        addLegend(pal = pal_wo_na, values = ~c(0, Fill, 100), opacity = 0.7, position = "bottomright", title = unname(full_name)) %>%
        fitBounds(lng1 = bounds[1], lng2 = bounds[3], lat1 = bounds[2], lat2 = bounds[4])
    }
    if (is_com){
      zcta_select <- gd_map[gd_map$GEOID == zcta_choose,]
      mp <- mp %>% addPolygons(data = zcta_select, fillColor = ~pal(Fill), weight = 4, opacity = 1, color = "#000", dashArray = "", fillOpacity = fill_opacity,
                               highlight = highlightOptions(weight = 4, color = "#000", dashArray = "", fillOpacity = 0.7, bringToFront = T), label = labels[gd_map$GEOID == zcta_choose])
    }
  } else {
    zcta_select <- gd_map[gd_map$GEOID == zcta_choose,]
    mp <- if (!is_all){
      us_proxy %>% addPolygons(data = zcta_select, fillColor = ~pal(Fill), weight = 4, opacity = 1, color = "#000", dashArray = "", fillOpacity = fill_opacity, layerId = "remove_me",
                               highlight = highlightOptions(weight = 4, color = "#000", dashArray = "", fillOpacity = 0.7, bringToFront = T), label = labels[gd_map$GEOID == zcta_choose])
    } else {
      us_proxy %>% addCircleMarkers(data = zcta_select, fillColor = ~pal(Fill), weight = 4, opacity = 1, color = "#000", dashArray = "", fillOpacity = 1, layerId = "remove_me",
                                    label = labels[gd_map$GEOID == zcta_choose], radius = 7)
    }
  }
  return(mp)
}

plot_bee_distr <- function(fill, st, mwi, idx, ol, is_all = F, hl = F, zcta_hl = ""){
  bee.df <- data.frame(val = mwi[,fill], zcta = mwi$ZCTA, lab = rep("val", nrow(mwi)), focus = rep("val", nrow(mwi)), focus_alpha = rep(1, nrow(mwi)))
  bee.df <- bee.df[complete.cases(bee.df),]
  if (fill != "Mental_Wellness_Index"){ bee.df$val <- ol$no_dir_perc_meas_df[bee.df$zcta, fill] }
  
  if (hl){
    row_hl <- which(bee.df$zcta == zcta_hl)
    bee.df$focus[row_hl] <- "Focus"
    bee.df$focus_alpha[-row_hl] <- .3
    pal <- colorNumeric(palette = meas_colors[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]], domain = c(0, bee.df$val, 100), na.color = "transparent")
    hl_pal <- c("val" = "#e3e3e3", "Focus" = pal(bee.df[row_hl, fill]))
    hl_size <- c("val" = 1.5, "Focus" = 3)
  }
  
  p <- if (hl){
    ggplot(bee.df, aes(lab, val, color = val, size = focus))+
      scale_color_gradientn(colors = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](100), limits = c(0, 100))+
      scale_size_manual(values = hl_size)
  } else {
    ggplot(bee.df, aes(lab, val, color = val), size = 1.5)+
      scale_color_gradientn(colors = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](100), limits = c(0, 100))
  }
  
  full_name <- ol$measure_to_names[[idx]][fill]
  if (full_name != "Mental Wellness Index"){ full_name <- paste0(full_name, " Ranking") }
  
  p <- suppressWarnings(p + theme_bw()+ ylab(full_name)+ theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", plot.title = element_text(hjust = .5))+ ylim(-3, 103)+
                          if (!is_all){ geom_quasirandom(aes(text = paste0("ZCTA: ", zcta, "\n", "ZIP Code: ", unname(zcta_to_zip[zcta]), "<br>", full_name, ": ", trunc(val))), dodge.width = NULL, alpha = bee.df$focus_alpha)
                          } else { geom_violin(fill = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](3)[2], color = meas_colors_pal[[ol$meas_col_to_type[ol$measure_to_names[[idx]][fill]]]](3)[2]) })
  
  ggplotly(p, tooltip = c("text")) %>% layout(margin = list(l = 0, r = 10, b = 0, t = 0)) %>% config(displayModeBar = F)
}

quant_map <- function(perc){
  if (perc < 34) { "bottom third" } else if (perc < 67){ "middle third" } else { "top third" }
}

html_color <- function(meas_color, text){
  return(paste0("<font color =", meas_color,">",text,"</font>"))
}

# 5. Data Loading Pipeline ----
mwi_toolkit_order <- c("MWI_Overview", "MWI_Populations_of_Focus", "MWI_Framework", "MWI_Measures_and_Data", "MWI_Tool_Videos_and_Guides",
                       "Share_the_MWI_With_Others", "The_Science_Behind_the_MWI", "MWI_in_Action", "Frequently_Asked_Questions", "Contact")
index_types <- c("Population" = "pop", "Black" = "black")
data_folder <- file.path("Data")

m_reg <- as.data.frame(read_excel(file.path(data_folder, "Metadata.xlsx"), sheet = 1))
m_reg <- m_reg[!is.na(m_reg$Numerator),]
rownames(m_reg) <- m_reg$Numerator
sub_m <- m_reg[, c("Measure", "Category", "Weights", "Weights")]
colnames(sub_m)[ncol(sub_m)-1] <- "Original Weights"
colnames(sub_m)[ncol(sub_m)] <- "Updated Weights"
rownames(sub_m) <- rownames(m_reg)

info_df <- read_csv_cached(file.path(data_folder, "Cleaned", "HSE_MWI_Data_Information.csv"))
rownames(info_df) <- info_df$Numerator

meas_df <- read_csv_cached(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_Converted_Measures.csv"), colClasses = c("GEOID" = "character"))
meas_df <- meas_df[meas_df$GEOID != "",]
rownames(meas_df) <- meas_df$GEOID

zip_cw <- read.csv(file.path(data_folder, "Resources", "Zip_to_zcta_crosswalk_2021.csv"), colClasses = c("ZIP_CODE" = "character", "ZCTA" = "character"))
territories <- c("AS", "FM", "GU", "MH", "MP", "PW", "PR", "VI")
zip_cw <- zip_cw[!zip_cw$STATE %in% territories,]
zip_to_zcta <- setNames(zip_cw$ZCTA, zip_cw$ZIP_CODE)
zcta_to_zip <- aggregate(ZIP_CODE ~ ZCTA, data = zip_cw, FUN = function(x){paste(x, collapse = ", ")})
zcta_to_zip <- setNames(zcta_to_zip$ZIP_CODE, zcta_to_zip$ZCTA)

county_cw <- read.csv(file.path(data_folder, "Resources", "zcta_county_rel_20.csv"), colClasses = c("ZCTA5" = "character", "GEOID" = "character"))
county_cw$STATE <- substr(county_cw$GEOID, 1, 2)
county_cw$COUNTY <- substr(county_cw$GEOID, 3, 5)
cty_cw <- aggregate(STATE ~ ZCTA5, data = county_cw, FUN = function(x){paste(x, collapse = "|")})
rownames(cty_cw) <- cty_cw$ZCTA5
un_st <- lapply(strsplit(cty_cw$STATE, "|", fixed = T), unique)
cty_cw$STATE <- sapply(un_st, `[`, 1)
cty_cw$STATE_2 <- sapply(un_st, `[`, 2)

data("fips_codes")
f_st <- setNames(unique(fips_codes$state_name), unique(fips_codes$state_code))
f_st <- f_st[f_st %in% c(state.name, "District of Columbia")]
st_to_fips <- setNames(names(f_st), f_st)
cty_cw$STATE_NAME <- f_st[cty_cw$STATE]
cty_cw$COUNTY <- aggregate(COUNTY ~ ZCTA5, data = county_cw, FUN = function(x){paste(x, collapse = "|")})[,2]
cty_cw$GEOID <- aggregate(GEOID ~ ZCTA5, data = county_cw, FUN = function(x){paste(x, collapse = "|")})[,2]

no_dir_perc_meas_df <- read_csv_cached(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv"), colClasses = c("GEOID" = "character"))
colnames(no_dir_perc_meas_df)[colnames(no_dir_perc_meas_df) == "GEOID"] <- "ZCTA"
no_dir_perc_meas_df <- no_dir_perc_meas_df[no_dir_perc_meas_df$ZCTA != "",]
rownames(no_dir_perc_meas_df) <- no_dir_perc_meas_df$ZCTA

mwi <- list()
mwi[["pop"]] <- read_csv_cached(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv"), colClasses = c("ZCTA" = "character"))
mwi[["pop"]] <- mwi[["pop"]][mwi[["pop"]]$ZCTA != "",]
mwi[["black"]] <- read_csv_cached(file.path(data_folder, "Cleaned", "HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv"), colClasses = c("ZCTA" = "character"))
mwi[["black"]] <- mwi[["black"]][mwi[["black"]]$ZCTA != "",]

st_abbrev_to_full <- c(state.name, "District of Columbia", "All States")
names(st_abbrev_to_full) <- c(state.abb, "DC", "All")

all_pop_df <- read.csv(file.path(data_folder, "Resources", "ACS_ZCTA_Total_Populations.csv"), colClasses = c("GEOID" = "character"))
all_pop_df$perc_black <- all_pop_df$total_black/all_pop_df$total_pop*100
all_pop_df$perc_pop <- 100
rownames(all_pop_df) <- all_pop_df$GEOID

# 6. Colors ----
meas_colors <- c("purples", "greens", "blues", "purple_blue_green")
names(meas_colors) <- c(unique(m_reg$Category), "Mental Wellness Index")
meas_max_colors <- c("#5d499e", "#157ba7", "#70ad47", "#00441b")
meas_min_colors <- c("#fcfbfd", "#f7fbff", "#f7fcf5", "#3f157d")
names(meas_max_colors) <- names(meas_min_colors) <- names(meas_colors)

meas_colors <- lapply(1:length(meas_min_colors), function(x){
  if (x != length(meas_min_colors)){ colorRamp(c(meas_min_colors[x], meas_max_colors[x]), interpolate = "linear")
  } else { colorRamp(c(meas_min_colors[x], "#c6dbef", meas_max_colors[x]), interpolate = "linear") }
})
meas_colors_pal <- lapply(1:length(meas_min_colors), function(x){
  if (x != length(meas_min_colors)){ colorRampPalette(c(meas_min_colors[x], meas_max_colors[x]), interpolate = "linear")
  } else { colorRampPalette(c(meas_min_colors[x], "#c6dbef", meas_max_colors[x]), interpolate = "linear") }
})
names(meas_colors) <- names(meas_colors_pal) <- names(meas_max_colors)
meas_max_colors["Mental Wellness Index"] <- meas_colors_pal[["Mental Wellness Index"]](7)[6]
meas_min_colors["Mental Wellness Index"] <- meas_colors_pal[["Mental Wellness Index"]](7)[2]

# 7. Final Data Prep ----
overall <- app_preprocess(m_reg, info_df, mwi, app_start = T)
overall[["m_reg"]] <- m_reg
overall[["meas_df"]] <- meas_df
overall[["info_dat"]] <- info_df
overall[["no_dir_perc_meas_df"]] <- no_dir_perc_meas_df

for (idx in index_types){
  mwi[[idx]][, colnames(cty_cw)[-1]] <- cty_cw[mwi[[idx]]$ZCTA, -1]
}
overall[["mwi"]] <- mwi
