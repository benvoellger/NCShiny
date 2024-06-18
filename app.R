#########################################################################
#                               NCShiny                                 #
#          a shiny app to visualize hospital management data            #
# case study: relaunch of a neurosurgical department in central Germany #
#                                                                       #
#          (c) 2024 Benjamin Voellger, under the MIT license            #
#########################################################################

#load dependencies
library(shiny)            #the stuff underneath
library(sf)               #simple features for R to deal with spatial vector data
library(osmdata)          #open street map
library(mapview)          #more map stuff 
library(leaflet)          #even more map stuff
library(pheatmap)         #pretty heatmaps
library(viridis)          #the paint bucket
library(data.table)       #contains the fread function

#read your data
catchment_data <- read.csv("./data/catchment_data.csv", sep = ";")
hospitals <- read.csv("./gis_data/hospitals.csv", sep = ";")
kpi_data <- read.csv("./data/kpi_data.csv", sep = ";")
icd_data <- t(read.csv("./data/icd_data.csv", sep = ";"))
ops_data <- t(fread("./data/ops_data.csv", sep = ";")) #to avoid nonsense in column headers, use fread here instead of read.csv 
feedback_data <- read.csv("./data/feedback_data.csv", sep = ";")

#to download and unpack a .zip container with shapefiles of German administrative units at several levels
#(level 0 - country; level 1 - federal states; level 2 - governmental districts ("Regierungsbezirke"); level 3 - districts ("Landkreise"))
#leave the following two lines uncommented at the first run of the app:
#download.file(url = "http://biogeo.ucdavis.edu/data/diva/adm/DEU_adm.zip", destfile = "./gis_data/germany.zip")
#unzip(zipfile = "./gis_data/germany.zip")

#read the unpacked level 3 (districts) shapefile:
german_districts <-read_sf("./gis_data/DEU_adm3.shp")

#to calculate travelling time polygons with the help of openrouteservice.org and to save the polygons as shapefiles
#register with openrouteservice.org, request a token there, and
#leave the following three lines uncommented at the first run of the app (modify and repeat ors_isochrones() and saveRDS() calls according to your needs):
#library(openrouteservice)
#yourPolygon <- ors_isochrones(c(yourLongitude, yourLatitude), profile = "driving-car", range = yourDrivingTimeInSeconds, api_key = "yourTokenGoesHere", output = "sf")
#saveRDS(yourPolygon, "yourDestinationFile.shape")

#read previously calculated shapefiles representing travelling times to index and neighbouring hospitals
#30 minutes by car
travellingTimeHEF1800 <- readRDS("./gis_data/dtHEF_1800s_car.shape")
travellingTimeAPD1800 <- readRDS("./gis_data/dtAPD_1800s_car.shape")
travellingTimeEF1800 <- readRDS("./gis_data/dtEF_1800s_car.shape")
travellingTimeFD1800 <- readRDS("./gis_data/dtFD_1800s_car.shape")
travellingTimeGI1800 <- readRDS("./gis_data/dtGI_1800s_car.shape")
travellingTimeGOE1800 <- readRDS("./gis_data/dtGOE_1800s_car.shape")
travellingTimeKS1800 <- readRDS("./gis_data/dtKS_1800s_car.shape")
travellingTimeMR1800 <- readRDS("./gis_data/dtMR_1800s_car.shape")

#60 minutes by car
travellingTimeHEF3600 <- readRDS("./gis_data/dtHEF_3600s_car.shape")
travellingTimeAPD3600 <- readRDS("./gis_data/dtAPD_3600s_car.shape")
travellingTimeEF3600 <- readRDS("./gis_data/dtEF_3600s_car.shape")
travellingTimeFD3600 <- readRDS("./gis_data/dtFD_3600s_car.shape")
travellingTimeGI3600 <- readRDS("./gis_data/dtGI_3600s_car.shape")
travellingTimeGOE3600 <- readRDS("./gis_data/dtGOE_3600s_car.shape")
travellingTimeKS3600 <- readRDS("./gis_data/dtKS_3600s_car.shape")
travellingTimeMR3600 <- readRDS("./gis_data/dtMR_3600s_car.shape")

#calculate travelling time (30 minutes by car) polygons (with overlap, where applicable) for neighbouring hospitals, remove nasty glitches from the matrix
#caution: with large polygons, it is recommended not to exceed 3 iterations of st_union() calls
travellingTimeOtherHospitalsHesse1800 <-
  st_union(
    st_union(
      st_union(st_make_valid(st_set_precision(travellingTimeFD1800, 1e6)),
                  st_make_valid(st_set_precision(travellingTimeGI1800, 1e6))),
                    st_make_valid(st_set_precision(travellingTimeKS1800, 1e6))),
                      st_make_valid(st_set_precision(travellingTimeMR1800, 1e6)))

travellingTimeOtherHospitalsElsewhere1800 <-
  st_union(
    st_union(st_make_valid(st_set_precision(travellingTimeAPD1800, 1e6)),
                st_make_valid(st_set_precision(travellingTimeEF1800, 1e6))),
                  st_make_valid(st_set_precision(travellingTimeGOE1800, 1e6)))

#calculate travelling time (60 minutes by car) polygons with overlap for neighbouring hospitals, remove nasty glitches from the matrix
travellingTimeOtherHospitalsHesse3600 <-
  st_union(
    st_union(
      st_union(st_make_valid(st_set_precision(travellingTimeFD3600, 1e5)),
                  st_make_valid(st_set_precision(travellingTimeGI3600, 1e5))),
                    st_make_valid(st_set_precision(travellingTimeKS3600, 1e5))),
                      st_make_valid(st_set_precision(travellingTimeMR3600, 1e5)))

travellingTimeOtherHospitalsElsewhere3600 <-
  st_union(
    st_union(st_make_valid(st_set_precision(travellingTimeAPD3600, 1e5)),
                st_make_valid(st_set_precision(travellingTimeEF3600, 1e5))),
                  st_make_valid(st_set_precision(travellingTimeGOE3600, 1e5)))

#define frontend
ui <- navbarPage(
  fluid = TRUE,
  #set theme
  theme = bslib::bs_theme(bootswatch = "lux"),
  #set page title
  title = div(h4("Case Study"), h6("Resuming a Neurotrauma and"), h6("Spinal Surgery Service in"), h6("Central Germany")),
  #set tab title
  windowTitle = "Hospital Management Data Goes Shiny",
  #define menu
  navbarMenu(title = "Menu",
    #catchment area map
    tabPanel("Catchment Area",
             title = "Catchment Area",
             sliderInput("month", label = "Month (1 = April 2022):", value = 5, min = 1, max = 24, width = 500),
             #draw map, print legend
             leafletOutput("map"),
             textOutput("map_legend"),
             tags$head(tags$style("#map_legend{color: black;
                                 font-size: 10px;
                                 font-style: normal;
                                 }"
             )
             )
    ),
    
    #key performance indicators
    tabPanel("Key Performance Indicators",
             title = "Key Performance Indicators",
             sliderInput("kpi_rng", label = "Months (1 = April 2022):", value = c(1, 24), min = 1, max = 24, width = 500),
             #draw kpi figures
             plotOutput("val_rat", height = 300),
             plotOutput("pats", height = 300),
             plotOutput("cmi", height = 300),
             plotOutput("los", height = 300)
    ),
    
    #top diagnoses and procedures
    tabPanel("Top Diagnoses and Procedures",
             title = "Top Diagnoses and Procedures",
             sliderInput("t_rng", label = "Months (1 = April 2022):", value = c(13, 24), min = 1, max = 24, width = 500),
             radioButtons("dgn_or_proc", label = "", choices = c("Diagnoses", "Procedures"), selected = "Diagnoses", inline = TRUE),
             #draw figures
             conditionalPanel(
               condition = "input.dgn_or_proc == 'Diagnoses'",
               plotOutput("icdmap")
             ),
             conditionalPanel(
               condition = "input.dgn_or_proc == 'Procedures'",
               plotOutput("opsmap")
             ),
             htmlOutput("dgn_proc_legend"),
             tags$head(tags$style("#dgn_proc_legend{color: black;
                                 font-size: 10px;
                                 font-style: normal;
                                 }"
             )
             )
    ),
    
    #patient feedback
    tabPanel("Feedback",
             title = "Feedback",
             sliderInput("fb_rng", label = "Months (1 = April 2022):", value = c(-2, 20), min = -2, max = 24, width = 500),
             #generate a contingency table from the slider input values, print the corresponding p value
             textOutput("ctable_fb"),
             sliderInput("time_fb", label = "Cutoff (Months after Service Resumption (1 = April 2022)):", value = 0, min = -2, max = 24, width = 500),
             sliderInput("eval", label = "Cutoff (Overall Evaluation Below Selected Grade):", value = 3, min = 0, max = 6, width = 500, ticks = FALSE),
             textOutput("pvalue_fb"),
             #draw the feedback figure
             plotOutput("fb", height = 500),
             htmlOutput("feedback_legend"),
             tags$head(tags$style("#feedback_legend{color: black;
                                 font-size: 10px;
                                 font-style: normal;
                                 }"
             )
             )
    )
  )
)

#define backend
server <- function(input, output, session){

  #catchment area
  #read month value from slider
  mo <- reactive(input$month)
  #setup a map, centered at the index hospital, on a slick background, with a metric scalebar
  m <- mapview()@map %>%
    leaflet(german_districts) %>%
    setView(lng = (hospitals$long[hospitals$loc == "HEF"]), lat = (hospitals$lat[hospitals$loc == "HEF"]), zoom = 7) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE))
  #fill map
  output$map <- renderLeaflet({
    #purple: index hospital catchment area; color intensity depends on the number of admissions per district and month
    for (i in (2:(ncol(catchment_data)-1))) {
      m <- addPolygons(
        group = "Index Hospital Catchment Area",
        m,
        data = (german_districts$geometry[german_districts$ID_3 == catchment_data[1, i]]),
        color = ("#440154FF"), opacity = 0, fillOpacity = catchment_data[mo()+1, i]/3, smooth = 0.1)
    }
    m %>%
    #blue: access to neighbouring hospitals by car
    addPolygons(
        group = "Neighbouring Hospitals (Hesse)",
        data = travellingTimeOtherHospitalsHesse1800,
        color ="#39568CFF", opacity = 0.3, weight = 0.3, fillOpacity = 0.3
    ) %>%
    addPolygons(
        group = "Neighbouring Hospitals (Hesse)",
        data = travellingTimeOtherHospitalsHesse3600,
        color ="#39568CFF", opacity = 0.3, weight = 0.3, fillOpacity = 0.3
    ) %>%
    addPolygons(
        group = "Neighbouring Hospitals (Other)",
        data = travellingTimeOtherHospitalsElsewhere1800,
        color ="#39568CFF", opacity = 0.3, weight = 0.3, fillOpacity = 0.3
    ) %>%
    addPolygons(
        group = "Neighbouring Hospitals (Other)",
        data = travellingTimeOtherHospitalsElsewhere3600,
        color ="#39568CFF", opacity = 0.3, weight = 0.3, fillOpacity = 0.3
    ) %>%  
    #green: access to index hospital by car  
    addPolygons(
        group="Index Hospital",
        data = travellingTimeHEF1800,
        color ="#73D055FF", opacity = 0.3, weight = 0.3, fillOpacity = 0.3
    ) %>%
    addPolygons(
        group="Index Hospital",
        data = travellingTimeHEF3600,
        color ="#73D055FF", opacity = 0.3, weight = 0.3, fillOpacity = 0.3
      ) %>%
    #mark hospitals  
    addAwesomeMarkers(group = (hospitals$class[hospitals$loc == "HEF"]), lat = (hospitals$lat[hospitals$loc == "HEF"]), lng = (hospitals$long[hospitals$loc == "HEF"]), label = (hospitals$alt[hospitals$loc == "HEF"]), icon = awesomeIcons(library = "fa", icon = "hospital-o", iconColor = "#440154FF", markerColor = "white")) %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "APD"]), lat = (hospitals$lat[hospitals$loc == "APD"]), lng = (hospitals$long[hospitals$loc == "APD"]), label = (hospitals$alt[hospitals$loc == "APD"]), color = "#000000") %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "EF"]), lat = (hospitals$lat[hospitals$loc == "EF"]), lng = (hospitals$long[hospitals$loc == "EF"]), label = (hospitals$alt[hospitals$loc == "EF"]), color = "#000000") %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "FD"]), lat = (hospitals$lat[hospitals$loc == "FD"]), lng = (hospitals$long[hospitals$loc == "FD"]), label = (hospitals$alt[hospitals$loc == "FD"]), color = "#000000") %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "GI"]), lat = (hospitals$lat[hospitals$loc == "GI"]), lng = (hospitals$long[hospitals$loc == "GI"]), label = (hospitals$alt[hospitals$loc == "GI"]), color = "#000000") %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "GOE"]), lat = (hospitals$lat[hospitals$loc == "GOE"]), lng = (hospitals$long[hospitals$loc == "GOE"]), label = (hospitals$alt[hospitals$loc == "GOE"]), color = "#000000") %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "MR"]), lat = (hospitals$lat[hospitals$loc == "MR"]), lng = (hospitals$long[hospitals$loc == "MR"]), label = (hospitals$alt[hospitals$loc == "MR"]), color = "#000000") %>%
    addCircleMarkers(group = (hospitals$class[hospitals$loc == "KS"]), lat = (hospitals$lat[hospitals$loc == "KS"]), lng = (hospitals$long[hospitals$loc == "KS"]), label = (hospitals$alt[hospitals$loc == "KS"]), color = "#000000") %>%
    addLayersControl(
      overlayGroups = c("Index Hospital", "Index Hospital Catchment Area", "Neighbouring Hospitals (Hesse)", "Neighbouring Hospitals (Other)"),
      options = layersControlOptions(collapsed = TRUE, position = "topright")
    ) %>%
    #place comments after the bracket above and at the beginning of the following two lines to start with excess visuals
    hideGroup("Neighbouring Hospitals (Hesse)") %>%
    hideGroup("Neighbouring Hospitals (Other)")
  })
  #define legend
  output$map_legend <- renderText({
    "Index hospital (white pin) catchment area (purple) for neurosurgical patients, color intensity increases with admissions. Access by car (30, 60 minutes) to index hospital (green), color intensity decreases with travelling time. Not visible by default: Selection of neighbouring hospitals with equal or higher levels of service (black circles), and access by car (30, 60 minutes) to neighbouring hospitals (blue). Expand checkboxes in the upper right corner of the map to toggle layer visibility."
  })
  
  #key performance indicators
  #read month values from slider
  k1 <- reactive(min(input$kpi_rng))
  k2 <- reactive(max(input$kpi_rng))
  #setup key performance indicator figures
  output$val_rat <- renderPlot({
    plot(
      kpi_data$month, kpi_data$val_ratios,
      xlab = ("Months after Service Resumption"), ylab = "Reimbursed Valuation Ratios", xlim = c(k1(), k2()),
      col = "#73D055FF", lwd = 4, type = "l"
    )
  })
  output$pats <- renderPlot({
    plot(kpi_data$month, kpi_data$patients, xlab = ("Months after Service Resumption"), ylab = "Number of Patients", xlim = c(k1(), k2()), col = "#1F968BFF", lwd = 4, type = "l")
  })
  output$cmi <- renderPlot({
    plot(kpi_data$month, kpi_data$val_ratios/kpi_data$patients, xlab = ("Months after Service Resumption"), ylab = "Case Mix Index", xlim = c(k1(), k2()), col = "#440154FF", lwd = 4, type = "l")
  })
  output$los <- renderPlot({
    plot(kpi_data$month, kpi_data$mean_los, xlab = ("Months after Service Resumption"), ylab = "Mean Length of Stay (Days)", xlim = c(k1(), k2()), col = "#39568CFF", lwd = 4, type = "l")
  })
  
  #top diagnoses and procedures
  #read month values from slider
  t1 <- reactive(min(input$t_rng))
  t2 <- reactive(max(input$t_rng))
  #setup heatmaps
  output$icdmap <- renderPlot({
    mat <- as.matrix(icd_data[2:nrow(icd_data), t1():t2()])
    colnames(mat) <- c(t1():t2())
    pheatmap(
      mat[rowSums(is.na(mat)) != ncol(mat),],
      color = viridis(((t2()-t1())*(nrow(mat)))+24),
      angle_col = 0,
      cluster_rows = FALSE,
      cluster_cols = FALSE)
  })
  output$opsmap <- renderPlot({
    mat <- as.matrix(ops_data[2:nrow(ops_data), t1():t2()])
    colnames(mat) <- c(t1():t2())
    pheatmap(
      mat[rowSums(is.na(mat)) != ncol(mat),],
      color = viridis(((t2()-t1())*(nrow(mat)))+24),
      angle_col = 0,
      cluster_rows = FALSE,
      cluster_cols = FALSE)
  })
  #define legend
  output$dgn_proc_legend <- renderText({
    "Color represents number of patients."
  })
  
  #patient feedback
  #read month values from top slider
  f1 <- reactive(min(input$fb_rng))
  f2 <- reactive(max(input$fb_rng))
  #define header for cutoff sliders
  output$ctable_fb <- renderText({
    "Contingency Table (Cutoff Values = Dashed Lines in the Figure)"
  })
  #read cutoff values from bottom sliders
  cut_time_fb <- reactive(input$time_fb)
  cut_eval <- reactive (input$eval)
  #calculate p value as defined by cutoff values, extract p value, round it to two decimal places
  pval_fb <- reactive(round(fisher.test(table(feedback_data$month<cut_time_fb(), feedback_data$feedback<cut_eval()))$p.value, 2))
  #hand over p value
  output$pvalue_fb <- renderText({
    paste("Fisher's Exact Test: p = ", pval_fb())
  })
  #setup feedback figure
  output$fb <- renderPlot({
    plot(
      feedback_data$month, feedback_data$feedback,
      xlab = ("Months after Service Resumption"), ylab = "Overall Evaluation (Grades: 0 = Worst, 6 = Best Possible)",
      xlim = c(f1(), f2()), ylim = c(0, 6),
      col = "#73D055FF", lwd = 5*feedback_data$freq, type = "p"
    )
    #setup two dashed lines for cutoff values
    abline(v = cut_time_fb(), lty = 2, col = "grey")
    abline(h = cut_eval(), lty = 2, col = "grey")
  })
  #define legend
  output$feedback_legend <- renderText({
    "Online feedback from patients as received at www.klinikbewertungen.de (circle diameter increases with evaluation count)."
  })
}

#run app
shinyApp(ui, server)