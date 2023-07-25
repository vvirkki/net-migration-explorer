# ------------------------------------------------------------------------------
#
# Net-migration explorer
# Shiny R app displaying the data of:
# 
# Niva Venla, Horton Alexander, Virkki Vili, Heino Matias, Kosonen Maria,
# Kallio Marko, Kinnunen Pekka, Abel Guy J, Muttarak Raya, Taka Maija,
# Varis Olli, Kummu Matti
# 
# "World's human migration patterns in 2000–2019 unveiled by high-resolution data"
#
# Published in [FILL DETAILS UPON PUBLICATION]
# DOI: [FILL DOI UPON PUBLICATION]
# 
# Corresponding authors of the article:
# Venla Niva (venla.niva@aalto.fi)
# Matti Kummu (matti.kummu@aalto.fi)
# 
# Script author:
# Vili Virkki (vili.virkki@aalto.fi)
#
# ------------------------------------------------------------------------------

# ENVIRONMENT ------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinybusy)
library(leaflet)
library(RColorBrewer)
library(xts)
library(readr)
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)

# HELPERS ----------------------------------------------------------------------

# helper function for choropleth mapping
# reference:
# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){

  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))

  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

# helper function in JS for choropleth mapping
# reference:
# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
leafletjs <- tags$head(tags$script(HTML('

window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  )))

clickPrevention <- tags$head(tags$style(HTML("
  .prevent_click {
  position:fixed;
  z-index:9;
  width:100%;
  height:100%;
  background-color:transparent;
  }"
)))

# DATA -------------------------------------------------------------------------
# For masterData.rds and admGeoms.rds, please refer to:
# http://doi.org/10.5281/zenodo.7997134

appData <- readRDS("masterData.rds")
adminGeomsList <- readRDS("admGeoms.rds")

# STATIC VISUALS FOR ALL MAPS  -------------------------------------------------

# bins for color classification
paletteBinsDiv <- c(-Inf, -30, -15, -7.5, 0, 7.5, 15, 30, Inf)
paletteBinsSeq <- c(0, 7.5, 15, 30, Inf)

# color palettes for polygons
palDiv <- colorBin(palette = "RdBu",
                   domain = c(-Inf, Inf),
                   na.color = "transparent",
                   bins = paletteBinsDiv,
                   reverse = FALSE)

palDivLegend <- colorBin(palette = "RdBu",
                         domain = c(-Inf, Inf),
                         na.color = "transparent",
                         bins = paletteBinsDiv,
                         reverse = TRUE)

palSeqReds <- colorBin(palette = "Reds",
                       domain = c(0, Inf),
                       na.color = "transparent",
                       bins = paletteBinsSeq,
                       reverse = FALSE)

palSeqRedsLegend <- colorBin(palette = "Reds",
                             domain = c(0, Inf),
                             na.color = "transparent",
                             bins = paletteBinsSeq,
                             reverse = TRUE)

palSeqBlues <- colorBin(palette = "Blues",
                        domain = c(0, Inf),
                        na.color = "transparent",
                        bins = paletteBinsSeq,
                        reverse = FALSE)

palSeqBluesLegend <- colorBin(palette = "Blues",
                              domain = c(0, Inf),
                              na.color = "transparent",
                              bins = paletteBinsSeq,
                              reverse = TRUE)

colorPalettes <- list(palDiv, palSeqBlues, palSeqReds, palDiv) %>%
  setNames(c("net migration", "births", "deaths", "population change"))

colorPalettesLegend <- list(palDivLegend, palSeqBluesLegend, palSeqRedsLegend, palDivLegend) %>%
  setNames(c("net migration", "births", "deaths", "population change"))

linePlotPal <- c("#2171b5", "#cb181d", "black", "#6a51a3")
linePlotLabs <- c("birth rate", "death rate", "net migration rate", "population change rate")

# SHINY UI ---------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  leafletjs,
  clickPrevention,
  titlePanel("Net migration explorer"),
  sidebarPanel(width = 4,
               radioButtons(inputId = "mapType",
                            label = "Areal division",
                            choices = c("Country (admin 0)", "Provincial (admin 1)", "Communal (admin 2)"),
                            selected = "Country (admin 0)",
                            inline = FALSE),
               uiOutput(outputId = "varUI"),
               uiOutput(outputId = "dateUI"),
               strong("Click on an area for details"),
               br(),
               plotOutput(outputId = "plot"),
               actionButton(inputId = "clearSelection",
                            label = "clear selection")
  ),
  mainPanel(width = 8,
            leafletOutput("map", height = "750px")
  ),
  conditionalPanel(
    condition = "$(\'html\').hasClass(\'shiny-busy\')",
    tags$div(class = "prevent_click")
  ),
  add_busy_spinner(spin = "fading-circle", timeout = 200, position = "top-right")
)

# SHINY SERVER -----------------------------------------------------------------
server <- function(input, output, session) {

  epsg4326 <- leafletCRS(crsClass = "L.CRS.EPSG4326",
                         proj4def = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  areaSelected <- reactiveVal("")
  areaSelectedPrev <- reactiveVal("")

  # CREATE UI CONTROLS ---------------------------------------------------------
  observe({
    # dropdown menu for variable to display
    output$varUI <- renderUI({
      selectInput(inputId = "dispVar",
                  label = "Display variable",
                  choices = c("net migration", "births", "deaths", "population change"),
                  selected = "net migration")
    })
    # slider control for selecting data year
    output$dateUI <- renderUI({
      sliderInput(inputId = "selYear",
                  label = "Year",
                  min = 2000,
                  max = 2019,
                  value = 2000,
                  step = 1,
                  animate = animationOptions(interval = 500, loop = FALSE))
    })
    shinyjs::disable("clearSelection")
  })

  # FILTER DATA ----------------------------------------------------------------
  filteredData <- reactive({

    req(input$mapType, input$dispVar, input$selYear)
    # admin area division to display
    selScaleLevel <- case_when(
      input$mapType == "Country (admin 0)" ~ "adm0",
      input$mapType == "Provincial (admin 1)" ~ "adm1",
      input$mapType == "Communal (admin 2)" ~ "adm2"
    )
    # variable to display
    selVariable <- case_when(
      input$dispVar == "net migration" ~ "netMgr",
      input$dispVar == "births" ~ "birthRate",
      input$dispVar == "deaths" ~ "deathRate",
      input$dispVar == "population change" ~ "popChange"
    )
    # filter data for updating the map
    appData %>%
      filter(year == input$selYear & scaleLevel == selScaleLevel) %>%
      select(polygonId, year, !!selVariable, adminName) %>%
      rename(displayVar = !!selVariable) %>%
      mutate(label = paste0(adminName, "<br>", input$dispVar, " (", year, ")<br>",
                            sprintf("%.1f", displayVar), " / 1000 inhabitants"))

  })

  # CREATE BASE FOR THE MAP ----------------------------------------------------
  observe({

    # admin area division to display
    adminGeoms <- adminGeomsList[[input$mapType]]

    # draw base leaflet map; changes when areal division changes
    output$map <- renderLeaflet({
      leaflet(adminGeoms, options = leafletOptions(crs = epsg4326)) %>%
        setView(lat = 0,
                lng = 0,
                zoom = 2) %>%
        addMapPane("countryOutlines", zIndex = 500) %>%
        addMapPane("data", zIndex = 400) %>%
        addPolygons(data = adminGeomsList[[1]],
                    fill = FALSE,
                    stroke = TRUE,
                    color = "#7f7f7f",
                    weight = 1,
                    opacity = 1,
                    options = pathOptions(pane = "countryOutlines", interactive = FALSE)) %>%
        addPolygons(layerId = ~polygonId, # layerId must be of type character
                    fillColor = "lightgray",
                    stroke = TRUE,
                    fillOpacity = 1,
                    color = "white",
                    weight = 1,
                    options = pathOptions(pane = "data"))
    })
  })

  # CHANGE THE APPEARANCE OF THE MAP -------------------------------------------
  observe({

    # update polygons by areal division, display variable, and year
    adminGeoms <- adminGeomsList[[input$mapType]]
    adminGeoms$displayVar <- filteredData()$displayVar[match(adminGeoms$polygonId, filteredData()$polygonId)]
    adminGeoms$displayColor <- colorPalettes[[input$dispVar]](adminGeoms$displayVar)
    adminGeoms$displayColor[adminGeoms$polygonId == areaSelected()] <- "yellow"
    adminGeoms@data$labelText <- filteredData()$label

    leafletProxy("map", data = adminGeoms) %>%
      setShapeStyle(layerId = ~polygonId,
                    fillColor = ~displayColor,
                    label = adminGeoms$labelText) %>%
      leaflet::removeControl(layerId = "legend") %>%
      leaflet::addLegend(pal = colorPalettesLegend[[input$dispVar]],
                         values = adminGeoms$displayVar,
                         opacity = 0.8,
                         title = paste0(input$dispVar, " per 1000 inhabitants"),
                         position = "bottomleft",
                         layerId = "legend",
                         labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

  })

  # HIGHLIGHT AND DO PLOT FOR CLICKED POLYGONS ---------------------------------
  linePlotData <- reactive({

    if (is.null(input$map_shape_click$id)) { return() }
    appData %>%
      filter(polygonId == input$map_shape_click$id) %>%
      select(-c(polygonId, scaleLevel)) %>%
      pivot_longer(cols = -c(year, adminName),
                   names_to = "variable",
                   values_to = "value")

  })

  observeEvent(input$map_shape_click, {

    areaSelected(input$map_shape_click$id)
    adminGeoms <- adminGeomsList[[input$mapType]]
    newSelection <- adminGeoms[adminGeoms$polygonId == areaSelected(),]
    prevSelection <- adminGeoms[adminGeoms$polygonId == areaSelectedPrev(),]

    if (nrow(newSelection) == 0) { return() }
    if (areaSelected() != areaSelectedPrev()) {

      # highlight selected area with yellow
      newSelection$displayColor <- "yellow"
      leafletProxy("map", data = newSelection) %>%
        setShapeStyle(layerId = ~polygonId,
                      fillColor = ~displayColor)

      # reset previous selection back to its original display
      if (nrow(prevSelection) > 0) {
        prevSelection$displayVar <- filteredData()$displayVar[match(prevSelection$polygonId, filteredData()$polygonId)]
        prevSelection$displayColor <- colorPalettes[[input$dispVar]](prevSelection$displayVar)
        leafletProxy("map", data = prevSelection) %>%
          setShapeStyle(layerId = ~polygonId,
                        fillColor = ~displayColor)
      }

      # draw line plot of the selected area
      polyData <- linePlotData()
      output$plot <-  renderPlot({
        ggplot(data = polyData, aes(x = year, y = value, color = variable, linetype = variable)) +
          geom_line() +
          geom_hline(yintercept = 0, color = "grey50") +
          scale_color_manual(values = linePlotPal,
                             labels = linePlotLabs) +
          scale_linetype_manual(values = c("solid", "solid", "solid", "dashed"),
                                labels = linePlotLabs) +
          scale_y_continuous(name = "per 1000 inhabitants",
                             limits = c(min(c(0, min(polyData$value))), max(polyData$value))) +
          scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2019)) +
          ggtitle(paste("Selected area:", unique(polyData$adminName))) +
          labs(color  = "variable", linetype = "variable")
      })

      # set this area as the latest selection now that the plot is drawn
      areaSelectedPrev(input$map_shape_click$id)

    }
    shinyjs::enable("clearSelection")

  })

  # clear all changes caused by selecting an area
  observeEvent(input$clearSelection, {

    adminGeoms <- adminGeomsList[[input$mapType]]
    currSelection <- adminGeoms[adminGeoms$polygonId == areaSelected(),]
    currSelection$displayVar <- filteredData()$displayVar[match(currSelection$polygonId, filteredData()$polygonId)]
    currSelection$displayColor <- colorPalettes[[input$dispVar]](currSelection$displayVar)

    leafletProxy("map", data = currSelection) %>%
      setShapeStyle(layerId = ~polygonId,
                    fillColor = ~displayColor)

    areaSelected("")
    output$plot <<- NULL
    shinyjs::disable("clearSelection")

  })

  observeEvent(input$mapType, {

    areaSelected("")
    output$plot <<- NULL
    shinyjs::disable("clearSelection")

  })

} # end server

# run app
shinyApp(ui, server)
