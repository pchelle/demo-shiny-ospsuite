#------ Libraries ------
# Load the libraries required to run the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(icons)
library(ospsuite)
library(gganatogram)
library(dplyr)
library(ggplot2)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

#------ Pre-processing ------
# Before running the app, load all required models and data
simulation <- loadSimulation("test.pkml")
simResults <- importResultsFromCSV(simulation, "test-results.csv")
simResults <- simulationResultsToTibble(simResults)
simResults$organ <- sapply(
  simResults$paths,
  function(path) {
    tolower(ospsuite::toPathArray(path)[2])
  }
)

# Map the relevant simulation path, to gganatogram body parts
# by following the naming convention of their organs
simResults <- simResults %>% mutate(
  value = simulationValues,
  type = "other",
  organ = gsub(pattern = "largeintestine", replacement = "small_intestine", organ),
  organ = gsub(pattern = "arterialblood", replacement = "aorta", organ),
  organ = gsub(pattern = "peripheralvenousblood", replacement = "atrial_appendage", organ)
)

# For displaying interactive organ names and values
hgMaleData <- do.call(rbind, hgMale_list) %>%
  filter(id %in% simResults$organ)
hgFemaleData <- do.call(rbind, hgFemale_list) %>%
  filter(id %in% simResults$organ)

# Suggested color palettes for the app
# Simple app UI
ui <- dashboardPage(
  dashboardHeader(title = span(
    tags$img(src="logo.png", width = "25px"),
    " Anatogram Demo"
    )),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(
      width = 4,
      title = span(icon("gear"), "Settings"),
      solidHeader = TRUE,
      status = "primary",
      sliderInput(
        "time",
        paste0("Time [", head(simResults$TimeUnit, 1), "]"),
        min = min(simResults$Time),
        max = 120, # can also be max(simResults$Time),
        value = min(simResults$Time),
        step = 3
      ),
      switchInput(
        inputId = "sex",
        label = "Gender",
        onStatus = "primary",
        offStatus = "danger",
        onLabel = span(icon("person"), "Male"),
        offLabel = span(icon("person-dress"), "Female"),
        value = TRUE
      ),
      selectInput(
        "bodyColor",
        "Body Color",
        choices = grDevices::colors(),
        selected = "aliceblue"
      )
    ),
    box(
      width = 8,
      title = span(icon("chart-line"), "Simulation Results"),
      solidHeader = TRUE,
      status = "primary",
      box(
        width = 8,
        title = span(icon("person"), "Anatogram"),
        solidHeader = TRUE,
        status = "success",
        plotOutput(
          "body",
          width = "400px",
          click = "body_click",
          dblclick = "body_dblclick",
          hover = "body_hover",
          brush = "body_brush"
        )
      ),
      box(
        width = 4,
        title = span(icon("circle-info"), "Selected Organ"),
        solidHeader = TRUE,
        status = "success",
        # Display selected organ information
        # (x, y, name, value, click
        # Get values of selected body part
        strong("Coordinates [cm]"), verbatimTextOutput("coord"),
        strong("Organ Name"), verbatimTextOutput("name"),
        strong("Concentration [umol/l]"), verbatimTextOutput("value"),
        strong("selected"), htmlOutput("click")
      )
    )
  )
)

# App Server
server <- function(input, output, session) {
  bodyRange <- reactiveValues(x = NULL, y = NULL)
  getSex <- reactive({
    if (input$sex) {
      return("male")
    }
    return("female")
  })
  getOrganName <- reactive({
    noName <- any(
      is.null(input$body_hover$x),
      is.null(input$body_hover$y)
    )
    if (noName) {
      return()
    }
    organName <- switch(getSex(),
      "male" = hgMaleData,
      "female" = hgFemaleData
    ) %>%
      mutate(distance = (x - input$body_hover$x)^2 + (y - abs(input$body_hover$y))^2) %>%
      filter(distance == min(distance, na.rm = TRUE)) %>%
      pull(id)
    return(unique(organName))
  })
  observeEvent(input$body_brush, {
    bodyRange$x <- c(input$body_brush$xmin, input$body_brush$xmax)
    bodyRange$y <- c(input$body_brush$ymin, input$body_brush$ymax)
  })
  observeEvent(input$body_dblclick, {
    bodyRange$x <- NULL
    bodyRange$y <- NULL
  })
  output$coord <- renderPrint({
    noCoord <- any(
      is.null(input$body_hover$x),
      is.null(input$body_hover$y)
    )
    if(noCoord){return()}
    paste(
      "x:", round(input$body_hover$x, 1),
      "y:", round(abs(input$body_hover$y), 1)
    )
  })

  output$name <- renderPrint({
    getOrganName()
  })
  output$value <- renderPrint({
    organName <- getOrganName()
    if (length(organName) == 0) {
      return()
    }
    simResults %>%
      filter(organ %in% organName, Time == input$time) %>%
      pull(value)
  })

  output$click <- renderPrint({
    noName <- any(
      is.null(input$body_click$x),
      is.null(input$body_click$y)
    )
    if (noName) {
      return()
    }
    organName <- switch(getSex(),
      "male" = hgMaleData,
      "female" = hgFemaleData
    ) %>%
      mutate(distance = (x - input$body_click$x)^2 + (y - abs(input$body_click$y))^2) %>%
      filter(distance == min(distance, na.rm = TRUE)) %>%
      pull(id)
    organName <- unique(organName)
    organLabel <- switch(organName,
      "aorta" = span(img(src="blood_vessel.svg"), "Aorta"),
      "small_intestine" = span(img(src="intestine.svg"), "Small Intestine"),
      "brain" = span(img(src="neurology.svg"), "Brain"),
      "lung" = span(img(src="lungs.svg"), "Lung"),
      "liver" = span(img(src="liver-alt.svg"), "Liver"),
      "heart" = span(img(src="heart.svg"), "Heart"),
      "pancreas" = span(img(src="pancreas.svg"), "Pancreas"),
      "kidney" = span(img(src="kidneys.svg"), "Kidney"),
      "stomach" = span(img(src="stomach.svg"), "Stomach")
    )
    organValue <- simResults %>%
      filter(organ %in% organName, Time == input$time) %>%
      pull(value)

    return(div(organLabel, br(), img(src="blood_bag.svg"), "Vancomycin:", round(organValue, 2), "umol/l"))
  })

  # Displayed anatogram
  output$body <- renderPlot({
    gganatogram(
      # data filtered by selected time point
      data = simResults %>% filter(Time == input$time),
      organism = "human",
      # display selected anatogram
      sex = getSex(),
      # map the color gradient to "value" column in data
      fill = "value",
      # display selected body Color
      fillOutline = input$bodyColor
    ) +
      # remove background as advised by package gganatogram
      theme_void() +
      # apply selected color gradient
      scale_fill_viridis_c(
        option = "turbo",
        # get a fixed window of color gradient
        limits = range(simResults$value)
      ) +
      # apply a fixed coord as advised by package gganatogram
      coord_fixed(xlim = bodyRange$x, ylim = bodyRange$y) +
      labs(fill = paste0("Concentration [", head(simResults$unit, 1), "]"))
  })
}

# Run the app
shinyApp(ui, server)
