#' Starts shiny application for the monobin package
#'@return Starts shiny application for the monobin package.
#'@examples
#' if 	(interactive()) {
#' 	suppressMessages(library(monobinShiny))	
#' 	monobinShinyApp()
#' 	}
#'@import DT 
#'@import monobin
#'@importFrom shiny fileInput column strong p div verbatimTextOutput actionButton observeEvent moduleServer renderPrint 
#' tagList NS selectInput reactiveValues selectizeInput checkboxInput textInput downloadButton numericInput  downloadHandler
#' uiOutput updateSelectizeInput updateCheckboxInput 				
#'@import shinydashboard
#'@import shinyjs
#'@export
monobinShinyApp <- function() {
		
	appDir <- system.file("monobinShiny",  package = "monobinShiny")
	source(system.file("monobinShiny/global.R", package = "monobinShiny"), local = environment())
	shiny::runApp(appDir = appDir, launch.browser = TRUE)
}
