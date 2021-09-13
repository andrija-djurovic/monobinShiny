#' Data manager module - user interface 
#'@param id Namespace id.
#'@return No return value, called for data manager module user interface.
#'
#'@examples
#' if 	(interactive()) {
#' 	dm.ui(id = "data.manager")
#' 	}
#'@export
dm.ui <- function(id) {
	ns <- NS(id)
	tagList(
	useShinyjs(),
	fluidRow(
	box(title = "DATA MANAGER", 
	    status = "danger", collapsible = TRUE, solisdHeader = FALSE, width = 12,
 		#block 1:
		column(3, 
		fileInput(inputId = ns("import"), 
			    label = "Import .csv file")),
 		#block 2:
		column(5, 
		strong(p("Data import log:")), 
		div(style = "overflow-y:auto; max-height: 300px;",
		verbatimTextOutput(outputId = ns("data.str"),
					 placeholder = TRUE))),
 		#block 3:
		column(1, offset = 2, align = "right",
		br(),
		actionButton(inputId = ns("import.dummy"),
				 label = "Import dummy data")))	
	)		
	)
}

#' Data manager module - server side
#'@param id Namespace id.
#'@return No return value, called for data manager module server side.
#'
#'@examples
#' if 	(interactive()) {
#' 	dm.server(id = "data.manager")
#' 	}
#'@export
dm.server <- function(id) {
	moduleServer(id, function(input, output, session) {

	get.ext <- function(x) {
		fp.split <- strsplit(x, ".", fixed=TRUE)[[1]]
		fp.split.l <- length(fp.split)
		if	(fp.split.l > 1) { 
			ext <- fp.split[fp.split.l]
			} else { 
			ext <- "" 
			}
	return(ext)
	}

	observeEvent(input$import, {
		fp <- isolate(input$import$datapath)	
		db <- try(read.csv(fp, header = TRUE, check.names = FALSE), 
				 silent = TRUE)
		check.ext <- get.ext(x = fp)
		if	("try-error"%in%class(db) | !check.ext%in%"csv") {
			msg <- ".csv file not in appropriate format.\n
				  First, Make sure that manual import using read.csv works."
			showNotification(msg, duration = 10, type = "error")
			return(NULL)
			} else {
			rv$db <- db
			}
		rv$num.rf <- names(rv$db)[sapply(rv$db, is.numeric)]
		if	(!length(rv$num.rf) > 1) {
			msg <- "At least two numeric variables are required."
			showNotification(msg, duration = 10, type = "error")
			return(NULL)
			}
		rv$dm.uptd <- ifelse(rv$dm.uptd == 0, 1, 0)
		rv$import.dummy <- FALSE
		rv$sync <- ifelse(rv$sync == 0, 1, 0)
		rv$dwnl.sync <- ifelse(rv$dwnl.sync == 0, 1, 0)
		}, ignoreInit = TRUE)

	observeEvent(input$import.dummy, {
		rv$db <- gcd
		rv$import.dummy <- TRUE
		rv$dm.uptd <- ifelse(rv$dm.uptd == 0, 1, 0)
		rv$num.rf <- names(rv$db)[sapply(rv$db, is.numeric)]
		rv$sync <- ifelse(rv$sync == 0, 1, 0)
		rv$dwnl.sync <- ifelse(rv$dwnl.sync == 0, 1, 0)
		reset("import")
		}, ignoreInit = TRUE)

	})
}

