#' Monobin module - user interface 
#'@param id Namespace id.
#'@return No return value, called for monobin module user interface.
#'
#'@examples
#' if 	(interactive()) {
#' 	mb.ui(id = "monobin")
#' 	}
#'@export
mb.ui <- function(id) {
	ns <- NS(id)
	tagList(
	useShinyjs(),
	fluidRow(
	box(title = "MONOTONIC BINNING",
	    status = "danger", collapsible = TRUE, collapsed = TRUE,
	    solidHeader = FALSE, width = 12, 
	#row 1:
		#block 1:
		column(5, 
		column(8, 
		selectInput(inputId = ns("trg.select"), 
				label = "Select target variable:",
				choices = "No imported data."),
		br(), 
		selectizeInput(inputId = ns("rf.select"), 
				   label = "Select risk factor(s):", 
				   choices = "No imported data.", 
				   selected = "No imported data.",
				   multiple = TRUE),	
		column(6, 
		checkboxInput(inputId = ns("rf.all"), 
				  label = "Select all risk factors", 
				  value = FALSE)),
		column(6, 
		checkboxInput(inputId = ns("rf.reset"), 
				  label = "Reset selection", 
				  value = FALSE)))),
		#block 2:
		column(2,
		selectInput(inputId = ns("monobin.method"),
				label = a("Select binning algorithm:", 
					    href = "https://cran.r-project.org/web/packages/monobin/monobin.pdf"),
				choices = monobin.algo)),
		#block 3:
		column(2,
		uiOutput(ns("algo.args"))), 
		#block 4:
		column(1, 		
		br(), 
		actionButton(inputId = ns("bin.run"),
				 label = "Run binning algorithm")),
	#row 2:
		br(), 
		hidden(div(id = ns("monobin.div"),  
		box(strong("Monotonic binning results"), width = 12,
			fluidRow(br(),
			column(3, 
			downloadButton(ns("summary.dwnl"), label = "Download monobin report")),
			column(3,
			downloadButton(ns("tbl.dwnl"), label = "Download dataset")))),
			column(12, DTOutput(ns("monobin.tbl")))))
	)
	)		
	)
}

#' Monobin module - server side 
#'@param id Namespace id.
#'@return No return value, called for monobin module server side.
#'
#'@examples
#' if 	(interactive()) {
#' 	mb.server(id = "monobin")
#' 	}
#'@export
mb.server <- function(id) {
	moduleServer(id, function(input, output, session) {

	srv <- reactiveValues(summary.tbl = NULL, db.trans = NULL)
	num.args <- c(5, 6, 7, 6, 7, 7)
	names(num.args) <- monobin.algo
	y.type.arg <- c(4, 3, 3, 4, 3, 3)
	names(y.type.arg) <- monobin.algo


	observeEvent(input$trg.select, {
		rv$target.select.3 <- input$trg.select
		rv$sync3 <- ifelse(rv$sync3 == 0, 1, 0)
		}, ignoreInit = TRUE)

	observeEvent(input$rf.all, {
		trigger <- input$rf.all
		if	(trigger) {
			if	(input$trg.select%in%c(" ", "No imported data.")) {
				showNotification("Select target first.", type = "error")
				updateCheckboxInput(session = session, 
							  inputId = "rf.all", 
							  value = FALSE)
				} else {
				rf.update <- c(rv$num.rf[!rv$num.rf%in%input$trg.select], 
						   rv$rf.imp, rv$rf.out)
				updateSelectizeInput(session = session, 
					    		   inputId = "rf.select",		
							   choices = rf.update,
							   selected = rf.update)
				}
			}
		})

	observeEvent(input$rf.reset, {
		if	(input$rf.reset) { 
			if	(input$trg.select%in%" " |
				paste0(input$rf.select, collapse = " & ")%in%"") {
				showNotification("Select targe and/or risk factors first.", 
						     type = "error")
				} else {
				rf.update <- c(rv$num.rf[!rv$num.rf%in%input$trg.select], 
						   rv$rf.imp, rv$rf.out)
				updateSelectizeInput(session = session, 
				   			   inputId = "rf.select",		
							   choices = rf.update,
							   selected = "")
				updateCheckboxInput(session = session, 
							  inputId = "rf.all", 
							  value = FALSE)
				}
			updateCheckboxInput(session = session, 
						  inputId = "rf.reset", 
						  value = FALSE)
			}
		})

	observeEvent(input$bin.run, {
		hide("monobin.div")
		if	(is.null(isolate(rv$db)) | input$trg.select%in%c(" ", "No imported data.")) {
			showNotification("No imported data or selected target variable.", 
					     type = "error")
			return(NULL)
			}
		if	(is.null(isolate(input$rf.select))) {
			showNotification("Select risk factors first.", type = "error")
			return(NULL)
			}
		bin.algo <- isolate(input$monobin.method)
		na <- num.args[bin.algo]
		args.e <- lapply(1:na, function(i) isolate(input[[paste0("arg", i)]]))		
		args.e <- lapply(args.e, function(x) ifelse(x%in%"NA", NA, x))
		args.e <- lapply(args.e, function(x) ifelse(x%in%c("TRUE", "FALSE"), as.logical(x), x))


		scr.check.res <- sc.check(x = args.e[[1]])
		if	(scr.check.res[[2]] >= 2) {
			msg <- "Special cases are not defined properly.
				  Check the following value(s): "
			msg <- paste0(msg, paste(scr.check.res[[3]], collapse = ", "))
			showNotification(msg, type = "error")
			return(NULL)
			}
		num.inp <- mono.inputs.check(x = bin.algo, args.e = args.e)
		if	(!num.inp[[1]]) {
			showNotification(num.inp[[2]], type = "error")
			return(NULL)
			}
		y.type <- args.e[y.type.arg[bin.algo]]
		if	(y.type == "bina") {
			d <- isolate(rv$db)
			y <- isolate(input$trg.select)
			cond <- !sum(d[!is.na(d[, y]), y]%in%c(0, 1)) == length(d[!is.na(d[, y]), y])
			if	(cond) {
				msg <- "Target variable is not 0/1 variable."
				showNotification(msg, type = "error")
				return(NULL)
				}
			}
		show("monobin.div")
		output$monobin.tbl <- renderDT({
			tbls <- withProgress(message = "Running the binning algorithm", 
						   value = 0, {
				  suppressWarnings(
				  monobin.run(algo = bin.algo, 
						  target.n = isolate(input$trg.select), 
						  rf = isolate(input$rf.select), 
						  sc = scr.check.res[[1]], 
						  args.e = args.e, 
						  db = isolate(rv$db))
				  )})
			srv$summary.tbl <- tbls[[1]]
			srv$db.trans <- tbls[[2]]
			num.format <- which(sapply(srv$summary.tbl, is.numeric))
			dt <- datatable(srv$summary.tbl, selection = "none", options = list(scrollX = TRUE))
			if	(length(num.format) > 0) {
				formatRound(dt, num.format[-1], digits = 3)
				} else {
				dt
				}
			})
		})

	output$summary.dwnl <- downloadHandler(
	filename = function() {paste0("monobin_summary_",
						Sys.Date(), ".csv")},
	content =  function(file) {write.csv(x = srv$summary.tbl, 
							 file = file,
							 row.names = FALSE)})

	output$tbl.dwnl <- downloadHandler(
	filename = function() {paste0("datast_transformed_",
						Sys.Date(), ".csv")},
	content =  function(file) {write.csv(x = srv$db.trans, 
							 file = file,
							 row.names = FALSE)})


	})
}

