#' Descriptive statistics and imputation module - user interface 
#'@param id Namespace id.
#'@return No return value, called for descriptive statistics and imputation module user interface.
#'
#'@examples
#' if 	(interactive()) {
#' 	di.ui(id = "desc.imputation")
#' 	}
#'@export
di.ui <- function(id) {
	ns <- NS(id)
	tagList(
	useShinyjs(),
	fluidRow(
	box(title = "DESCRIPTIVE STATISTICS AND IMPUTATION", 
	    status = "danger", collapsible = TRUE,  collapsed = TRUE, 
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
				  value = FALSE)),
		textInput(inputId = ns("sc.all"),
			    label = HTML("Define special cases<br/>
					     (comma separated list of values, e.g: NA, NaN, Inf, 99999):"),
			    value = "NA, NaN, Inf")),
		column(4, align = "left",
		selectInput(inputId = ns("sc.method"),
				label = "How to treat special cases?",
				choices = c("together", "separately")),
		br(), br(), br(), br(), br(), 
		actionButton(inputId = ns("desc"),
				 label = "Run descriptive statistics",
				 width = 200))),
		#block 2:
		column(3,
		selectizeInput(inputId = ns("rf.imp"), 
				   label = "Select risk factor(s):", 
				   choices = "No imported data.", 
				   selected =  "No imported data.",
				   multiple = TRUE),
		column(9, 
		textInput(inputId = ns("sc.replace"),
			    label = HTML("Special cases top be replaced<br/>
					 (comma separated list of values,<br/>e.g: NA, NaN, Inf, 99999):"),
			    value = "NA, NaN, Inf"),
		selectInput(inputId = ns("impute.method"), 
					    label = "Select imputation method:",
					    choices = c("mean", "median"))),
		br(), br(), br(), 
		column(3,
		actionButton(inputId = ns("impute.run"),
				 label = "Run special case imputation",
				 width = 200),	
		br(), br(), br(), 
		hidden(div(id = ns("imp.div"), 
		downloadButton(ns("imp.dwnl"), 
				   label = "Download imputation report")))
		)),
		#block 3:
		column(3, offset = 1,
		selectizeInput(inputId = ns("rf.out"), 
				   label = "Select risk factor(s):", 
				   choices = "No imported data.", 
				   selected =  "No imported data.",
				   multiple = TRUE),
		strong(p("Percentile based outlier imputation:")),
		column(6, 
		numericInput(inputId = ns("upper.outlier"),
				 label = "Select upper bound:",
				 value = 0.99,
				 min = 0.50,
				 max = 0.99,
				 step = 0.01),
		numericInput(inputId = ns("lower.outlier"),
				 label = "Select lower bound:",
				 value = 0.01,
				 min = 0.01,
				 max = 0.5,
				 step = 0.01)),
		br(), 
		fluidRow(
		column(6, 
		actionButton(inputId = ns("outlier.run"),
				 label = "Run outlier imputation",
				 width = 180),
		br(), br(), br(), 
		hidden(div(id = ns("out.div"), 
		downloadButton(ns("out.dwnl"), 
				   label = "Download outlier report"))))
		)),
	#row 2:
		br(), 
		hidden(div(id = ns("desc.div"),  
		box(strong("Descriptive statstics results"), width = 12,
			fluidRow(br(),
			column(3, 
			downloadButton(ns("desc.dwnl"), label = "Download descriptive statistics report")),
			column(3,
			downloadButton(ns("tbl.dwnl"), label = "Download dataset")))),
		fluidRow(
		column(12, DTOutput(ns("desc.tbl")))))
		)
	)
	)		
	)
}

#' Descriptive statistics and imputation module - server side 
#'@param id Namespace id.
#'@return No return value, called for descriptive statistics and imputation module server side.
#'
#'@examples
#' if 	(interactive()) {
#' 	di.server(id = "desc.imputation")
#' 	}
#'@export
di.server <- function(id) {
	moduleServer(id, function(input, output, session) {

	srv <- reactiveValues(desc.stat = NULL)

	observeEvent(input$trg.select, {	
		rv$target.select.2 <- input$trg.select
		rv$sync2 <- ifelse(rv$sync2 == 0, 1, 0)
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
				rf.update <- c(rv$num.rf[!rv$num.rf%in%input$trg.select], rv$rf.imp, rv$rf.out)
				updateSelectizeInput(session = session, 
					    		   inputId = "rf.select",		
							   choices = rf.update,
							   selected = rf.update)
				}
			}
		}, ignoreInit = TRUE)

	observeEvent(input$rf.reset, {
		if	(input$rf.reset) { 
			if	(input$trg.select%in%" " |
				paste0(input$rf.select, collapse = " & ")%in%"") {
				showNotification("Select targe and/or risk factors first.", 
						     type = "error")
				} else {
				rf.update <- c(rv$num.rf[!rv$num.rf%in%input$trg.select], rv$rf.imp, rv$rf.out)
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
		}, ignoreInit = TRUE)

	observeEvent(input$desc, {
		if	(is.null(isolate(rv$db)) | input$trg.select%in%c(" ", "No imported data.")) {
			showNotification("No imported data or selected target variable.", 
					     type = "error")
			return(NULL)
			}
		if	(is.null(input$rf.select)) {
			showNotification("Select risk factors first.", type = "error")
			return(NULL)
			}
		sca.check.res <- sc.check(x = input$sc.all)
		if	(sca.check.res[[2]] >= 2) {
			msg <- "Special cases are not defined properly.
				  Check the following value(s): "
			msg <- paste0(msg, paste(sca.check.res[[3]], collapse = ", "))
			showNotification(msg, type = "error")
			return(NULL)
			}
		target <- isolate(input$trg.select)
		rf <- isolate(input$rf.select)
		sc <- sca.check.res[[1]]
		sc.method <- isolate(input$sc.method)

		show("desc.div")
		output$desc.tbl <- renderDT({
			srv$desc.stat <-  withProgress(message = "Running descriptive statistics report", 
								 value = 0, {
						desc.report(target = "qual", 
								rf = rf, 
								sc = sc, 
								sc.method = sc.method, 
								db = isolate(rv$db))
						})
			dt <- datatable(srv$desc.stat, selection = "none", options = list(scrollX = TRUE))
			num.format <- which(sapply(srv$desc.stat, is.numeric))
			if	(length(num.format) > 0) {
				formatRound(dt, num.format[-c(1, 2)], digits = 3)
				} else {
				dt
				}
			})
		})

	observeEvent(input$impute.run, {
		hide("imp.div")
		if	(is.null(rv$db) | input$trg.select%in%c(" ", "No imported data.")) {
			showNotification("No imported data or selected target variable.", 
					     type = "error")
			return(NULL)
			}
		if	(is.null(input$rf.imp)) {
			showNotification("Select risk factors first.", type = "error")
			return(NULL)
			}
		rf <- input$rf.imp
		scr.check.res <- sc.check(x = input$sc.replace)
		if	(scr.check.res[[2]] >= 2) {
			msg <- "Special cases to be replaced are not defined properly.
				  Check the following value(s): "
			msg <- paste0(msg, paste(scr.check.res[[3]], collapse = ", "))
			showNotification(msg, type = "error")
			return(NULL)
			}
		sca.check.res <- sc.check(x = input$sc.all)
		if	(sca.check.res[[2]] >= 2) {
			msg <- "Special cases are not defined properly.
				  Check the following value(s): "
			msg <- paste0(msg, paste(sca.check.res[[3]], collapse = ", "))
			showNotification(msg, type = "error")
			return(NULL)
			}
		imp.method <- input$impute.method
		imp.res <- suppressWarnings(
			     sc.impute(tbl = rv$db, 
					   rf = rf, 
					   sc = sca.check.res[[1]],
					   sc.replace = scr.check.res[[1]], 
					   imp.method = imp.method)
				)
		if	(!nrow(imp.res[[3]]) == 0) {
			show("imp.div")
			output$imp.dwnl <- downloadHandler(
			filename = function() {paste0("special_case_imputation_report_",
								Sys.Date(), ".csv")},
			content =  function(file) {write.csv(x = imp.res[[3]], 
							 		 file = file,
							 		 row.names = FALSE)})
			}
		rv$db <- imp.res[[1]]
		rv$rf.imp <- unique(c(rv$rf.imp, imp.res[[2]]))
		rv$sync23 <- ifelse(rv$sync23 == 0, 1, 0)
		showNotification("Special cases imputation complete.")		
		})

	observeEvent(input$outlier.run, {
		hide("out.div")
		if	(is.null(rv$db) | input$trg.select%in%c(" ", "No imported data.")) {
			showNotification("No imported data or selected target variable.", 
					     type = "error")
			return(NULL)
			}
		if	(is.null(input$rf.out)) {
			showNotification("Select risk factors first.", type = "error")
			return(NULL)
			}	
		upper.pct <- input$upper.outlier
		lower.pct <- input$lower.outlier	
		if	(upper.pct >= 1 | lower.pct <= 0) {
			showNotification("Upper bound has to be less than 1 and lower bound has to be greater 
					     than 0.", type = "error")
			return(NULL)
			}
		sca.check.res <- sc.check(x = input$sc.all)
			if	(sca.check.res[[2]] >= 2) {
			msg <- "Special cases are not defined properly.
				  Check the following value(s): "
			msg <- paste0(msg, paste(sca.check.res[[3]], collapse = ", "))
			showNotification(msg, type = "error")
			return(NULL)
			}
		imp.res <-  suppressWarnings(
				out.impute(tbl = rv$db, 		
					    rf = input$rf.out,
					    ub = upper.pct,
					    lb = lower.pct,
					    sc = sca.check.res[[1]])
				)
		if	(!nrow(imp.res[[3]]) == 0) {
			show("out.div")
			output$out.dwnl <- downloadHandler(
			filename = function() {paste0("outliers_imputation_report_",
								Sys.Date(), ".csv")},
			content =  function(file) {write.csv(x = imp.res[[3]], 
							 		 file = file,
							 		 row.names = FALSE)})
			}
		rv$db <- imp.res[[1]]
		rv$rf.out <- unique(c(rv$rf.out, imp.res[[2]]))
		rv$sync23 <- ifelse(rv$sync23 == 0, 1, 0)
		showNotification("Outliers imputation complete.")	
		})
	
	output$desc.dwnl <- downloadHandler(
	filename = function() {paste0("descriptive_statistics_report_",
						Sys.Date(), ".csv")},
	content =  function(file) {write.csv(x = srv$desc.stat, 
							 file = file,
							 row.names = FALSE)})

	output$tbl.dwnl <- downloadHandler(
	filename = function() {paste0("dataset_",
						Sys.Date(), ".csv")},
	content =  function(file) {write.csv(x = rv$db, 
							 file = file,
							 row.names = FALSE)})
	})
}


