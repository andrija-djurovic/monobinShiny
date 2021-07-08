#' Update data manager output
#'@param id Namespace id.
#'@param dummy A logical value indicating whether gcd data (from monobin package) or specific csv file are imported.
#'@export
upd.dm <- function(id, dummy) {	
	moduleServer(id, function(input, output, session) {
	check.msg <- check.vars(tbl = rv$db)
	str.output <- capture.output(str(rv$db, list.len = ncol(rv$db)))
	if	(dummy) {
		reset("import")
		import.desc <- "Dummy data imported (gcd from monobin package)"
		} else {
		import.desc <- "Manual import"
		} 
	print.res <- list("Import" = import.desc,
			  "Variable check" = check.msg, 
			  "Table structure" = str.output)
	output$data.str <- renderPrint({print(print.res)})
	})
}
#' Update sync between descriptive statistics and monobin module
#'@param upd.rf Vector of risk factor field ids that need to be updated.
#'@param num.rf Vector of updated numeric risk factors.
#'@param session Session object .
#'@export
upd.si.m23 <- function(upd.rf, num.rf, session) {
	for	(i in 1:length(upd.rf)) {
		 updateSelectizeInput(session = session, 
					    inputId = upd.rf[i],		
					    choices = num.rf,
					    selected = "")
		}
}
#' Sync between descriptive statistics and monobin module after data import
#'@param id Namespace id.
#'@param num.rf Vector of updated numeric risk factors.
#'@param module Descriptive statistic or monobin module.
#'@export
sync.m23 <- function(id, num.rf, module) {
	moduleServer(id, function(input, output, session) {
	updateSelectInput(session = session, 
				inputId = "trg.select",		
				choices = c(" ", num.rf),
				selected = NULL)
	if	(module%in%"desc") {
		upd.rf <- c("rf.select", "rf.imp", "rf.out") 
		hide("desc.div")
		} else {
		upd.rf <- "rf.select"
		hide("monobin.div")
		}
	upd.si.m23(upd.rf = upd.rf, 
		    num.rf = num.rf, 	
		    session = session)
	updateCheckboxInput(session = session, 
				  inputId = "rf.all", 
				  value = FALSE)
	})
}
#' Sync between descriptive statistics and monobin module after imputation process
#'@param id Namespace id.
#'@param num.rf Vector of updated numeric risk factors.
#'@param module Descriptive statistic or monobin module.
#'@export
sync.m23.imp <- function(id, num.rf, module) {
	moduleServer(id, function(input, output, session) {
	if	(module%in%"desc") {
		upd.rf <- c("rf.select", "rf.imp", "rf.out") 
		} else {
		upd.rf <- "rf.select"
		}
	upd.si.m23(upd.rf = upd.rf, 
		    num.rf = num.rf, 	
		    session = session)
	updateCheckboxInput(session = session, 
				  inputId = "rf.all", 
				  value = FALSE)
	})
}
#' Hide download buttons from descriptive statistics module
#'@param id Namespace id.
#'@export
hide.dwnl.buttons <- function(id) {
	moduleServer(id, function(input, output, session) {
	hide("imp.div")
	hide("out.div")
	})
}

