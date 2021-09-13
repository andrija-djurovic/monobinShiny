#' Update data manager UI output
#'@param id Namespace id.
#'@param dummy A logical value indicating whether gcd data (from monobin package) or specific csv file are imported.
#'@return No return value, called in order to update data manager UI output after data import.
#'
#'@examples
#' if 	(interactive()) {
#'	observeEvent(rv$dm.uptd, {
#'		upd.dm(id = "data.manager", dummy = rv$import.dummy)
#'		}, ignoreInit = TRUE)
#' 	}
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
#' Sync between descriptive statistics and monobin module
#'@param upd.rf Vector of risk factor field ids that need to be updated.
#'@param num.rf Vector of updated numeric risk factors.
#'@param session Session object.
#'@return No return value, called in order to sync between descriptive statistics and monobin modules' UI elements after imputation procedures.
#'
#'@examples
#' if 	(interactive()) {
#'	upd.si.m23(upd.rf = upd.rf, 
#'		    num.rf = num.rf, 	
#'		    session = session)
#' 	}
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
#'@return No return value, called in order to sync between descriptive statistics and monobin modules' UI elements after data import.
#'
#'@examples
#' if 	(interactive()) {
#'	observeEvent(rv$sync, {
#'		sync.m23(id = "desc.imputation", 
#'			 num.rf = rv$num.rf,
#'			 module = "desc")
#'		sync.m23(id = "monobin", 
#'			 num.rf = rv$num.rf,
#'			 module = "monobin")
#'		rv$rf.imp <- NULL
#'		rv$rf.out <- NULL
#'		}, ignoreInit = TRUE)
#' 	}
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
#'@return No return value, called in order to sync between descriptive statistics and monobin modules' UI elements after imputation process.
#'
#'@examples
#' if 	(interactive()) {
#'	observeEvent(rv$sync2, {
#'		rf.update.2 <- c(rv$num.rf[!rv$num.rf%in%rv$target.select.2], rv$rf.imp, rv$rf.out)
#'		sync.m23.imp(id = "desc.imputation", 
#'			     num.rf = rf.update.2,
#'			     module = "desc")
#'		}, ignoreInit = TRUE)
#' 	}
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
#'@return No return value, called in order to hide download buttons (imp.div and out.div) from descriptive statistics module.
#'
#'@examples
#' if 	(interactive()) {
#'	observeEvent(rv$dwnl.sync, {
#'		hide.dwnl.buttons(id = "desc.imputation")
#'		}, ignoreInit = TRUE)
#' 	}
#'@export
hide.dwnl.buttons <- function(id) {
	moduleServer(id, function(input, output, session) {
	hide("imp.div")
	hide("out.div")
	})
}

