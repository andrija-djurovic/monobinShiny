server <- function(input, output, session) {
 	session$onSessionEnded(function() {stopApp()})

	algo.ui(id = "monobin")
	dm.server(id = "data.manager")
	di.server(id = "desc.imputation")
	mb.server(id = "monobin")

	observeEvent(rv$dm.uptd, {
		upd.dm(id = "data.manager", dummy = rv$import.dummy)
		}, ignoreInit = TRUE)

	observeEvent(rv$sync, {
		sync.m23(id = "desc.imputation", 
			 num.rf = rv$num.rf,
			 module = "desc")
		sync.m23(id = "monobin", 
			 num.rf = rv$num.rf,
			 module = "monobin")
		rv$rf.imp <- NULL
		rv$rf.out <- NULL
		}, ignoreInit = TRUE)

	observeEvent(rv$sync2, {
		rf.update.2 <- c(rv$num.rf[!rv$num.rf%in%rv$target.select.2], rv$rf.imp, rv$rf.out)
		sync.m23.imp(id = "desc.imputation", 
			     num.rf = rf.update.2,
			     module = "desc")
		}, ignoreInit = TRUE)

	observeEvent(rv$sync3, {
		rf.update.3 <- c(rv$num.rf[!rv$num.rf%in%rv$target.select.3], rv$rf.imp, rv$rf.out)
		sync.m23.imp(id = "monobin", 
			     num.rf = rf.update.3,
			     module = "monobin")
		}, ignoreInit = TRUE)

	observeEvent(rv$sync23, {
		rf.update.2 <- c(rv$num.rf[!rv$num.rf%in%rv$target.select.2], rv$rf.imp, rv$rf.out)
		rf.update.3 <- c(rv$num.rf[!rv$num.rf%in%rv$target.select.3], rv$rf.imp, rv$rf.out)
		sync.m23.imp(id = "desc.imputation", 
			     num.rf = rf.update.2,
			     module = "desc")
		sync.m23.imp(id = "monobin", 
			     num.rf = rf.update.3,
			     module = "monobin")
		}, ignoreInit = TRUE)

	observeEvent(rv$dwnl.sync, {
		hide.dwnl.buttons(id = "desc.imputation")
		}, ignoreInit = TRUE)

}



