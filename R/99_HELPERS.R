#' mdt.bin - monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, called for user interface of the iso.bin - monobin functions' inputs.
#'
#'@examples
#' if 	(interactive()) {
#' 	output$algo.args <- renderUI({tagList(switch(algo.select, "cum.bin" = cum.ui(id = id),
#'								  "iso.bin" = iso.ui(id = id),
#'								  "ndr.bin" = ndr.sts.ui(id = id),
#'								  "sts.bin" = ndr.sts.ui(id = id),
#'								  "pct.bin" = pct.ui(id = id),
#'								  "woe.bin" = woe.ui(id = id),
#'							          "mdt.bin" = mdt.ui(id = id)))
#'			})	
#' 	}
#'@export
mdt.ui <- function(id) {
	ns <- NS(id)
	column(12, 
	textInput(inputId = ns("arg1"),
		    label = "Special case elements",
		    value = "NA, NaN, Inf"),
	numericInput(inputId = ns("arg2"),
			 label = "Number of splitting groups for each node",
			 value = 50,
			 min = 2,
			 max = 150),
	selectInput(inputId = ns("arg3"),
			label = "How to treat special cases?",
			choices = c("together", "separately")),
	selectInput(inputId = ns("arg4"),
			label = "Type of target variable",
			choices = c(NA, "bina", "cont")),
	numericInput(inputId = ns("arg5"),
			 label = "Minimum pct. of observations per bin",
			 value = 0.05,
			 min = 0,
			 max = 1),
	numericInput(inputId = ns("arg6"),
			 label = "Minimum avg. target rate per bin",
			 value = 0.01,
			 min = 0,
			 max = 1),
	selectInput(inputId = ns("arg7"),
			label = "Force trend",
			choices = c(NA, "i", "d"))
	)
}
#' cum.bin - monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, called for user interface of the cum.bin - monobin functions' inputs.
#'
#'@examples
#' if 	(interactive()) {
#' 	output$algo.args <- renderUI({tagList(switch(algo.select, "cum.bin" = cum.ui(id = id),
#'								  "iso.bin" = iso.ui(id = id),
#'								  "ndr.bin" = ndr.sts.ui(id = id),
#'								  "sts.bin" = ndr.sts.ui(id = id),
#'								  "pct.bin" = pct.ui(id = id),
#'								  "woe.bin" = woe.ui(id = id),
#'							          "mdt.bin" = mdt.ui(id = id)))
#'			})	
#' 	}
#'@export
cum.ui <- function(id) {
	ns <- NS(id)
	column(12, 
	textInput(inputId = ns("arg1"),
		    label = "Special case elements",
		    value = "NA, NaN, Inf"),
	selectInput(inputId = ns("arg2"),
			label = "How to treat special cases?",
			choices = c("together", "separately")),
	numericInput(inputId = ns("arg3"),
			 label = "Number of starting groups",
			 value = 15,
			 min = 2,
			 max = 40),
	selectInput(inputId = ns("arg4"),
			label = "Type of target variable",
			choices = c(NA, "bina", "cont")),
	selectInput(inputId = ns("arg5"),
			label = "Force trend",
			choices = c(NA, "i", "d"))
	)
}
#' iso.bin - monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, called for user interface of the iso.bin - monobin functions' inputs.
#'
#'@examples
#' if 	(interactive()) {
#' 	output$algo.args <- renderUI({tagList(switch(algo.select, "cum.bin" = cum.ui(id = id),
#'								  "iso.bin" = iso.ui(id = id),
#'								  "ndr.bin" = ndr.sts.ui(id = id),
#'								  "sts.bin" = ndr.sts.ui(id = id),
#'								  "pct.bin" = pct.ui(id = id),
#'								  "woe.bin" = woe.ui(id = id),
#'							          "mdt.bin" = mdt.ui(id = id)))
#'			})	
#' 	}
#'@export
iso.ui <- function(id) {
	ns <- NS(id)
	column(12, 
	textInput(inputId = ns("arg1"),
		    label = "Special case elements",
		    value = "NA, NaN, Inf"),
	selectInput(inputId = ns("arg2"),
			label = "How to treat special cases?",
			choices = c("together", "separately")),
	selectInput(inputId = ns("arg3"),
			label = "Type of target variable",
			choices = c(NA, "bina", "cont")),
	numericInput(inputId = ns("arg4"),
			 label = "Minimum pct. of observations per bin",
			 value = 0.05,
			 min = 0,
			 max = 1),
	numericInput(inputId = ns("arg5"),
			 label = "Minimum avg. target rate per bin",
			 value = 0.01,
			 min = 0,
			 max = 1),
	selectInput(inputId = ns("arg6"),
			label = "Force trend",
			choices = c(NA, "i", "d"))
	)
}
#' ndr.bin / sts.bin - monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, called for user interface of the ndr.bin / sts.bin - monobin functions' inputs.
#'
#'@examples
#' if 	(interactive()) {
#' 	output$algo.args <- renderUI({tagList(switch(algo.select, "cum.bin" = cum.ui(id = id),
#'								  "iso.bin" = iso.ui(id = id),
#'								  "ndr.bin" = ndr.sts.ui(id = id),
#'								  "sts.bin" = ndr.sts.ui(id = id),
#'								  "pct.bin" = pct.ui(id = id),
#'								  "woe.bin" = woe.ui(id = id),
#'							          "mdt.bin" = mdt.ui(id = id)))
#'			})	
#' 	}
#'@export
ndr.sts.ui <- function(id) {
	ns <- NS(id)
	column(12, 
	textInput(inputId = ns("arg1"),
		    label = "Special case elements",
		    value = "NA, NaN, Inf"),
	selectInput(inputId = ns("arg2"),
			label = "How to treat special cases?",
			choices = c("together", "separately")),
	selectInput(inputId = ns("arg3"),
			label = "Type of target variable",
			choices = c(NA, "bina", "cont")),
	numericInput(inputId = ns("arg4"),
			 label = "Minimum pct of observations per bin",
			 value = 0.05,
			 min = 0,
			 max = 1),
	numericInput(inputId = ns("arg5"),
			 label = "Minimum avg target rate per bin",
			 value = 0.01,
			 min = 0,
			 max = 1),
	numericInput(inputId = ns("arg6"),
			 label = "p-value",
			 value = 0.05,
			 min = 0,
			 max = 1),
	selectInput(inputId = ns("arg7"),
			label = "Force trend",
			choices = c(NA, "i", "d"))
	)
}
#' pct.bin - monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, called for user interface of the pct.bin - monobin functions' inputs.
#'
#'@examples
#' if 	(interactive()) {
#' 	output$algo.args <- renderUI({tagList(switch(algo.select, "cum.bin" = cum.ui(id = id),
#'								  "iso.bin" = iso.ui(id = id),
#'								  "ndr.bin" = ndr.sts.ui(id = id),
#'								  "sts.bin" = ndr.sts.ui(id = id),
#'								  "pct.bin" = pct.ui(id = id),
#'								  "woe.bin" = woe.ui(id = id),
#'							          "mdt.bin" = mdt.ui(id = id)))
#'			})	
#' 	}
#'@export
pct.ui <- function(id) {
	ns <- NS(id)
	column(12, 
	textInput(inputId = ns("arg1"),
		    label = "Special case elements",
		    value = "NA, NaN, Inf"),
	selectInput(inputId = ns("arg2"),
			label = "How to treat special cases?",
			choices = c("together", "separately")),
	numericInput(inputId = ns("arg3"),
			 label = "Number of starting groups",
			 value = 15,
			 min = 2,
			 max = 40),
	selectInput(inputId = ns("arg4"),
			label = "Type of target variable",
			choices = c(NA, "bina", "cont")),
	selectInput(inputId = ns("arg5"),
			label = HTML("Force WoE trend</br>
					 (applied only for continuous target)"),
			choices = c(TRUE, FALSE)),
	selectInput(inputId = ns("arg6"),
			label = "Force trend",
			choices = c(NA, "i", "d"))
	)
}
#' woe.bin - monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, called for user interface of the woe.bin - monobin functions' inputs.
#'
#'@examples
#' if 	(interactive()) {
#' 	output$algo.args <- renderUI({tagList(switch(algo.select, "cum.bin" = cum.ui(id = id),
#'								  "iso.bin" = iso.ui(id = id),
#'								  "ndr.bin" = ndr.sts.ui(id = id),
#'								  "sts.bin" = ndr.sts.ui(id = id),
#'								  "pct.bin" = pct.ui(id = id),
#'								  "woe.bin" = woe.ui(id = id),
#'							          "mdt.bin" = mdt.ui(id = id)))
#'			})	
#' 	}
#'@export
woe.ui <- function(id) {
	ns <- NS(id)
	column(12, 
	textInput(inputId = ns("arg1"),
		    label = "Special case elements",
		    value = "NA, NaN, Inf"),
	selectInput(inputId = ns("arg2"),
			label = "How to treat special cases?",
			choices = c("together", "separately")),
	selectInput(inputId = ns("arg3"),
			label = "Type of target variable",
			choices = c(NA, "bina", "cont")),
	numericInput(inputId = ns("arg4"),
			 label = "Minimum pct of observations per bin",
			 value = 0.05,
			 min = 0,
			 max = 1),
	numericInput(inputId = ns("arg5"),
			 label = "Minimum avg target rate per bin",
			 value = 0.01,
			 min = 0,
			 max = 1),
	numericInput(inputId = ns("arg6"),
			 label = "WoE threshold",
			 value = 0.1,
			 min = 0,
			 max = 1),
	selectInput(inputId = ns("arg7"),
			label = "Force trend",
			choices = c(NA, "i", "d"))
	)
}
#' Server side for monobin functions' inputs
#'@param id Namespace id.
#'@return No return value, server side call for user interface of the selected binning algorithm.
#'
#'@examples
#' if 	(interactive()) {
#' 	algo.ui(id = "monobin")
#' 	}
#'@export
algo.ui <- function(id) {
	moduleServer(id, function(input, output, session) {
	observeEvent(input$monobin.method, {
		algo.select <- input$monobin.method
		output$algo.args <- renderUI({
			tagList(switch(algo.select, 
					   "cum.bin" = cum.ui(id = id),
					   "iso.bin" = iso.ui(id = id),
					   "ndr.bin" = ndr.sts.ui(id = id),
					   "sts.bin" = ndr.sts.ui(id = id),
					   "pct.bin" = pct.ui(id = id),
					   "woe.bin" = woe.ui(id = id),
					   "mdt.bin" = mdt.ui(id = id)))
			})	
		})
	})
}
#' Check for categorical variables when importing the data
#'@param tbl Imported data frame.
#'@return Returns a character vector which describes variables type of imported data frame.
#'
#'@examples
#' if 	(interactive()) {
#' 	check.msg <- check.vars(tbl = rv$db)
#' 	}
#'@export
check.vars <- function(tbl) {
	check <- !sapply(tbl, is.numeric)
	check.col <- names(tbl)[check]
	if	(length(check.col) == 0) {
		msg <- "All variables of numeric type."
		} else {
		cols <- paste(check.col, collapse = ", ")
		msg <- paste0("Following variables identified as non-numeric:",
				  cols, ".")
		}
return(msg) 
}
#' Special cases - check input values
#'@param x Numeric vector of special case values.
#'@return Returns a list of three vectors: special case input(s) converted to numeric type, 
#' number of special case input(s) that cannot be converted to 
#' numeric type (including \code{NA}, \code{NaN} and \code{Inf}) and special case input(s) 
#' that cannot be converted to numeric type.
#'
#'@examples
#' if 	(interactive()) {
#' 	sca.check.res <- sc.check(x = input$sc.all)
#' 	scr.check.res <- sc.check(x = input$sc.replace)
#' 	}
#'sc.check(x = "NA, NaN, Inf")
#'sc.check(x = "NA, abc")
#'sc.check(x = "NaN, abc")
#'sc.check(x = "Inf, abc")
#'sc.check(x = "9999999999, abc")
#'sc.check(x = "NA, NaN, Inf, 9999999999")
#'@export
sc.check <- function(x) {
	if	(""%in%x) {
		return(list(NULL, 2, "Empty input field."))
		}
	if	(is.na(x)) {
		sc <- NA
		} else {
		sc <- trimws(strsplit(x, ",")[[1]])
		}
	if	(length(sc) == 1) {
		if	(is.na(x) | is.infinite(x)) {
			return(list(x, 0, x))
			}
		if	(!sc%in%c("NA", "NaN", "Inf") & suppressWarnings(is.na(as.numeric(sc)))) {
			return(list(NULL, 2, sc))
			}
		}
	sc.num <- suppressWarnings(as.numeric(sc))
	check.01 <- is.na(sc.num) 
	check.02 <- is.nan(sc.num)
	check.99 <- check.01 & !check.02
	check.val <- sc[check.99]
	if	(length(check.val) > 0) {
		if	(length(check.val) == 1) {
			if	(check.val != "NA") {
				check.sum <- 2
				} else {
				check.sum <- 1
				}
			} else {
			check.sum <- sum(check.99)
			}
		} else {
		check.sum <- 0
		}
return(list(sc.num, check.sum, check.val))
}
#' Special case imputation
#'@param tbl Data frame with risk factors ready for imputation.
#'@param rf Vector of risk factors to be imputed.
#'@param sc Numeric vector of special case values.
#'@param sc.replace Numeric vector of special case values that are selected for imputation.
#'@param imp.method Imputation method (mean or median).
#'@return Returns a list of three elements. The first element is a data frame with imputed values,
#' the second element is a vector of newly created risk factors (with imputed values) and the third one is 
#' a data frame with information about possible imputation errors.
#'
#'@examples
#' if 	(interactive()) {
#'		imp.res <- suppressWarnings(
#'			     sc.impute(tbl = rv$db, 
#'					   rf = rf, 
#'					   sc = sca.check.res[[1]],
#'					   sc.replace = scr.check.res[[1]], 
#'					   imp.method = imp.method)
#'				)
#' 	}
#'
#'@export
sc.impute <- function(tbl, rf, sc, sc.replace, imp.method) {
	rfl <- length(rf)
	rfn <- paste0(rf, "_sc_", imp.method)
	rfn.f <- rfn
	info.tbl <- vector("list", rfl)
	info.msg <- "Imputed value cannot be calculated properly.
			 Selected risk factor is not processed.
			 Check the risk factor manually."
	eval.exp <- paste0(imp.method, "(rf.imp[!rf.imp%in%sc])")
	for	(i in 1:rfl) {
		rf.l <- rf[i]
		rfn.l <- rfn[i]
		rf.imp <- tbl[, rf.l]
		rf.imp.val <- eval(parse(text = eval.exp))
		if	(rf.imp.val%in%c(NA, Inf, NaN)) {
			info.tbl[[i]] <- data.frame(risk.factor = rf.l, 
							    info = info.msg, 
							    imputation.method = imp.method)
			rfn.f <- rfn.f[!rfn.f%in%rfn.l]
			next
			} else {
			rf.imp[rf.imp%in%sc.replace] <- rf.imp.val
			if	(rfn.l%in%names(tbl)) {
				tbl[, rfn.l] <- rf.imp
				} else {
				tbl <- cbind.data.frame(tbl, rf.imp)
				names(tbl)[ncol(tbl)] <- rfn.l
				}
			}
		}
	info.tbl <- bind_rows(info.tbl)	
return(list(tbl, rf.imp = rfn.f, info = data.frame(info.tbl)))
}
#' Outliers imputation
#'@param tbl Data frame with risk factors ready for imputation.
#'@param rf Vector of risk factors to be imputed.
#'@param ub Upper bound percentiles.
#'@param lb Lower bound percentiles.
#'@param sc Numeric vector of special case values.
#'@return Returns a list of three elements. The first element is a data frame with imputed values,
#' the second element is a vector of newly created risk factors (with imputed values) and the third one is 
#' a data frame with information about possible imputation errors.
#'
#'@examples
#' if 	(interactive()) {
#'		imp.res <-  suppressWarnings(
#'				out.impute(tbl = rv$db, 		
#'					    rf = input$rf.out,
#'					    ub = upper.pct,
#'					    lb = lower.pct,
#'					    sc = sca.check.res[[1]])
#'				)
#' 	}
#'
#'@export
out.impute <- function(tbl, rf, ub, lb, sc) {
	rfl <- length(rf)
	rfn <- paste0(rf, "_out_", ub, "_", lb)
	rfn.f <- rfn
	info.tbl <- vector("list", rfl)
	info.msg <- "Imputed outlier value(s) cannot be calculated properly.
			 Selected risk factor is not processed.
			 Check the risk factor manually."
	for	(i in 1:rfl) {
		rf.l <- rf[i]
		rfn.l <- rfn[i]
		rf.imp <- tbl[, rf.l]
		complete.c <- rf.imp[!rf.imp%in%sc]
		rf.imp.ub <- try(quantile(complete.c, ub), silent = TRUE)
		rf.imp.lb <- try(quantile(complete.c, lb), silent = TRUE)
		cond <- class(rf.imp.ub)%in%"try-error" | class(rf.imp.lb)%in%"try-error" |
			  rf.imp.ub%in%c(NA, Inf, NaN) | rf.imp.lb%in%c(NA, Inf, NaN)
		if	(cond) {
			info.tbl[[i]] <- data.frame(risk.factor = rf.l, 
							    info = info.msg, 
							    pct.selected = paste0("upper bound = ", ub, 
										        "; lower bound = ", lb))
			rfn.f <- rfn.f[!rfn.f%in%rfn.l]
			next
			} else {
			rf.imp[!rf.imp%in%sc & rf.imp > rf.imp.ub] <- rf.imp.ub
			rf.imp[!rf.imp%in%sc & rf.imp < rf.imp.lb] <- rf.imp.lb
			if	(rfn.l%in%names(tbl)) {
				tbl[, rfn.l] <- rf.imp
				} else {
				tbl <- cbind.data.frame(tbl, rf.imp)
				names(tbl)[ncol(tbl)] <- rfn.l
				}
			}
		}
	info.tbl <- bind_rows(info.tbl)	
return(list(tbl, rf.imp = rfn.f, info = data.frame(info.tbl)))
}
#' Descriptive statistics report
#'@param target Selected target.
#'@param rf  Vector of a selected numeric risk factors.
#'@param sc Numeric vector of special case values.
#'@param sc.method Define how special cases will be treated, all together or in separate bins.
#'@param db Data frame of target and numeric risk factors.
#'@return Returns a data frame with descriptive statistics for the selected risk drivers.
#'
#'@examples
#' if 	(interactive()) {
#'		srv$desc.stat <-  withProgress(message = "Running descriptive statistics report", 
#'							 value = 0, {
#'					desc.report(target = "qual", 
#'							rf = rf, 
#'							sc = sc, 
#'							sc.method = sc.method, 
#'							db = isolate(rv$db))
#'					})
#' 	}
#'
#'@export
desc.report <- function(target, rf, sc, sc.method, db) {
	y <- db[, target]
	rfl <- length(rf)
	res <- vector("list", rfl)
	for	(i in 1:rfl) {
		rf.n <- rf[i]
		incProgress(1 / (rfl - i + 1), detail = paste("Processing risk factor:", rf.n))
		x <- db[, rf.n]
		desc.l <- suppressWarnings( 
			    desc.stat(y = y, 
					  x = x, 
					  sc = sc, 
					  sc.method = sc.method)
			    )
		desc.l <- cbind.data.frame(risk.factor = rf.n, desc.l)
		res[[i]] <- desc.l		
		}
	res <- data.frame(bind_rows(res))
return(res)
}
#' Numeric arguments - monobin module
#'@param x Binning algorithm from monobin package.
#'@return Returns a list of two vectors: index and UI element label of numeric arguments of 
#' the selected monobin function.
#'@examples
#' if 	(interactive()) {
#'		inp.indx <- num.inputs(x = x)
#' 	}
#' num.inputs(x = "cum.bin")
#'
#'@export
num.inputs <- function(x) {
	switch(x, 
		 "cum.bin" = list(3, "Number of starting groups"),
		 "iso.bin" = list(c(4, 5), c("Minimum pct. of observations per bin", 
						     "Minimum avg. target rate per bin")),
		 "ndr.bin" = list(c(4, 5, 6), c("Minimum pct. of observations per bin", 
						        "Minimum avg. target rate per bin",
							  "p-value")),
		 "pct.bin" = list(3, "Number of starting groups"),
		 "sts.bin" = list(c(4, 5, 6), c("Minimum pct. of observations per bin", 
						        "Minimum avg. target rate per bin",
							  "p-value")),
		 "woe.bin" = list(c(4, 5, 6), c("Minimum pct. of observations per bin", 
						        "Minimum avg. target rate per bin",
						        "WoE threshold")),
		 "mdt.bin" = list(c(2, 5, 6), c("Number of splitting groups for each node", 
							  "Minimum pct. of observations per bin", 
						        "Minimum avg. target rate per bin")))
}
#' Check for numeric arguments - monobin module
#'@param x Binning algorithm from monobin package.
#'@param args.e Argument elements of the selected monobin function.
#'@return Returns a list of two vectors: logical if validation is successful and character vector
#' with validation message.
#'@examples
#' if 	(interactive()) {
#'		num.inp <- mono.inputs.check(x = bin.algo, args.e = args.e)
#' 	}
#'
#'@export
mono.inputs.check <- function(x, args.e) {
	inp.indx <- num.inputs(x = x)
	num.args <- c(args.e[inp.indx[[1]]], recursive = TRUE)
	check.01 <- any(is.na(num.args))
	if	(check.01) {
		msg <- paste0("Numeric input(s) required for: ", 
				  paste0(paste0("#", inp.indx[[2]], "#"), collapse = ", "), ".")
		return(list(check.ok = FALSE, msg = msg))
		}
	if	(x%in%c("cum.bin", "pct.bin")) {
		if	(num.args[1] < 2 | num.args[1] > 40) {
			msg <- paste0("Input #Number of starting groups# has to be between 2 and 40.")
			return(list(check.ok = FALSE, msg = msg))
			}
		}
	if	(x%in%c("iso.bin", "woe.bin")) {
		if	(any(num.args[1:2] < 0) | any(num.args[1:2] > 0.5)) {
			msg <- paste0("Inputs: ", 
					  paste0(paste0("#", inp.indx[[2]][1:2], "#"), collapse = ", "), 
					  " have to be between 0 and 0.5")
			return(list(check.ok = FALSE, msg = msg))
			}
		}
	if	(x%in%c("ndr.bin", "sts.bin")) {
		if	(any(num.args[1:2] < 0) | any(num.args[1:2] > 0.5)) {
			msg <- paste0("Inputs: ", 
					  paste0(paste0("#", inp.indx[[2]][1:2], "#"), collapse = ", "), 
					  " have to be between 0 and 0.5")
			return(list(check.ok = FALSE, msg = msg))
			}
		if	(num.args[3] < 0.001 | num.args[3] > 0.2) {
			msg <- paste0("Input #p-value# has to be between 0.001 and 0.2")
			return(list(check.ok = FALSE, msg = msg))
			}
		}
	if	(x%in%c("mdt.bin")) {
		if	(num.args[1] < 2 | num.args[1] > 150) {
			msg <- paste0("Input #Number of splitting groups for each node# has to be between 2 and 150.")
			return(list(check.ok = FALSE, msg = msg))
			}
		if	(any(num.args[2:3] < 0) | any(num.args[2:3] > 0.5)) {
			msg <- paste0("Inputs: ", 
					  paste0(paste0("#", inp.indx[[2]][2:3], "#"), collapse = ", "), 
					  " have to be between 0 and 0.5")
			return(list(check.ok = FALSE, msg = msg))
			}
		}
return(list(check.ok = TRUE, msg = "Inputs validated."))
}
#' Evaluation expression of the selected monobin function and its arguments
#'@param x Binning algorithm from monobin package.
#'@return Returns an evaluation expression of the selected monobin algorithm.
#'@examples
#' if 	(interactive()) {
#'		expr.eval <- monobin.fun(x = algo)
#' 	}
#' monobin.fun(x = "ndr.bin")
#'
#'
#'@export
monobin.fun <- function(x) {
	switch(x, 
		 "cum.bin" = "cum.bin(y = target, x = rf.l,  sc = sc,
					   sc.method = args.e[[2]], g = args.e[[3]],
					   y.type = args.e[[4]], force.trend = args.e[[5]])",
		 "iso.bin" = "iso.bin(y = target, x = rf.l,  sc = sc,
					   sc.method = args.e[[2]], y.type = args.e[[3]], 
					   min.pct.obs = args.e[[4]], min.avg.rate = args.e[[5]], 
					   force.trend = args.e[[6]])",
		 "ndr.bin" = "ndr.bin(y = target, x = rf.l,  sc = sc,
					   sc.method = args.e[[2]], y.type = args.e[[3]], 
					   min.pct.obs = args.e[[4]], min.avg.rate = args.e[[5]], 
					   p.val = args.e[[6]], force.trend = args.e[[7]])",
		 "pct.bin" = "pct.bin(y = target, x = rf.l,  sc = sc,
					   sc.method = args.e[[2]], g = args.e[[3]],
					   y.type = args.e[[4]], woe.trend = args.e[[5]],
					   force.trend = args.e[[6]])",
		 "sts.bin" = "sts.bin(y = target, x = rf.l,  sc = sc,
					   sc.method = args.e[[2]], y.type = args.e[[3]], 
					   min.pct.obs = args.e[[4]], min.avg.rate = args.e[[5]], 
					   p.val = args.e[[6]], force.trend = args.e[[7]])",
		 "woe.bin" = "woe.bin(y = target, x = rf.l,  sc = sc,
					   sc.method = args.e[[2]], y.type = args.e[[3]], 
					   min.pct.obs = args.e[[4]], min.avg.rate = args.e[[5]], 
					   woe.gap = args.e[[6]], force.trend = args.e[[7]])",
		 "mdt.bin" = "mdt.bin(y = target, x = rf.l,  sc = sc, g = args.e[[2]], 
					   sc.method = args.e[[3]], y.type = args.e[[4]], 
					   min.pct.obs = args.e[[5]], min.avg.rate = args.e[[6]], 
					   force.trend = args.e[[7]])")

}
#' Run monobin algorithm for the selected inputs
#'@param algo Binning algorithm from monobin package.
#'@param target.n Selected target.
#'@param rf Vector of a selected numeric risk factors.
#'@param sc Numeric vector of special case values.
#'@param args.e Argument elements of the selected monobin function.
#'@param db Data frame of target and numeric risk factors.
#'@return Returns a list of two data frame. The first data frame contains the results of implemented 
#' binning algorithm, while the second one contains transformed risk factors.
#'@examples
#' if 	(interactive()) {
#'		tbls <- withProgress(message = "Running the binning algorithm", 
#'						   value = 0, {
#'				  	  suppressWarnings(
#'				  	  monobin.run(algo = bin.algo, 
#'							  target.n = isolate(input$trg.select), 
#'							  rf = isolate(input$rf.select), 
#'							  sc = scr.check.res[[1]], 
#'							  args.e = args.e, 
#'							  db = isolate(rv$db))
#'			  )})
#'
#' 	}
#'
#'@export
monobin.run <- function(algo, target.n, rf, sc, args.e, db) {
	target <- db[, target.n]
	expr.eval <- monobin.fun(x = algo)
	rfl <- length(rf)
	res <- vector("list", rfl)
	res.trans <- vector("list", rfl)
	for	(i in 1:rfl) {
		rf.n <- rf[i]
		incProgress(1 / (rfl - i + 1), detail = paste("Processing risk factor:", rf.n))
		rf.l <- db[, rf.n]
		res.l <- eval(parse(text = expr.eval))
		if	(length(res.l) == 1) {
			res.tbl <- res.l[1]
			res.trans[[i]] <- NULL
			} else {
			res.tbl <- res.l[[1]]
			res.trans[[i]] <- data.frame(x = res.l[[2]])
			names(res.trans[[i]]) <- rf.n
			}
		res.tbl <- cbind.data.frame(risk.factor = rf.n, res.tbl)
		res[[i]] <- res.tbl
		}
	res <- bind_rows(res)
	res.trans <- bind_cols(res.trans)
	if	(nrow(res.trans) == 0) {
		res.trans <- cbind.data.frame(target = target)
		} else {
		res.trans <- cbind.data.frame(target = target, res.trans)
		}
	names(res.trans)[1] <- target.n
return(list(res, res.trans))
}





