#' Descriptive statistics
#'
#' \code{desc.stat} returns the descriptive statistics of numeric risk factor. Reported metrics covers mainly
#' univariate and part of bivariate analysis which are usually standard steps in credit rating model development.
#' Metrics are reported for special (if exists) and complete case groups separately. 
#' Report includes:
#' \itemize{
#'   \item risk.factor: Risk factor name.
#'   \item type: Special case or complete case group.
#'   \item bin: When special case method is \code{together} then bin is the same as type, otherwise
#'              all special cases are reported separately.
#'   \item cnt: Number of observations.
#'   \item pct: Percentage of observations.
#'   \item min: Minimum value. 
#'   \item p1, p5, p25, p50, p75, p95, p99: Percentile values.
#'   \item avg: Mean value.  
#'   \item avg.se: Standard error of mean.
#'   \item max: Maximum value.  
#'   \item neg: Number of negative values.    
#'   \item pos: Number of positive values.   
#'   \item cnt.outliers: Number of outliers. 
#'                       Records above and below \code{Q75 + 1.5 * IQR}, where \code{IQR = Q75 - Q25}, where 
#'			 IQR is interquartile range.	
#'}
#'@param x Numeric risk factor.
#'@param y Numeric target vector (binary or continuous).
#'@param sc Numeric vector with special case elements. Default values are c(NA, NaN, Inf). 
#' Recommendation is to keep the default values always and add new ones if needed. 
#' Otherwise, if these values exist in x and are not defined in the sc vector, function will report the error.
#'@param sc.method Define how special cases will be treated, all together or in separate bins.
#' Possible values are \code{"together", "separately"}.
#'@return Data frame of descriptive statistics metrics, separately for complete and special case groups.
#'
#'@examples
#' suppressMessages(library(monobinShiny))
#' data(gcd)
#' desc.stat(x = gcd$age, y = gcd$qual)
#' gcd$age[1:10] <- NA
#' gcd$age[50:75] <- Inf
#' desc.stat(x = gcd$age, y = gcd$qual, sc.method = "together")
#' desc.stat(x = gcd$age, y = gcd$qual, sc.method = "separately")
#'
#'@importFrom graphics boxplot
#'@importFrom stats cor quantile sd
#'@importFrom utils capture.output read.csv str write.csv
#'@import dplyr
#'@export
desc.stat <- function(x, y, sc = c(NA, NaN, Inf), sc.method = "together") {
	scm.opts <- c("together", "separately")
	cond.01 <- !is.numeric(x) | !is.numeric(y) | ifelse(length(sc) == 1, ifelse(is.numeric(sc) | is.na(sc), FALSE,  TRUE), 
							    ifelse(is.numeric(sc), FALSE, TRUE)) 
	cond.02 <- !sc.method[1]%in%scm.opts
	if	(cond.01) {
		stop("x, y & sc have to be numeric vectors")
		}
	if	(cond.02) {
		stop(paste0("sc.method  argument has to be one of: ", 
				paste0(scm.opts, collapse = ", ")))
		}
	d <- data.frame(y, x)
	d <- d[!is.na(y), ]
	if	(sc.method[1] == "together") {
		d$bin <- ifelse(d$x%in%sc, "special cases", "complete cases")
		} else {
		d$bin <- ifelse(d$x%in%sc, d$x, "complete cases")
		}
	d$type <- factor(ifelse(d$bin%in%"complete cases", "complete cases", "special cases"),
			     levels = c("special cases", "complete cases"), 
			     ordered = TRUE)
	if	(sum(d$type%in%"special cases") == nrow(d)) {
		res <- suppressWarnings(
			 d %>% 
			 group_by(type, bin = as.character(bin)) %>%
			 summarise(cnt = n(),
				     pct = n() / nrow(d),
			 .groups = "drop")
			 )
		res <- data.frame(res)
		} else {
		res <- suppressWarnings(
			 d %>% 
			 group_by(type, bin) %>%
			 summarise(cnt = n(),
				     pct = n() / nrow(d),
				     min = min(x, na.rm = TRUE),
				     p1 = quantile(x, prob = 0.01, na.rm = TRUE),
				     p5 = quantile(x, prob = 0.05, na.rm = TRUE),				
				     p25 = quantile(x, prob = 0.25, na.rm = TRUE),
				     p50 = quantile(x, prob = 0.50, na.rm = TRUE),
				     avg = mean(x, na.rm = TRUE), 
				     avg.se = sd(x, na.rm = TRUE) / sqrt(n()), 
				     p75 = quantile(x, prob = 0.75, na.rm = TRUE),				
				     p95 = quantile(x, prob = 0.95, na.rm = TRUE),	
				     p99 = quantile(x, prob = 0.99, na.rm = TRUE),	
				     max = max(x, na.rm = TRUE),
				     neg = sum(x < 0),
				     pos = sum(x > 0),
				     cnt.outliers = length(boxplot(x[!is.na(x)], plot = FALSE)$out),
			  .groups = "drop")
			 )
		res <- data.frame(res)
		cc <- d[d$type%in%"complete cases", ]
		if	(length(unique(cc$y)) == 1 | length(unique(cc$x)) == 1) {
			res$monotonicity <- "unique value of y or x"
			} else {
			cor.coef <- cor(cc$y, cc$x, method = "spearman")
			sign.cor.coef <- ifelse(!is.na(cor.coef),
					     ifelse(cor.coef >= 0, "positive", "negative"), NA)
			res$monotonicity <- sign.cor.coef
			}
		}
return(res)
}

