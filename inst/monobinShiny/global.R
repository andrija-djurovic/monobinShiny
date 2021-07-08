monobin.algo <- c("cum.bin", "iso.bin", "ndr.bin", "pct.bin", "sts.bin", "woe.bin")
rv <- reactiveValues(sync = 0, sync2 = 0, sync3 = 0, sync23 = 0, dm.uptd = 0, import.dummy = FALSE, 
		    dwnl.sync = 0, db = NULL, target.select.2 = NULL, target.select.3 = NULL, 
		    num.rf = NULL, rf.imp = NULL, rf.out = NULL)

