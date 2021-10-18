# monobinShiny 0.1.0
Changes:<br/>
1. new binning algorithm available - ```mdt.bin```. This change is triggered by the changes in ```monobin``` package.
2. in MONOTONIC BINNING module, now special case field accepts only ```NA``` as an input.
3. ```sc``` argument of ```desc.stat``` now accepts ```NA``` only, not producing the error that numeric vector is needed. Condition
```!is.numeric(sc)``` replaced with ```ifelse(length(sc) == 1, ifelse(is.numeric(sc) | is.na(sc), FALSE, TRUE), ifelse(is.numeric(sc), FALSE, TRUE))```
