# monobinSiny
Shiny based user interface for monobin R package

<!-- badges: start -->
<!-- badges: end -->

The goal of monobin is to perform monotonic binning of numeric risk factor in credit 
rating models (PD, LGD, EAD) development. All functions handle both binary and 
continuous target variable. Missing values and other possible special values are treated 
separately from so-called complete cases.

## Installation

You can install the released version of monobin from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("monobin")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
suppressMessages(library(monobin))
data(gcd)
amount.bin <- cum.bin(x = gcd$amount, y = gcd$qual)
amount.bin[[1]]
gcd$amount.bin <- amount.bin[[2]]
gcd %>% group_by(amount.bin) %>% summarise(n = n(), y.avg = mean(qual))
#increase default number of groups (g = 20)
amount.bin.1 <- cum.bin(x = gcd$amount, y = gcd$qual, g = 20)
amount.bin.1[[1]]
#force trend to decreasing
cum.bin(x = gcd$amount, y = gcd$qual, g = 20, force.trend = "d")[[1]]
```
Picture 1:
![Alt text](/../<branch name>/path/to/image.png?raw=true "Optional Title")


