# monobinSiny
This is an add-on package to the ```monobin``` package that simplifies its use. The goal of monobin is to perform monotonic binning of numeric risk factor in credit 
rating models (PD, LGD, EAD) development. All functions handle both binary and continuous target variable. Missing values and other possible special values are treated 
separately from so-called complete cases.

```monobinSiny``` provides shiny-based user interface (UI) to monobin packaga and it can be especially handy for less experienced R user as well as for those who intend to 
perform quick scanning of numeric risk factors when building credit rating models. The additional functions implemented in monobinShiny that do no exist in monobin package are
descriptive statistics, special case and outliers imputation. The function descriptive statistics is exported and can be used in R sessions independently from the user 
interface, while special case and outlier imputation functions are written to be used with shiny UI.



## Installation

You can install the released version of monobinShiny from [CRAN](https://CRAN.R-project.org) executing the following line of the code:

``` r
install.packages("monobinShiny")
```

## How to start shiny UI for monobin package?

To start shiny interface for monobin package, just type:

``` r
monobinShiny::monobinApp()
```
If the application is install and run properly, the following should appear in web browser:

![plot](./pics/pic00.png)

## DATA MANAGER MODULE
![plot](./pics/pic01.png)

> :warning: Be aware that only risk factors identified as of numeric type will be processed for other two modules. When import the data, in Data Import log output, overview of the data structure will be presented along with information about identified numeric / categorical risk factors.
