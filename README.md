# monobinShiny
This is an add-on package to the ```monobin``` package that simplifies its use. The goal of monobin is to perform monotonic binning of numeric risk factor in credit 
rating models (PD, LGD, EAD) development. All functions handle both binary and continuous target variable. Missing values and other possible special values are treated 
separately from so-called complete cases.

```monobinSiny``` provides shiny-based user interface (UI) to monobin packaga and it can be especially handy for less experienced R user as well as for those who intend to 
perform quick scanning of numeric risk factors when building credit rating models. The additional functions implemented in ```monobinShiny``` that do no exist in ```monobin``` 
package are descriptive statistics, special case and outliers imputation. The function descriptive statistics is exported and can be used in R sessions independently from the 
user interface, while special case and outlier imputation functions are written to be used with shiny UI.


## Installation

You can install the released version of monobinShiny from [CRAN](https://CRAN.R-project.org) executing the following line of the code:

``` r
install.packages("monobinShiny")
```

## How to start monobinShiny application?

After installation, to start shiny application, just type:

``` r
suppressMessages(library(monobinShiny))
monobinShinyApp()
```
If the application is installed and started properly, the following should appear in web browser:

![plot](./pics/pic00.png)

Application consists of the three modules:
1. data manager
2. descriptive statistics and imputation
3. monotonic binning

Following sections provide short description of the each module.
> :information_source: Almost all reactive elements of the application results with a notification, so be aware of this as presented in the right lower corner.

## DATA MANAGER MODULE
This module serves for data import: manual import of prepared .csv file (browsing for a file) or automatic import of ```gcd``` from ```monobin``` package (Import dummy data).

![plot](./pics/pic01.png)

During manual data import, set of checks are performed such as: 

> :warning: Be aware that only risk factors identified as of numeric type will be processed for other two modules. When import the data, in Data Import log output, overview of the data structure will be presented along with information about identified numeric / categorical risk factors.
