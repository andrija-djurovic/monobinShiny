# monobinShiny
This is an add-on package to the ```monobin``` package that simplifies its use. The goal of monobin is to perform monotonic binning of numeric risk factor in credit 
rating models (PD, LGD, EAD) development. All functions handle both binary and continuous target variable. Missing values and other possible special values are treated 
separately from so-called complete cases.

```monobinSiny``` provides shiny-based user interface (UI) to monobin package and it can be especially handy for less experienced R users as well as for those who intend to 
perform quick scanning of numeric risk factors when building credit rating models. The additional functions implemented in ```monobinShiny``` that do no exist in ```monobin``` 
package are: descriptive statistics, special case and outliers imputation. The function descriptive statistics is exported and can be used in R sessions independently from the 
user interface, while the special case and the outlier imputation functions are written to be used with shiny UI.


## Installation

User can install the released version of monobinShiny from [CRAN](https://CRAN.R-project.org) executing the following line of the code:

``` r
install.packages("monobinShiny")
```
Additionally, development version can be installed using:
 ```
     library(devtools)
     install_github("andrija-djurovic/monobinShiny")
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
1. data manager;
2. descriptive statistics and imputation;
3. monotonic binning.

Following sections provide short description of the each module. <br/>
> :information_source: Almost all reactive elements of the application result with a notification, so user should be aware of this as presented in the lower right corner.

## DATA MANAGER MODULE
This module serves for data import: the manual import browsing for a file (**only .csv files accepted**) or the automatic import of ```gcd``` from ```monobin``` package (Import dummy 
data).

![plot](./pics/pic01.png)

<br/>
During the manual data import, set of checks are performed such as: file extension, approprietness of .csv file and the number of identified numeric variables. 
If data are imported successfully, in <i>Data Import log</i> output, overview of the data structure will be presented along with information about identified numeric/categorical 
variables. <br/><br/>

> :warning: Be aware that only variables identified as of numeric type will be processed for other two modules. 

## DESCRIPTIVE STATISTICS AND IMPUTATION
This module covers the standard steps of univariate and part of bivariate analysis in model development supplemented by the simple options (mean or median) for imputation of special case values 
and outlier imputation based on selected percentiles thresholds.<br/><br/>

Before running any of the imputation procedures, target variable needs to be selected:

![plot](./pics/pic02.png)

<br/>
After selecting target variable, usually imputation procedures are run. 

![plot](./pics/pic03.png) <br/> <br/>


> :warning: Be aware that imputation procedures will create and add a new risk factor to imported data set. The special case values imputation will add the new risk factor 
            names as ```selected risk factor + _sc_ + selected imputation method ```. Example: if user selects risk factor age and mean as the imputation method, the new risk 
            factor will be added as age_sc_mean.
            The same procedure will run for the outlier imputation adding a new risk factor as  ```selected risk factor + _out_ + selected upper percentile + _ +  selected 
            lower percentile``` (e.g. age_out_0.99_0.01).
            Special attention should be paid when data set contains more risk factors, because the final number of risk factors can increase significantly using 
            imputations. 
            <br/>
            
> :information_source: In the case when a imputation values cannot be calculated, download buttons will appear providing possibility to the user to download and check for which 
                       risk factors inputs are not properly defined (all special case values and/or special case values to be imputed). Both fields, the all special case values 
                       and the special case values to be imputed should be defined as a list of numeric values (or values that can coerce to numeric including NA) separated by 
                       comma (,).<br/>
                       
Ultimate goal of this module is to create report of descriptive statistics. Image below presents example of descriptive report. Details on calculated metrics can be found in 
the help page of the function  ```desc.stat (?desc.stat)```.
As it can be seen, user has a possibility to download the descriptive statistics report as well as data set used for its creation. If the imputation procedures are run, the 
data set will contain added risk factors (.csv files).

![plot](./pics/pic04.png) 

## MONOTONIC BINNING
Monotonic binning module reflects the main purpose of this package - interface to ```monobin``` package. Similar to the previous module, user should first select the target 
variable, then the risk factors ready for binning (if imputation is performed, the list of available risk factors will contain newly created risk factors) and finally define 
arguments of selected binning algorithm. Available binning algorithms are those implemented in ```monobin``` package.

![plot](./pics/pic05.png)

Running the binning procedure will result in summary table of processed risk factors and transformed data set. Both outputs can be downloaded as .csv files.

![plot](./pics/pic06.png)

## Data checks and notifications
As already stated, almost every reactive element of the application produces notification output (lower righ corner). Example of error notification for trying to import file 
other than .csv is presented in the following image:

![plot](./pics/pic07.png)

Below is the list of data checks and notification implemented:
1. if imported file has at least two numeric variables;
2. if target variable is selected when running the imputation and report procedures;
3. if risk factors are selected when running imputation and report procedures;
5. if special case values are defined properly;
6. if percentile bounds (upper and lower) for outlier imputation are defined properly;
7. if numeric inputs for binning algorithms are defined properly;
8. if binary type of target is specified as 0/1 variable.

