Random effects demo
================

\#Loading

``` r
library(lmerTest)
```

    ## Loading required package: lme4

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(tidyverse)
```

    ## Warning: package 'lubridate' was built under R version 4.4.1

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::expand() masks Matrix::expand()
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ tidyr::pack()   masks Matrix::pack()
    ## ✖ tidyr::unpack() masks Matrix::unpack()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Today we will be using a subset of data published in Menalled et
al. (2023): <https://doi.org/10.1038/s41598-023-43987-x>

This experiment looks at changes in weed communities across 10 cover
crop treatments (5 summer cover crop treatments, 5 winter cover crop
treatments). Today, we will just work with winter cover crops
(i.e. tilled control, canola, cereal rye (CR), hairy vetch (HV), and HV
x CR mix).

Here is a nice picture of the map and experimental design of the data we
are working with today

<img src="Example_MapDesign.png" width="452" />

``` r
#read.csv()
```
