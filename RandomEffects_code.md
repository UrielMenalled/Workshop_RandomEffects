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

``` r
#Each year is stored separately, I'll load them in, calculate total weed biomass, and combind the dataframes
WintData1_tmp <- read.csv("https://ecommons.cornell.edu/server/api/core/bitstreams/adb24a0c-0b2c-4bd6-90ad-e203c270bba3/content")
WintData2_tmp <- read.csv("https://ecommons.cornell.edu/server/api/core/bitstreams/f05bf7b9-3c15-48ec-9f28-0c978cf0560b/content")

str(WintData1_tmp) #loaded in nicely
str(WintData2_tmp) #loaded in nicely

#We don't want to do weed community analysis today, just simple models of total weed biomass
WintData1 <-
  WintData1_tmp %>% 
  rowwise() %>%
  mutate(WeedBiomass = sum(c_across(-c(Trial, Year, Site, Block, Plot, CoverCrop, CoverCropBiomass, canola, 
                                       cereal.rye, hairy.vetch)), na.rm = FALSE)) %>% 
  ungroup() %>% 
  select(Year, Site, Block, Plot, CoverCrop,WeedBiomass)

WintData2 <-
  WintData2_tmp %>% 
  rowwise() %>%
  mutate(WeedBiomass = sum(c_across(-c(Trial, Year, Site, Block, Plot, CoverCrop, CoverCropBiomass, canola, 
                                       cereal.rye, hairy.vetch)), na.rm = FALSE)) %>%
  ungroup() %>% 
  select(Year, Site, Block, Plot, CoverCrop,WeedBiomass)

#Combine data frames
WintData_tmp <- bind_rows(WintData1,WintData2)

WintData <- WintData_tmp %>% mutate(across(-WeedBiomass, as.factor))
str(WintData) #looks good!
```

\#Modeling

I want to see how weed biomass is impacted by cover crop treatment,
basically WeedBiomass~CoverCrop. However, my experiment has clear
nesting! I must account for non-independence through a model with more
fixed and/or random effects. Looking at my experimental design (picture
below) we can see that there are **three** *total* levels.

1.  Site and Year are at an equal level because both sites are in both
    years and visa versa. there are 2 levels of each site and year

2.  Block is within Site and Year. I know this because we used different
    fields in each site year, so block 1 in Musgrave during year 1 **is
    not** the same as block 1 in Musgrave during year 2. Because each
    block is within a unique site year, there are 2 sites x 2 years x 4
    blocks = 16 groups.

3.  Cover crop treatments are within blocks, this is the level at which
    I made my observations.

As nice it would be to account for Site and Year through a random
effect, we don’t have enough levels of either group. Remember we need at
least three groups per variable for it to be a random effect. Thus,
these variables will be fixed effects. On the other hand, block has 16
levels across site-years, it can be a random effect! As for CoverCrop,
this is the level of observation, it does not need to be a random
effect.

<figure>
<img src="Example_MapDesign.png" width="452"
alt="Here is a nice picture of the map and experimental design of the data we are working with today" />
<figcaption aria-hidden="true">Here is a nice picture of the map and
experimental design of the data we are working with today</figcaption>
</figure>
