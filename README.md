---
title: "README"
author: "CC & LH"
output: html_document 
runtime: shuny
---

# rinmap
An R interface to inMAP

### Installing rinmap
In RStudio, install the rinmap R package from github.  

#### Install and load devtools
```{r}
install.packages(c('devtools'))
library(data.table)
```
#### Install and load rinmap
```{r}
devtools::install_github("cchoirat/rinmap")
library(rinmap)
```
### rinmap example
#### Load the data
Load data with appropriate column headers - you can load sample 2005 data with the command ```data(units2005)```. 

Check out the data:
```{r}
units2005
#      V1      ID Latitude Longitude       SOx       CO2      NOx   Height    Diam Velocity     Temp
#  1:   1   1-Oct  32.6017  -87.7811 34091.460 3126122.3 5538.144 152.4000 7.39140  86.8680 284.7056
#  2:   2   2-Oct  32.6017  -87.7811 30665.058 2977011.0 5273.010 152.4000 7.39140  86.8680 284.7056
#  3:   3  1001-1  39.9239  -87.4272 34362.130 2797246.7 4619.338 152.4000 5.94360  88.3920 305.3722
#  4:   4  1001-2  39.9239  -87.4272 43279.336 3653868.6 6930.661 152.4000 5.94360  88.3920 305.3722
#  5:   5 10075-1  47.5314  -90.9111  1967.712  611774.3 1256.936  67.0560 3.01752  99.0600 302.3077
# ---                                                                                               
#779: 779   994-3  38.5267  -87.2525 17832.088 4038204.0 4518.585 189.8904 6.70560  47.5488 302.3722
#780: 780   994-4  38.5267  -87.2525 17850.150 4030024.0 5472.019 190.5000 6.70560  47.8536 303.3722
#781: 781   995-7  41.6433  -87.1225  2539.264 1100114.6 4075.654 146.3040 6.24840  39.6240 303.9222
#782: 782   995-8  41.6433  -87.1225  2180.948 2248060.4 8253.657 146.3040 6.24840  39.6240 303.9222
#783: 783  997-12  41.7203  -86.9097 16745.081 2991116.1 5068.963 153.9240 6.40080 101.1936 307.4278
```


#### Save as a csv file
```
path <- '.'
write.csv(units2005, 'units2005.csv')
```


#### run inmap to find impacts of emission reductions from all facilities in ```units2005```
inmap_impacts <- run_inmap('units2005.csv')


#### Utilities for individual facility impacts
You may want to simulate InMAP impacts for many facilities. ```rinmap``` includes utilities that produces and saves rinmap-ready ```.csv``` input files for scenarios that measure impacts one facility/unit at a time (```create_input_facility_one_by_one``` and ```create_input_unit_one_by_one```) or by leaving one facility/unit out (```create_input_facility_all_but_one``` and ```create_input_unit_all_but_one```). Inputs to these functions are simply the original ```.csv``` file with all units, for example:
```
create_input_facility_one_by_one.csv('units2005.csv')
```

#### Combine outputs into single dataset
```combine_inmap_out``` takes as input a directory that contains ```run_inmap``` output and combines it into a single data table linked with spatial data for plotting. Accepts a ```pattern``` argument for regex matching.


#### Plot results
Using zipcode-linked inMAP output from ```combine_inmap_output```, plot change in InMAP impacts
at zip code level compared to base year using ```plot_inmap```. For example,
```
setwd("~/Dropbox/Harvard/RFMeval_Local/InMAP")

# list files to read in
path.out = './RCE_output/'

#read in USA zip codes
zcta_shapefile <- '~/Dropbox/Harvard/RFMeval_Local/shapefiles/cb_2016_us_zcta510_500k/cb_2016_us_zcta510_500k.shp'


inmap <- combine_inmap_output(path.out,
pattern = 'inmap_2017diff')


inmap_2017diff_plots <- plot_inmap(inmap, 
                                   zcta_shapefile, 
                                   legend_lims=c(-10,0),
                                   plot.names = c('2017','2017 less E'),
                                   cores=2,
                                   gif.name = 'giffygif')

```




```run_inmap``` creates 

rmarkdown::render('README.md')













