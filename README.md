---  
dev version: 0.0.0.9000  
---  


# aermod


The _aermod_ package provides a data frame interface for EPA's air dispersion modeling platform `AERMOD`. 

> __Disclaimer:__ The Windows operating system is required to run EPA's modeling _.EXE_ file. This package is currently designed to only run one emission source at a time. Multiple sources may be run as a batch within a `for` loop.


**AERMOD** is a steady-state plume model that incorporates air dispersion based on planetary boundary layer turbulence and scaling concepts, including treatment of both surface and elevated sources, and both simple and complex terrain.  

http://www.epa.gov/ttn/scram/dispersion_prefrec.htm#aermod


## Install 

```r
install.packages("remotes")

remotes::install_github(c("dKvale/installEPA", 
                          "dKvale/bpip", 
                          "dKvale/receptors",
                          "dKvale/aermod"))
```

## Examples

Let's create an AERMOD receptor file: `receptors.rou`. We'll set the grid to have circles of receptors at every 5 meters from 5 to 100 meters from the center point.

```r
library(aermod)
library(receptors)

recepts <- polar_grid(radii = seq(5, 100, 5))

write_rou(recepts, "receptors.rou")
```
