# ggtreeSpace

This package is a comprehensive visualization tool specifically designed for 
exploring phylomorphospace. It not only simplifies the process of generating 
phylomorphospace, but also enhances it  with the capability to add graphic 
layers to the plot with grammar of graphics to create fully annotated
phylomorphospaces.

## Installation

The released version from `Bioconductor`

```r
if (!require("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("ggtreeSpace")
```
Alternatively, you can grab the development version from github using
devtools:

``` r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("YuLab-SMU/ggtreeSpace")
```

## Quick Example
``` r
 library(ggtree)
 library(phytools)
 library(ggtreeSpace)
 
 tr <- rtree(15)
 td <- fastBM(tr, nsim = 2)
 ggtreespace(tr, td) +
  geom_tippoint()
```

## Author

-   [Guangchuang Yu](https://guangchuangyu.github.io) Professor, PI
-   [Lin Li](https://github.com/SanL20) Master’s Student
