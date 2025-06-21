# RSD

`RSD` (**R** **S**tochastic **D**ominance) is designed and developed to calculate
Stochastic Dominance (SD) and Almost Stochastic Dominance (ASD) in general. In
more details, given two probability mass functions (PMF), this package helps with:

* Calculate the first and second order SD values (FSD and SSD, respectively),
* Calculate the first and second order ASD values (AFSD and ASSD, respectively), 
* Compare two probability distributions by these methods.

# Installation

Install the released version of `RSD` from [CRAN](https://doi.org/10.32614/CRAN.package.RSD) by:

```r
install.packages("RSD")
```

Or from [GitHub](https://github.com/ShayanTohidi/RSD) by:

```r
install.packages("pak")
pak::pkg_install("ShayanTohidi/RSD")
```

# Getting Started

`RSD` provides a function, `createStochasticDominance()` to create the SD object to be
used in all other functions of this package. This function requires two discrete
distributions. Here, the example data set `data_ex` will be used for creating
the object:

```r
library(RSD)
outcome1 = data_ex$yield[data_ex$gen == 'B73/PHM49']
outcome2 = data_ex$yield[data_ex$gen == 'LH74/PHN82']
pr = rep(1/29,29)
sd.obj = createStochasticDominance(outcome1, outcome2, pr, pr)
```

Using this code, we can compare the distributions of the performance (yield) of
two cultivars.
The output of this code snippet, `sd.obj` contains all information for performing
SD and ASD comparisons. This is the main argument in the other functions of this
package.
