Rutils
======

Utilies for R. In particular check out `ddmergev` and `spread` which I tend to use all the time.

### Install

    install.packages('devtools');require(devtools);install_github('rutils',user='tlamadon')

### Functions

 - `recover_once` set R in recover on error, but will do it only once.
 - `multiplot` allows to combine multiple plots into one plot
 - `renameany` will rename columns or string or factors within a data.frame
 - `dist` compute the norm realtive norm between two multidimensional arrays
 - `spread` allows to extend a multidimensional array in any dimension
 - `getNormCop`  generates a Normal transition matrix using Gaussian copula
 - `ddmergev`  merges 2 data.frame using a formula example of formula is `var1 + var2 | key1 + key2`

### Contributors

 - [Thibaut Lamadon](https://github.com/tlamadon)
