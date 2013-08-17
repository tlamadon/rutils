Rutils
======

Utilies for R. In particular check out `ddmergev` and `spread` which I tend to use all the time.

### Install

    install.packages('devtools');require(devtools);install_github('rutils',user='tlamadon')

### Functions

 - `recover_once` sets R in recover on error, but will recover only first error.
 - `multiplot` allows to combine multiple plots into one image.
 - `renameany` renames columns or string or factors within a data.frame.
 - `dist` computes the norm realtive norm between two multidimensional arrays.
 - `spread` allows to extend a multidimensional array in any dimension.
 - `getNormCop`  generates a Normal transition matrix using Gaussian copula.
 - `ddmergev`  merges 2 data.frame using a formula example of formula is `var1 + var2 | key1 + key2`.

### Contributors

 - [Thibaut Lamadon](https://github.com/tlamadon)
