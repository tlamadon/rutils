Rutils
======

Utilies for R. In particular check out `ddmergev` and `spread` which I tend to use all the time.

### Install

    install.packages('devtools');require(devtools);install_github('rutils',user='tlamadon')

### Functions

#### `recover_once`

Don't you find it irritating to set `options(error=recover)` and then it keeps stopping until you set `options(error=NULL)`? The function `recover_once()` will set the option to recover once. Which is most of the time exactly what you want!

#### `multiplot`

Do you miss the matlab function `subplot` ? Well you don't have to anymore, this function allows you to create seperate plots and then combine then easily by calling:

    multiplot(gp1,gp2,gp3,gp4,cols=2)

The code was pretty much borrowed from the [R cookbook](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)

### Contributors

 - [Thibaut Lamadon](https://github.com/tlamadon)
