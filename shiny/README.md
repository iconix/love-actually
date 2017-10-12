_Love, Actually_ Network Analysis ([detached fork](https://github.com/dgrtwo/love-actually-network))
--------------------

Originally created by [David Robinson](http://varianceexplained.org/r/love-actually-network/) in R, the major difference in this Shiny fork is that it uses a [`love_actually_data.rda`](https://github.com/iconix/love-actually/blob/master/python/love_actually_data.rda) file generated in Python - see the Python code at [iconix/love-actually](https://github.com/iconix/love-actually).

Shiny app shown <a href="https://iconix.shinyapps.io/love-actually-network/">here</a>, which plots a network of [_Love, Actually_](http://www.imdb.com/title/tt0314331/) connections at various points of the movie.

Pre-deploy: Signed up for a shinyapps.io account with my GitHub and claimed a URL.

Deploy steps:
```
install.packages(c('rsconnect', 'shiny', 'ggplot2', 'networkD3', 'dplyr', 'reshape2'))
rsconnect::setAccountInfo(name='<NAME>',
			  token='<TOKEN>',
			  secret='<SECRET>')
library(rsconnect)
rsconnect::deployApp('path/to/your/app')
```

I blog about the conversion from R to Python here:
