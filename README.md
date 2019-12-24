sunmoon provides graphs of the sun and moon trajectories through the sky.  It does two main things.

1. Create graphs shown at a website
   (http://emit.phys.ocean.dal.ca/~kelley/sunmoon) that shows this graph for
Halifax, Nova Scotia, Canada.  That site is auto-updated daily with the
following 'crontab' entry.
```
20 2 * * * /usr/local/bin/R --no-save < /Users/kelley/Sites/sunmoon/sunmoon4halifax.R
```

2. Supply an R shiny script that lets the user alter the day of prediction.
As noted on the website, this can be run by
```
source("http://emit.phys.ocean.dal.ca/~kelley/sunmoon/sun_moon_app.R")
shinyApp(ui, server)
```
although users may find it preferable to use a local file from a clone of this
github repository.


