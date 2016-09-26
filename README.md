# Bring the Heat

Preliminary analysis in R of Chicago's professional ultimate frisbee team's 2015 season stats.  
For an overview, check out `bh_knit.md`  
("Bring the heat" for the cheer fans do at games.)

## Workflow
* Import, tidy, and summarize data by player in a table
* Create a graph to visualize season MVP

## Files
* Data in ChicagoWildfire2015-stats.csv pulled from [UltiAnalytics](http://www.ultianalytics.com/app/#/5671536392404992/players)
* `bh_knit.Rmd` is an Rmarkdown file that outlines the munging and produces an example plot. `bh_knit.md` is the same, but the plot renders on GitHub (though the LaTex equation for calculating plus-minus doesn't :disappointed:)
* `R/bh_prep.R` cleans the data
* `R/bh_stats.R` computes some summary statistics and a outlines a few simple models
* `R/bh_plot.R` makes a few plots in ggplot2

## Other
More info on the [Wildfire](http://theaudl.com/teams/wildfire)
