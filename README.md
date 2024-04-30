# How to Navigate the Shiny app - READ THIS PLEASE

Please read this and make sure that you understand what you're looking at.

## First time working with Shiny?

Shiny is a package in R that lets you create dashboards. When opening and cloning this repository, there will be two folders - `Shinyapp` and `prototype`. The original dashboard is in `Shinyapp` and the newer prototype is in `prototype/shinyapp`. You'll have two files in each folder - `server.R` for the dashboard's backend and `ui.R` for the frontend. Make sure to **install all dependencies before running** and once you do there's a button titled "Run App" on the top right. You can click on this in either file and the dashboard will load.

### Upon loading the prototype dashboard

Upon loading the dashboard, you'll see that there are six tabs - **sociodemographics, education, economics, media & entertainment, politics & justice, and people & values.** Now, the `data`, `markdown`, and `code` folders are all organized with respect to each tab so that it'll be easy to point what code, data or Markdown files belong where. At the same time, if you scroll through the tabs, you'll notice that a lot of plots and maps are interactive - those are Plotly plots and Leaflet maps. You're gonna need to understand how those work.

### Upon closer inspection

If you look at the files under `code`, each file corresponds to either cleaned datasets or server functions. We ask you to clean all your data in these files - ***do NOT create server functions in these files.*** We learned a bit too late that this will cause desync issues between `server.R` and the respective file. If you look at the `data` folder, each folder under `data` will have subfolders except for `sodem` and `education`. Please keep it this way, and if you're gonna add new data under a new tab please follow these conventions.

### Theming and coloring

Your sponsors will most likely ask you to follow their theming guide next semester - please try your hardest to follow these conventions and create color palettes as functions or lists.

## Required dependencies for the dashboard to work

* Shiny - self-explanatory.
* `bslib` - an R library that allows [Bootstrap](https://www.w3schools.com/whatis/whatis_bootstrap.asp) functionality (responsive web development) and adds necessary dashboard widgets (cards and value boxes).
* Leaflet - a wrapper for a JavaScript library used for interactive maps
* Plotly - a wrapper for a JavaScript plotting library
* `tidyverse` - necessary for data cleaning.

# Future Work - PLEASE READ

Our Capstne team (Spring 2024) started working on the second rendition of this dashboard and is heavily inspired by the original dashboard and we ask you to continue this work.

**TL;DR READ THE HEADERS AND LOOK AT ALL THE `TODO` MARKERS ON EVERY FILE**

## Data Collection & Cleaning

All the datasets in `sodem` and `education` are updated to 2022. However, the datasets are not updated past 2019 on every other folder and the functions cleaning these datasets are outdated, lack commenting and therefore are impossible to work with. Please follow our footsteps and find and clean ***INSIDE R AND NOT ON EXCEL*** the data for each tab. Ideally, use `tidyverse` libraries to clean these datasets as they're far faster than using base R. This will be extremely tedious so take your time with this and do this one tab at a time, and clean your data under `code`.

If you want an example, take a look at `code/education.R` for a baseline on what to expect. At the same time, if you look at `code/media.R` and `code/people_values.R`, you'll see that there are Shiny server functions inside these files - please move them back to `server.R` and only focus on cleaning data in these files.

## Dashboard Interactivity

The education tab is the only tab within this dashboard that *really* allows you to interact with the data given to you. We ask you to keep following this sort of interactivity throughout the rest of the dashboard. How you do it is completely up to you and your sponsors.

## Standardizing All Plots to Plotly

We ask you to standardize all your plots to Plotly. It's not a hard library to use, and there's even a `ggplotly()` function that mostly does a good job creating Plotly plots from ggplot objects. The majority of plots on the dashboard are still ggplot objects but we'd rather use Plotly for the extra interactivity.

## All Geographic Data Should Come From `geo_data.rds`

The current dashboard has a bad habit of recycling geographic data. We ask you to use the data from `data/geo_data.rds` to create your Shapefile objects (datasets that are compatible with Leaflet) and Leaflet maps.

## Go Through Every `TODO` Comment

We've left `TODO` comments for you to pick up where we left off.

### Mobile compatibility?

The current dashboard doesn't have mobile compatibility. This could be worth a try. This is not at all a priority and moreso a suggestion.

# References

* [Mastering Shiny](https://mastering-shiny.org/)
* [Shiny Docs](https://shiny.posit.co/r/reference/shiny/latest/)
* [bslib Docs](https://rstudio.github.io/bslib/)
* [Leaflet Docs](https://rstudio.github.io/leaflet/)
* [Plotly Docs](https://plotly.com/r/)

# Good luck!

We hope that this guide helps you navigate through this semester a little bit more easily! ❤️

Yours truly,

Marcos from The BRAND Band
