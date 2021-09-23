# final-project-minhpham-connertaylor

## Link to shinyapps.io page:

## List of files:
* `app.R`: the main file that runs the shiny app
* `datacreation.R`: an R file that was used to create the merged data set, contains all of the web scraping, regex work, and tidying
* `merged_games.csv`: the merged data set, containing data on games released for 6 video game consoles spanning two generations and over a decade of releases
* `extdata`: a folder of images used in some of the graphs

## Methodology
We went to the Wikipedia articles for the games released for each of the six consoles we were to look at: the [TurboGrafx-16](https://en.wikipedia.org/wiki/List_of_TurboGrafx-16_games), [Sega Genesis](https://en.wikipedia.org/wiki/List_of_Sega_Genesis_games), [Super Nintendo](https://en.wikipedia.org/wiki/List_of_Super_Nintendo_Entertainment_System_games), [Sega Saturn](https://en.wikipedia.org/wiki/List_of_Sega_Saturn_games), [PlayStation](https://en.wikipedia.org/wiki/List_of_PlayStation_games_(A%E2%80%93L))-- which was [split into two pages](https://en.wikipedia.org/wiki/List_of_PlayStation_games_(M%E2%80%93Z)), and [Nintendo 64](https://en.wikipedia.org/wiki/List_of_Nintendo_64_games). These were scraped into individual `.csv` files which then had the `Title`, `Developer`, and `Publisher` columns edited to be accurately parsed by manipulation of regular expressions (although due to some inconsistency between the Wikipedia articles and how a game's different, regional titles was displayed in some of the articles, there were some instances in which the names of things could not be parsed without messing up another game's name that we could figure out). Then the three columns (two in the case of the TurboGrafx-16) for regional release dates were tidied into their own observations, and the data was split into three depending on how specific the release date was: having the entire date, just month and year, and just year. These three sets were then combined back together to complete an entire system's data, of which all six were merged together into one final data set, `merged_games.csv`, to be manipulated into the final shiny app.
