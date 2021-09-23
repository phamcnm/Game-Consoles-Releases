library(tidyverse)
library(rvest)
library(lubridate)
library(readr)

has_date <- function(x) {
  str_detect(x, "^[JFMASOND][^,]+,") #if the release date had the actual day, then it would start with one of the letters of the month's name and include a comma after the month
}
has_month <- function(x) {
  str_detect(x, "^[JFMASOND][^,]+$") #if it just had the month, it would have no comma after the month
}
has_year <- function(x) {
  str_detect(x, "^[12]") #if it was just the year, it would start with a 1 or a 2
}
#create functions in order to determine whether or not a game's release date has only a year, year and month, or the full date included.

add_date_info <- function(x, sys_name, gen_name) { #input in the name of the individual system set, and then a custom system name and generation name as variables
  df <- x %>%
    mutate(Has_Date = has_date(Release),
           Has_Month = has_month(Release),
           Has_Year = has_year(Release))
  #create logical vectors for release dates of each entry per the above functions
  df_date <- df %>%
    filter(Has_Date == TRUE) %>%
    mutate(Date = mdy(Release),
           Month = month(Date),
           Year = year(Date))
  df_month <- df %>%
    filter(Has_Month == TRUE) %>%
    mutate(Month = month(mdy(Release)),
           Year = year(mdy(Release)))
  df_year <- df %>%
    filter(Has_Year == TRUE) %>%
    mutate(Year = as.numeric(Release))
  #create separate data sets for each isolated time specification to merge together
  df <- bind_rows(df_date, df_month)
  df <- bind_rows(df, df_year)
  #merge the rows together
  df <- df %>%
    select(-Has_Date, -Has_Month, -Has_Year) %>%
    arrange(Title, Region) %>%
    mutate(System = sys_name, Generation = gen_name)
  #get rid of the logical vectors, resort the data by title and region, and then add the system and generation as a final variable for once all individual system sets are to be merged
  x <- df
}

# SNES Games Data

url <- paste0("https://en.wikipedia.org/wiki/List_of_Super_Nintendo_Entertainment_System_games")
h <- read_html(url)
tab <- h %>% html_nodes("table") #make a temporary "tab" data set to do most of the regex work and column adjustment
tab <- tab[[1]] %>% html_table #some of the systems had earlier tables that games some additional information on how the main table was sorted
colnames(tab) <- c("Title", "Developer", "Publisher", "Japan", "North_America", "PAL", "Ref") #the order of the regions (in this case Japan, NA, and PAL), was also different from wiki page to wiki page
tab <- tab %>%
  select(-Ref) %>%
  filter(Title != "Title") %>% #the title here was just a copy of the original header to get rid of it, as it became an observation when scraped, the name of the title, due to Wikipedia citations, etc., was typically different for each system
  mutate(Title = str_replace_all(Title, "•", " / "), #the bullet point was used on some of the wiki pages to separate different titles for different regions
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"), #this separates any regional markers (i.e. JP for Japan) that would normally just be at the end of the title to it's own parenthetical
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"), #same as the above, but for instances before a slash
         Title = str_replace_all(Title, " †", ""), #one instance I recall in the SNES data set had a dagger at the end, so I got rid of it and kept the code in case for all systems
         Publisher = str_replace_all(Publisher, "\\)([A-Z])", "\\) / \\1"), #similar code for publisher names as the title ones above
         North_America = str_replace_all(North_America, " \\(.+\\)", "")) #there was one game, Cannondale Cup, which had a rerelease as Exertainment Mountain Bike Rally in NA and I chose to remove the second release date since the two were the same game
#many of the adjustments made to the temporary "tab" data set were altered for each on individually
#all of these particulars made it so I couldn't figure out how to make a function to handle all of them
snes_games <- tab %>%
  pivot_longer(c(Japan:PAL), names_to = "Region", values_to = "Release")
#tidy the SNES games so that each game was listed 3 times for each region

snes_games <- add_date_info(snes_games, "SNES", "4th")
#running the add_date_info function would also only include games for regions that it was actually released in

ggplot(snes_games, aes(x = Year, fill = Region)) + geom_bar()
ggplot(snes_games, aes(x = Date, fill = Region)) + geom_histogram()
#test graphs to see if the data was working okay, and to see initially what graphing the data would look like

# TG16 Games Data

url <- paste0("https://en.wikipedia.org/wiki/List_of_TurboGrafx-16_games")
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[2]] %>% html_table
colnames(tab) <- c("Title", "Publisher", "North_America", "Japan", "Format") #no PAL release, and NA before Japan. Format refers to whether or not the game was released on card or on CD for the Turbografx-CD add-on, a column that I chose to get rid of
tab <- tab %>%
  select(-Format) %>%
  filter(Title != "Main Title") %>%
  mutate(Title = str_replace_all(Title, "•", " / "), #same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"), #same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"), #same as SNES data
         Title = str_replace_all(Title, " †", ""), #same as SNES data
         Publisher = str_replace_all(Publisher, "\\)([A-Z])", "\\) / \\1"), #same as SNES data
         North_America = str_replace_all(North_America, " \\(.+\\)", ""), #same as SNES data
         North_America = str_replace_all(North_America, "\\[.+\\]$", ""), #since there was no reference column, this was to get rid of Wikipedia's citations
         Japan = str_replace_all(Japan, "\\[.+\\]$", ""), #same as above
         Japan = str_replace_all(Japan, "([1289]{4}).+", "\\1")) #two Japanese release dates had two release dates in one value, I chose to keep the earlier one

tg16_games <- tab %>%
  pivot_longer(c(North_America:Japan), names_to = "Region", values_to = "Release")

tg16_games <- add_date_info(tg16_games, "TG16", "4th")

# Genesis Games Data

url <- paste0("https://en.wikipedia.org/wiki/List_of_Sega_Genesis_games")
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[2]] %>% html_table
colnames(tab) <- c("Title", "Developer", "Publisher", "Japan", "North_America", "PAL", "Other")
tab <- tab %>%
  select(-Other) %>%
  filter(Title != "Title(s)[12][13][14]") %>%
  mutate(Title = str_replace_all(Title, "//s", " / "), #the Genesis data didn't have the bulletpoints, so I attempted to get rid of spaces/newlines to try and separate regional name changes, but I couldn't 
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"), #same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"), #same as SNES data
         Title = str_replace_all(Title, " †", ""), #same as SNES data
         Publisher = str_replace_all(Publisher, "\\)([A-Z])", "\\) / \\1"), #same as SNES data
         North_America = str_replace_all(North_America, " \\(.+\\)", ""), #same as SNES data
         North_America = str_replace_all(North_America, "\\[.+\\]", ""), #same as TG16 data
         Japan = str_replace_all(Japan, "\\[.+\\]", ""), #same as TG16 data
         PAL = str_replace_all(PAL, "\\[.+\\]", "")) #same as TG16 data, but for PAL release dates

gen_games <- tab %>%
  pivot_longer(c(Japan:PAL), names_to = "Region", values_to = "Release")

gen_games <- add_date_info(gen_games, "Genesis", "4th")

# Saturn Games Data

url <- paste0("https://en.wikipedia.org/wiki/List_of_Sega_Saturn_games")
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[1]] %>% html_table
colnames(tab) <- c("Title", "Developer", "Publisher", "North_America", "PAL", "Japan", "Other")
tab <- tab %>%
  filter(Title != "Title[1][2][3][4][5][6]") %>%
  mutate(Title = str_replace_all(Title, "•", " / "), # same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"), # same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"), # same as SNES data
         Title = str_replace_all(Title, " †", ""), # same as SNES data
         Publisher = str_replace_all(Publisher, "\\)([A-Z])", "\\) / \\1"), # same as SNES data
         North_America = str_replace_all(North_America, " \\(.+\\)", ""), # same as SNES data
         North_America = str_replace_all(North_America, "\\[.+\\]", ""), # same as TG16 data
         Japan = str_replace_all(Japan, "\\[.+\\]", ""), # same as TG16 data
         PAL = str_replace_all(PAL, "\\[.+\\]", "")) # same as TG16 data

sat_games <- tab %>%
  pivot_longer(c(North_America:Japan), names_to = "Region", values_to = "Release")

sat_games <- add_date_info(sat_games, "Saturn", "5th")

# PS1 Games Data

url <- paste0("https://en.wikipedia.org/wiki/List_of_PlayStation_games_(A%E2%80%93L)")
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[1]] %>% html_table
colnames(tab) <- c("Title", "Developer", "Publisher", "Japan", "PAL", "North_America", "Ref")
tab <- tab %>%
  select(-Ref) %>%
  filter(Title != "Title") %>%
  mutate(Title = str_replace_all(Title, "•", " / "), # same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"), # same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"), # same as SNES data
         Title = str_replace_all(Title, " †", ""), # same as SNES data
         Publisher = str_replace_all(Publisher, "\\)([A-Z])", "\\) / \\1"), # same as SNES data
         North_America = str_replace_all(North_America, " \\(.+\\)", ""), # same as SNES data
         North_America = str_replace_all(North_America, "\\[.+\\]", ""), # same as TG16 data
         Japan = str_replace_all(Japan, "\\[.+\\]", ""), # same as TG16 data
         PAL = str_replace_all(PAL, "\\[.+\\]", ""), # same as TG16 data
         Japan = str_replace_all(Japan, "([0-9]{4}).+", "\\1"), #some release dates had multiple, particularly PAL, separating Australia form the rest for some games
         North_America = str_replace_all(North_America, "([0-9]{4}).+", "\\1"), #same as above
         PAL = str_replace_all(PAL, "([0-9]{4}).+", "\\1")) #same as above

ps1_games_al <- tab %>%
  pivot_longer(c(Japan:North_America), names_to = "Region", values_to = "Release")

url <- paste0("https://en.wikipedia.org/wiki/List_of_PlayStation_games_(M%E2%80%93Z)")
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[1]] %>% html_table
colnames(tab) <- c("Title", "Developer", "Publisher", "Japan", "PAL", "North_America", "Ref")
tab <- tab %>%
  select(-Ref) %>%
  filter(Title != "Title") %>%
  mutate(Title = str_replace_all(Title, "•", " / "),
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"),
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"),
         Title = str_replace_all(Title, " †", ""),
         Publisher = str_replace_all(Publisher, "\\)([A-Z])", "\\) / \\1"),
         North_America = str_replace_all(North_America, " \\(.+\\)", ""),
         North_America = str_replace_all(North_America, "\\[.+\\]", ""),
         Japan = str_replace_all(Japan, "\\[.+\\]", ""),
         PAL = str_replace_all(PAL, "\\[.+\\]", ""),
         Japan = str_replace_all(Japan, "([0-9]{4}).+", "\\1"),
         North_America = str_replace_all(North_America, "([0-9]{4}).+", "\\1"),
         PAL = str_replace_all(PAL, "([0-9]{4}).+", "\\1"))

ps1_games_mz <- tab %>%
  pivot_longer(c(Japan:North_America), names_to = "Region", values_to = "Release")

ps1_games <- bind_rows(ps1_games_al, ps1_games_mz) #since the PS1 data was in two halves, this merged them together

ps1_games <- add_date_info(ps1_games, "PS1", "5th")

# N64 Games Data

url <- paste0("https://en.wikipedia.org/wiki/List_of_Nintendo_64_games")
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[2]] %>% html_table
colnames(tab) <- c("Title", "Developer", "Publisher", "Genre", "First_Release",
                   "Japan", "North_America", "PAL", "A", "B", "C")
tab <- tab %>%
  select(-c(Genre, First_Release, A, B, C)) %>%
  filter(Title != "Title[11][12][13][14]") %>%
  mutate(Title = str_replace_all(Title, "•", " / "), # same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}$)", "\\1 (\\2)"), # same as SNES data
         Title = str_replace_all(Title, "([a-z])([A-Z]{2,3}) /", "\\1 (\\2) /"), # same as SNES data
         Title = str_replace_all(Title, " †", ""), # same as SNES data
         North_America = str_replace_all(North_America, " \\(.+\\)", ""), # same as SNES data
         North_America = str_replace_all(North_America, "\\[.+\\]", ""), # same as TG16 data
         Japan = str_replace_all(Japan, "\\[.+\\]", ""), # same as TG16 data
         PAL = str_replace_all(PAL, "\\[.+\\]", ""), # same as TG16 data
         Japan = str_replace_all(Japan, "([0-9]{4}).+", "\\1"), # same as PS1 data
         North_America = str_replace_all(North_America, "([0-9]{4}).+", "\\1"), # same as PS1 data
         PAL = str_replace_all(PAL, "([0-9]{4}).+", "\\1")) # same as PS1 data

n64_games <- tab %>%
  pivot_longer(c(Japan:PAL), names_to = "Region", values_to = "Release")

n64_games <- add_date_info(n64_games, "N64", "5th")

merged_games <- bind_rows(snes_games, gen_games, tg16_games, ps1_games, sat_games, n64_games)

merged_games$Developer[merged_games$Developer == ""] <- NA
merged_games$Publisher[merged_games$Publisher == ""] <- NA
#turning some blank developer and publisher entries into NA

write.csv(merged_games, "merged_games.csv")
#writing the merged games file so this page didn't have to be in the app.R file

vgsales <- read_csv("vgsales.csv")
vgsales <- transform(vgsales, Year = as.numeric(Year))
sales <- vgsales %>%
  drop_na(Year) %>%
  group_by(Year) %>%
  summarise(total_NA_sales = sum(NA_Sales),
            total_JP_sales = sum(JP_Sales),
            total_EU_sales = sum(EU_Sales),
            total_other_sales = sum(Other_Sales),
            total_global_sales = sum(Global_Sales))
#we considered using some sales data for some of the best selling games, but in the end chose not to