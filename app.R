# Required Libraries ----

# 1. Data Manipulation ----
library(tidyverse)
library(lubridate)

# 2. Shiny ----
library(shiny)
library(shinyjs)
library(shinythemes)

# 3. Extra Visualization ----
library(cowplot)
library(DT)
library(gridGraphics)
library(gridExtra)
library(magick)
library(scales)

# Read data
merged_games <- read.csv("merged_games.csv")
merged_games <- merged_games %>%
  select(-X) %>%
  mutate(Date = ymd(Date),
         System = fct_relevel(System, "TG16", "Genesis", "SNES",
                              "Saturn", "PS1", "N64"),
         Region = fct_relevel(Region, "Japan", "North_America", "PAL"))

launch_titles <- merged_games %>%
  group_by(System, Region) %>%
  arrange(Date) %>%
  filter(Date == first(Date))

# GET COLOR OF BAR GRAPH
pick_color <- function(values, colors, value) {
  idx <- values %>% { which(. == value) }
  colors[idx]
}

ui <- fluidPage( shinyjs::useShinyjs(), theme = shinytheme("sandstone"),
  navbarPage("4th & 5th Generation Game Console Releases",
    
    # WELCOME
    tabPanel("Welcome",
      h1("Introduction"),
      br(),
      p("Welcome! This site goes over our analysis of video game release data
        from the three major consoles from the 4th and 5th generations of game
        console releases, roughly coinciding with the 1990s: the NEC and Hudson
        Soft TurboGrafx-16 (or TG16, known as the PC Engine in Japan), the Sega
        Genesis (known as the Mega Drive in all regions outside of North
        America), and the Super Nintendo Entertainment System (or SNES, known as
        the Super Famicom in Japan) from the 4th Generation, and the Sega
        Saturn, Sony PlayStation (or PS1), and Nintendo 64 (or N64) from the 5th
        Generation. A console generation is usually separated by the release of
        a system that uses a significantly more advanced architecture than the
        preceding generation, and the separation between the 4th and 5th
        generation features the shift in most games from 2D to 3D. All of the
        game release data comes from combining the English Wikipedia articles
        for the games releases, for example, \"List of Super Nintendo
        Entertainment System Games\" for the SNES."),
      br(),
      p("Because of this particular shift, we were curious as to how the numbers
        of games released would vary from system-to-system and generation-to-
        generation, alongside if there were any significant changes in how many
        games were released in the three major regions: Japan, North America,
        and the PAL Region (which is the term often used to refer to Europe and
        sometimes Australia and Brazil, since most of those nations use the PAL
        color encoding system for analog TV, as opposed to North America and
        Japan's NTSC, although some nations, notably France, are included in
        this terminology for the PAL Region despite using a third encoding
        system called SECAM). Additionally, since many of the games had their
        full release date known, we could get info on how many games were
        released in a particular month or day of the week, which console or
        region had the most launch titles, or simply general trends relating
        to the publisher and developers from the data. Finally, there is a way
        to fully explore data per whichever systems, regions, and/or
        generations, as well as any particular of years you wish to choose."),
      br(),
      img(src='intro.png', height = 350, width = 900, align = 'center')
    ),
    
    # DATA BASICS
    # THE DATA
    navbarMenu("Data Basics",
    tabPanel("The Data",
      h1("The Data"),
      br(),
      p("As mentioned in the beginning, all data was pulled from the English
        Wikipedia pages for the releases of their respective consoles, all with
        the same \"List of X Games\" naming convention, with X being substituted
        for the full name of the console. The Sega and Nintendo systems feature
        the company's name in the console's name (e.g. Sega Genesis or Nintendo
        64), whereas the PlayStation and TurboGrafx-16 do not feature their
        company's name in the console's name."),
      br(),
      p("While all pages followed some formats similar to each other, most of
      them had their own way of specifying the game's different titles,
      developers, and publishers per region. The PlayStation games list was
      split into two pages, alphabetically from A-L and from M-Z, and the
      TurboGrafx-16 data did not specify a developer or publisher but just a
      \"distributor\", which was classified as a publisher when added to the
      overall data. Due to the inconsistencies with many of the naming
      conventions, some of the instances with multiple title, developer or
      publisher names could not be broken up properly, particularly for pages
      like for the Genesis games where there was just a line break in the same
      box as the break did not exist as part of the web scraped file's string
      data, and splitting it based on regular expressions could likely break
      an otherwise accurate name. However, all instances in which there was only
      one name, or instances where there was a bullet point before the alternate
      titles should for the most part be all accurately parsed."),
      br(),
      dataTableOutput("main_data")
      ),
    
    # DATA OVERVIEW
    tabPanel("Data Overview",
      # DISTRIBUTION OF RELEASES
      h1("Distribution of Releases"),
      br(),
      strong("Although the database has the release year for almost all titles,
        several cases only have the month, and fewer still the entire release
        date."),
      "So we would like to know whether the subset of those that have
        known release dates are well represented by the database as a whole by
        comparing the console and system distributions by region for both.",
      br(),
      br(),
      plotOutput("data1", height = 500),
      checkboxInput(inputId = "date",
                    label = "Check to only show releases with known release date",
                    value = FALSE),
      checkboxInput(inputId = "generation",
                    label = "Check to just compare distribution of console generations",
                    value = FALSE),
      br(),
      p("The two different ways to distribute the releases are fairly identical,
        though there is a lower range across all with known release dates as
        opposed to just the release year. The greater precision in the full date
        values allows for more separation between the median and one of the
        quantiles, particularly with the Sega Saturn across all regions due to
        it's small range of years its games were on the market. Outliers also
        change quite dramatically between the two methods. However, the only
        really significant change comes with the North American TurboGrafx-16
        data, whose distribution changes dramatically, since very few actual
        dates are known for its releases, the majority of which are the system's
        launch titles."),
    
      # NUMBER OF RELEASES FOR EACH TIME KNOWN
      h1("Number of Releases for Each Time Known"),
      p("Below is the proportion of releases with the most specific known time--
        i.e. what proportion of releases in the data have the full date, just
        the month and year, or just the year. For most instances, these
        proportions were very high, with the Genesis being the only notable
        exception. This likely is what caused the earlier distributions to be
        mostly identical when switching from just the year to the specific
        date."),
      br(),
      plotOutput("timeknown", height = 250),
    
      # RELEASES BY REGION
      h1("Releases by Region"),
      p("Comparing the number of releases by region and the number of regions
        per release shows that Japan had the most releases and that most
        releases were only in one region. North America and the PAL Territory
        were similar, as was the count of releases in 2 regions and the count
        in 3 regions. Japan being the most popular region for releases can
        likely be attributed to the fact that all of these particular console
        manufacturers (NEC, Hudson Soft, Sega, Nintendo, and Sony) are (or were
        in the case of Hudson Soft, which was absorbed into Konami in 2012)
        Japanese. Additionally, since both North America and the PAL Territory
        feature English-speaking populations and that it may be easier to
        translate from English to another PAL Region language as opposed to
        Japanese to either English or another PAL Region language, there are
        likely fewer releases exclusive to just one of North America or the PAL
        Region. This similarity is also interesting considering that the
        TurboGrafx-16 data did not include PAL releases."),
      br(),
      tabsetPanel(id = "tabs_data2",
                  tabPanel("All", value = "All"),
                  tabPanel("TurboGrafx-16", value = "TG16"),
                  tabPanel("Genesis", value = "Genesis"),
                  tabPanel("Super Nintendo", value = "SNES"),
                  tabPanel("Saturn", value = "Saturn"),
                  tabPanel("PlayStation", value = "PS1"),
                  tabPanel("Nintendo 64", value = "N64")),
      plotOutput("data2", height = 300)
    )),
    
    # VIEW BY SYSTEM
    navbarMenu("Detailed Timeline",
    tabPanel("View by System",
      sidebarPanel(
        h2("By System"),
        p("The distribution of the number of releases per year for each system
          is shown to the right, with the ability to show the proportions for
          each system per year underneath. The distribution of all systems
          combined is approximately bell-shaped, and in each of the tabs above
          to select individual systems, many of them are also fairly normal,
          with the TurboGrafx-16 and PlayStation being slighly right-skewed and
          the Genesis being slightly left-skewed. Clicking on the individual
          graphs can zoom them in to show them without maintaining the x and y
          axes of the original, all-system graph. 1996 appears to be the last
          year in which a substantial amount of games were released for a 4th
          generation console, primarily the SNES.")
      ),
      mainPanel(
        tabsetPanel(id = "tabs_system",
          tabPanel("All", value = "All", br(), em("Select other tabs to see the histogram of each system")),
          tabPanel("TurboGrafx-16", value = "TG16", br(), em("Click on graph to zoom in/out")),
          tabPanel("Genesis", value = "Genesis", br(), em("Click on graph to zoom in/out")),
          tabPanel("Super Nintendo", value = "SNES", br(), em("Click on graph to zoom in/out")),
          tabPanel("Saturn", value = "Saturn", br(), em("Click on graph to zoom in/out")),
          tabPanel("PlayStation", value = "PS1",br(), em("Click on graph to zoom in/out")),
          tabPanel("Nintendo 64", value = "N64", br(), em("Click on graph to zoom in/out"))),
        plotOutput("plot_by_system", height = 400, click = "zoom"),
        checkboxInput(inputId = "prop_system",
                      label = "Compare system proportions",
                      value = FALSE)
      )
    ),   
    
    # VIEW BY REGION
    tabPanel("View by Region",
      sidebarPanel(
        h2("By Region"),
        p("The distribution is the same as the system view, now just with the
          color separating releases by region. The low numbers of releases in
          1987, 1988, 2005, and 2006 for each console make their regional
          proportions the only ones entirely to one region, with the former
          two due to the TurboGrafx-16 and Genesis not releasing outside of
          Japan until 1989, and the latter two due to only a handful of
          PlayStation games being released those years due to the subsequent 6th
          generation consoles' release around 1999-2001, and the eventual
          discontinuation of the 5th generation consoles around 2004. The
          overall proportions between appear to be somewhat cubic, with North
          American and PAL releases being less common around 1989-1990, and
          Japanese releases less common around 2003-2004. Since the prior and
          subsequent 3rd and 6th generation consoles are not concluded in the
          overall data set, perhaps the proportion of games released per region
          is fairly constant, with about half of all releases being in Japan
          and about a quarter each for North America and the PAL Region. Like
          the system view, each individual region graph can be clicked on to
          zoom it in, and all the North American and PAL graphs are
          approximately bell-shaped, whereas the Japanese releases are
          somewhat left-skewed.")
      ),
      mainPanel(
        tabsetPanel(id = "tabs_region",
          tabPanel("All", value = "All", br(), em("Select other tabs to see the histogram of each region")),
          tabPanel("North America", value = "North_America", br(), em("Click on graph to zoom in/out")),
          tabPanel("Japan", value = "Japan", br(), em("Click on graph to zoom in/out")),
          tabPanel("PAL", value = "PAL", br(), em("Click on graph to zoom in/out")) ),
        plotOutput("plot_by_region", height = 400, click = "zoom"),
        checkboxInput(inputId = "prop_region",
                      label = "Compare Region Proportions",
                      value = FALSE)
      )
    ),
    
    # VIEW BY GENERATION
    tabPanel("View by Generation",
      sidebarPanel(
        h2("By Generation"),
        p("The distribution is the same as the previous two, and is actually
          identical to the system view if the Turbografx-16, Genesis, and SNES
          were the same color, and the Saturn, PlayStation, and Nintendo 64 were
          the same color. This shows better the slightly curved shift between
          generations, with the 5th generation having a lot of releases early in
          its lifespan, around 1995-1996, but the 4th generation having still a
          few more years of a few releases afterwards, from 1997 up until 2000,
          compared to only 1994 having a few 5th generation releases at the
          absolute start of the generation. Like the previous two views, the
          individual generation graphs can be clicked to zoom them in, and
          interestingly, the 4th generation is slightly left-skewed whereas the
          5th generation is slightly more-so right-skewed. The 5th generation's
          further skewness is why the separation line between the proportions of
          the two is slightly curved with more 4th generation releases well into
          the peak of the 5th generation.")
      ),
      mainPanel(
        tabsetPanel(id = "tabs_gen",
                    tabPanel("All", value = "All", br(), em("Select other tabs to see the histogram of each generation")),
          tabPanel("4th Gen", value = "4th", br(), em("Click on graph to zoom in/out")),
          tabPanel("5th Gen", value = "5th", br(), em("Click on graph to zoom in/out")) ),
        plotOutput("plot_by_gen", height = 400, click = "zoom"),
        checkboxInput(inputId = "prop_gen",
                      label = "Compare Region Proportions",
                      value = FALSE)
      )
    ),
    
    # BASIC VIEW BY (KNOWN RELEASE DATE)
    tabPanel("Basic View By (Known Release Date)",
      sidebarPanel(
        h2("By Known Release Date"),
        p("This distribution is just one more way to show the similarities
          between the year-only and full-date data, as the graphs and different
          color fills are all similar to the earlier views, with the only real
          difference being some more specificity with the overall graph having
          many small peaks rather than being what looked to be a smooth bell
          curve.")
      ),
      mainPanel(
        plotOutput("plot_by_date"),
        varSelectInput(inputId = "by_date_choose",
                       label = "Color Data By",
                       data = select(merged_games, 4, 9, 10),
                       selected = "Region")
      )
    )),
    
    # MONTH / DAY OF THE WEEK
    tabPanel("Month/Day of Week",
      h1("Data by Month / Day of the Week"),
      br(),
      p("Using the month and date data, below are graphs for the number of
        releases for a given month or day of the week. More games are released
        in November and December, and Thursday and Friday, and March is a
        secluded peak compared to the slowest months of January and May around
        it. November has a slightly higher proportion of North American releases
        than other months, and both Thursday and Friday have a far greater
        proporton of Japanese releases than any other day of the week, with
        North America being the most common region for every other day of the
        week, although it's almost tied with Japan on Saturday. While each
        system and generation are approximately equal in proportion over any
        given month, a larger proportion of PlayStation games were released on
        Thursday than any other system."),
      br(),
      plotOutput("month_week"),
      varSelectInput(inputId = "month_week_choose",
                     label = "Color Data By: ",
                     data = select(merged_games, 4, 9, 10),
                     selected = "Region")
    ),
    
    # LAUNCH TITLES
    tabPanel("Launch Titles",
      h1("Launch Titles"),
      br(),
      p("Below is a graph that just features the launch titles of each system by
        region, the games that are released alongside the system and thus are
        the first showcases of it's power and potential. Since these use actual
        dates, certain details that couldn't be seen in the year visualization
        can be seen, such as the fact that the 1994 launches of the Saturn and
        PlayStation in Japan happened near the very end of the year for both
        systems. As each system is Japanese in origin, the Japanese launches are
        the first for each system, but additionally, the PAL release can often
        be quite a bit after the North American one, with the Saturn and
        Playstation being the only systems to have their North American and PAL
        releases around the same time it looks like. Only two pairs of launches:
        the TurboGrafx-16 and Genesis in North America, and the Saturn and
        PlayStation in Japan, were of competing hardware coming out at
        approximately at the same time. The PAL launch of the PlayStation
        appears to have the most launch titles, at around 9, with the North
        American Genesis launch in second place at around 7 titles. The PAL
        Genesis and North American PlayStation launches, on the other hand,
        appears to be the only instances where there seems to be only one launch
        title."),
      br(),
      plotOutput("launchtitles"),
      varSelectInput(inputId = "launchtitles_choose",
                     label = "Color Data By: ",
                     data = select(merged_games, 4, 9, 10),
                     selected = "Region")
                     
    ),
    
    # DEVELOPER / PUBLISHER BASICS
    # DEVELOPER / PUBLISHER COUNTS
    navbarMenu("Developer / Publisher Basics",
       tabPanel("Developer / Publisher Counts",
          h1("Developer / Publisher Counts"),
          br(),
          p("Excluding the Turbografx-16, whose sole \"distributor\" column was 
          classified as the game's publisher, the number of developers and
          publishers per system looks to be about the same distribution, just
          with the number of developers being greater for each non-Turbografx
          system. The PlayStation had the most publishers or developers by far,
          with the Nintendo 64 and Turbografx-16 having the least.
"),
          br(),
          plotOutput("devpub_counts")
    ),
    
    # COMMON DEVELOPERS
    tabPanel("Common Developers",
      h1("Common Developers"),
      br(),
      p("This shows the number of games released for any developers with more
        than 50 releases, with Konami and Capcom having the greatest numbers and
        similar regional and system distributions. Some developers, such as
        Bandai, made games almost exclusively in Japan, and others were mostly
        split between North America and the PAL Region, such as Iguana
        Entertainment and Sculptured Software. The former, alongside Rare, are
        the only developers to make a significant number of games for the
        Nintendo 64, the console with the least known developers in the data
        set. Only one company in this list developed games exclusively for one
        region and one system, Lightspan, who developed games exclusively for
        the PlayStation in North America. Important to note is that some notable
        publishers, particularly Sega, 5th on this list but 1st in terms of
        publishing, and Nintendo, whose not on the developer list at all but 4th
        on the publishing list, have several internal studios themselves that
        split up their developing numbers, such as Sega AM1 or Nintendo EAD."),
      br(),
      plotOutput("common_dev", height = 600),
      varSelectInput(inputId = "common_dev_choose",
                     label = "Color Data By: ",
                     data = select(merged_games, 4, 9, 10),
                     selected = "Region")
    ),
    
    # COMMON PUBLISHERS
    tabPanel("Common Publishers",
      h1("Common Publishers"),
      br(),
      p("Since the number of publishers with over 50 games was far greater than
        the developers list, and it would start to include pairs of publishers
        (where a game would be published by a different company in a different
        region) and included some of the naming errors from the inconsistent
        writeups of the Wikipedia data, only publishers with over 100 releases
        are shown here. Many similar observations as in the common developers
        distribution can be made here. Of note is that Konami, the most prolific
        developer and second most prolific publisher, did not develop games for
        the Nintendo 64 but did publish them. Additionally, of all the companies
        involved in the console's manufacture (NEC's involvement in the
        Turbografx-16 is just in terms of the system's hardware), only Hudson
        Soft published titles frequently for a system that was not their own,
        with Sega, Nintendo, and Sony (their most prolific publisher,
        Sony Computer Entertainment America, listed here as SCEA) either
        exclusively or almost exclusively publishing only for their own
        systems."),
      br(),
      plotOutput("common_pub", height = 600),
      varSelectInput(inputId = "common_pub_choose",
                     label = "Color Data By: ",
                     data = select(merged_games, 4, 9, 10),
                     selected = "Region")
      
    )),
    
    # EXPLORE YOURSELF
    tabPanel(
      "Explore Yourself",
      h1("Explore Yourself"),
      br(),
      p("Pick your own filter criteria. Choose from regions, systems, etc.. and pick as many or as few criteria as you'd like!"),
      sidebarPanel(
        # regions
        checkboxGroupInput("region_free", h4("Regions"), 
                          choices = list("North America" = "North_America", 
                                         "Japan", "PAL"),
                          selected = "North_America"),
        # systems
        checkboxGroupInput("system_free", h4("Systems"), 
                          choices = list("TG16", "Genesis", "SNES",
                                         "Saturn", "PS1", "N64"),
                          selected = list("TG16", "Saturn")),
        # generations
        checkboxGroupInput("gen_free", h4("Generations"), 
                          choices = list("4th Gen" = "4th", "5th Gen" = "5th"),
                          selected = list("4th", "5th")),
        # year range
        sliderInput("year_range", h4("Year Range"),
                   min = 1986, max = 2007, value = c(1990, 2000), sep = ""),
        # color by
        varSelectInput("selectcolor", h4("Color Data By"),
                      data = select(merged_games, 4, 9, 10),
                      selected = "System")
      ),
      mainPanel(
        plotOutput("plot_free", height = 400)
      )
    )
  )
)


server <- function(input, output, session){
  # DATA BASICS
  # THE DATA
  output$main_data <- renderDataTable({
      merged_games
  })
  
  # DISTRIBUTION OF DATA
  output$data1 <- renderPlot({
    if (input$date == TRUE && input$generation == FALSE) {
      merged_games %>%
        ggplot(aes(x = System, fill = Region, y = Date)) +
        geom_boxplot() +
        labs(title = "Distribution of Releases with Known Release Dates by System and Region") +
        scale_fill_discrete(labels = c("Japan", "North America", "PAL Regions")) +
        theme_half_open() +
        theme(legend.position = "bottom")
    } else if (input$date == FALSE && input$generation == FALSE) {
      merged_games %>%
        ggplot(aes(x = System, fill = Region, y = Year)) +
        geom_boxplot() +
        labs(title = "Distribution of Releases per Year by System and Region") +
        scale_fill_discrete(labels = c("Japan", "North America", "PAL Regions")) +
        theme_half_open() +
        theme(legend.position = "bottom")
    } else if (input$date == TRUE && input$generation == TRUE) {
      merged_games %>%
        ggplot(aes(x = Generation, fill = Region, y = Date)) +
        geom_boxplot() +
        labs(title = "Distribution of Releases with Known Release Dates by Generation and Region") +
        scale_fill_discrete(labels = c("Japan", "North America", "PAL Regions")) +
        theme_half_open() +
        theme(legend.position = "bottom")
    } else {
      merged_games %>%
        ggplot(aes(x = Generation, fill = Region, y = Year)) +
        geom_boxplot() +
        labs(title = "Distribution of Releases per Year by Generation and Region") +
        scale_fill_discrete(labels = c("Japan", "North America", "PAL Regions")) +
        theme_half_open() +
        theme(legend.position = "bottom")
    }
  })
  
  #NUMBER OF RELEASES FOR EACH TIME KNOWN
  output$timeknown <- renderPlot({
    merged_games %>%
      group_by(System) %>%
      mutate(Has_Date = !is.na(Date),
             Has_Month = !is.na(Month),
             Has_Year = !is.na(Year)) %>%
      summarise(Date = mean(Has_Date),
                Month = mean(Has_Month),
                Year = mean(Has_Year)) %>%
      pivot_longer(c(Date:Year), names_to = "Most_Specific_Time", values_to = "Proportion") %>%
      ggplot(aes(x = Most_Specific_Time, y = Proportion, fill = System)) +
      geom_col() +
      facet_grid(cols = vars(System)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_half_open() +
      labs(title = paste("Proportion of Releases with Most Specific Known Time by System", sep = "")) +
      xlab("Most Specific Time")
  })
  
  #RELEASES BY REGION
  output$data2 <- renderPlot({
    if(input$tabs_data2 != "All") {
      colors = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")
      fill_color = pick_color(levels(merged_games$System), colors, input$tabs_data2)
      # logo
      logo_file <- paste("./extdata/",input$tabs_data2, ".png", sep="")
      
      p1 <- merged_games %>%
        filter(System == !!input$tabs_data2) %>%
        group_by(Title, Region) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = Region)) +
        geom_bar(fill = fill_color) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_half_open() +
        labs(title = paste("Number of Games Released by Region (", input$tabs_data2, ")", sep = "")) +
        ylab("Number of Games Released")
      p2 <- merged_games %>%
        group_by(Title, System) %>%
        filter(System == !!input$tabs_data2) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = Count)) +
        geom_bar(fill = fill_color) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_half_open() +
        labs(title = paste("Number of Regions per Release (", input$tabs_data2, ")", sep = "")) +
        xlab("Number of Regions") +
        ylab("Number of Games Released")
      p2 <- ggdraw(p2) + 
        draw_image(logo_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)
      ptlist = list(p1, p2)
      grid.arrange(grobs=ptlist, ncol=length(ptlist))
    } else {
      p1 <- merged_games %>%
        group_by(Title, Region) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = Region)) +
        geom_bar(fill ="#c2b280") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_half_open() +
        labs(title = "Number of Games Released by Region") +
        ylab("Number of Games Released")
      p2 <- merged_games %>%
        group_by(Title, System) %>%
        summarise(Count = n()) %>%
        ggplot(aes(x = Count)) +
        geom_bar(fill ="#c2b280") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_half_open() +
        theme(panel.grid.major.x = element_blank()) +
        labs(title = "Number of Regions per Release") +
        xlab("Number of Regions") +
        ylab("Number of Games Released")
      ptlist = list(p1, p2)
      grid.arrange(grobs=ptlist, ncol=length(ptlist))
    }
  })
  
  # show/hide the prop buttons
  observe({
    if (input$tabs_system == "All") {
      shinyjs::show("prop_system")
    } else {
      shinyjs::hide("prop_system")
    }
    if (input$tabs_region == "All") {
      shinyjs::show("prop_region")
    } else {
      shinyjs::hide("prop_region")
    }
    if (input$tabs_gen == "All") {
      shinyjs::show("prop_gen")
    } else {
      shinyjs::hide("prop_gen")
    }
  })
  # detect click on graph to zoom
  plot_data <- reactiveValues(trigger = 0)
  observe({
    req(input$zoom)
    isolate(plot_data$trigger <- plot_data$trigger + 1)
  })
  observe({
    req(input$tabs_system, input$tabs_region, input$tabs_gen)
    plot_data$trigger <- 0
  })
  
  # VIEW BY SYSTEM
  output$plot_by_system <- renderPlot({
    if (input$tabs_system != "All") { # Filter Individual
      colors = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")
      fill_color = pick_color(levels(merged_games$System), colors, input$tabs_system)
      g <- merged_games %>% filter(System == !!input$tabs_system) %>%
        ggplot(aes(x = Year)) +
        geom_bar(fill = fill_color) + 
        theme(legend.position = "bottom") +
        theme_minimal_hgrid() +
        labs(title = paste("Number of Games Released on the", input$tabs_system, "by Year")) +
        ylab(paste("Number of Games Released"))
      # click to zoom
      if (plot_data$trigger %% 2 == 0) {
        g <- g + scale_x_continuous(limits=c(1987, 2006)) +
          scale_y_continuous(limits=c(0, 1500))
      } else {
        g <- g + scale_x_continuous(breaks= pretty_breaks())
      }
      # logo
      logo_file <- paste("./extdata/",input$tabs_system, ".png", sep="")
      ggdraw(g) + 
        draw_image(logo_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)
    } else { # ALL
      g <- merged_games %>% ggplot(aes(x = Year, fill = System)) +
        theme_minimal_hgrid() +
        scale_x_continuous(breaks= pretty_breaks()) +
        labs(title = "Number of Games Released on All Systems by Year") +
        ylab("Number of Games Released") +
        scale_fill_discrete(labels = c("TurboGrafx-16", "Genesis", "Super Nintendo",
                                       "Saturn", "PlayStation", "Nintendo 64")) +
        theme(legend.position = c(.8, .9),
              legend.background = element_rect(fill = "white"))
      if (input$prop_system) { # View PROP
        g + geom_bar(position = "fill")
      } else {
        g + geom_bar()
      }
    }
  })
  
  
  #VIEW BY REGION
  output$plot_by_region <- renderPlot({
    if (input$tabs_region != "All") { # Filter Individual
      # color
      colors = c("#F8766D", "#00BA38", "#619CFF")
      fill_color = pick_color(levels(merged_games$Region), colors, input$tabs_region)
      g <- merged_games %>% filter(Region == !!input$tabs_region) %>%
        ggplot(aes(x = Year)) +
        geom_bar(fill = fill_color) + theme(legend.position = "bottom") +
        theme_minimal_hgrid() +
        labs(title = paste("Number of Games Released in", input$tabs_region, "by Year")) +
        ylab(paste("Number of Games Released in", input$tabs_region))
      # click to zoom
      if (plot_data$trigger %% 2 == 0) {
        g <- g + scale_x_continuous(limits=c(1987, 2006)) +
          scale_y_continuous(limits=c(0, 1500))
      } else {
        g <- g + scale_x_continuous(breaks= pretty_breaks())
      }
      # logo
      logo_file <- paste("./extdata/",input$tabs_region, ".png", sep="")
      ggdraw(g) + 
        draw_image(logo_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)
    } else { # ALL
      g <- merged_games %>% ggplot(aes(x = Year, fill = Region)) +
        theme_minimal_hgrid() +
        scale_x_continuous(breaks= pretty_breaks()) +
        labs(title = "Number of Games Released Globally by Year") +
        ylab('Number of Games Released Globally') +
        scale_fill_discrete(labels = c("Japan", "North America", "PAL Regions")) +
        theme(legend.position = c(.8, .9),
              legend.background = element_rect(fill = "white"))
      if (input$prop_region) { # View PROP
        g + geom_bar(position = "fill")
      } else {
        g + geom_bar()
      }
      
    }
  })
  
  
  #VIEW BY GENERATION
  output$plot_by_gen <- renderPlot({
    if (input$tabs_gen != "All") { # Filter Individual
      # color
      colors = c("#F8766D", "#00BFC4")
      fill_color = pick_color(c("4th", "5th"), colors, input$tabs_gen)
      g <- merged_games %>% filter(Generation == !!input$tabs_gen) %>%
        ggplot(aes(x = Year)) +
        geom_bar(fill = fill_color) + theme(legend.position = "bottom") +
        theme_minimal_hgrid() +
        labs(title = paste("Number of", input$tabs_gen, "Games Released by Year")) +
        ylab(paste("Number of", input$tabs_gen, "Games Released"))
      # click to zoom
      if (plot_data$trigger %% 2 == 0) {
        g <- g + scale_x_continuous(limits=c(1987, 2006)) +
          scale_y_continuous(limits=c(0, 1500))
      } else {
        g <- g + scale_x_continuous(breaks= pretty_breaks())
      }
      g
    } else { # ALL
      g <- merged_games %>% ggplot(aes(x = Year, fill = Generation)) +
        theme_minimal_hgrid() +
        scale_x_continuous(breaks= pretty_breaks()) +
        labs(title = "Number of Games Released by Year") +
        ylab('Number of Games Released') +
        theme(legend.position = c(.8, .9),
              legend.background = element_rect(fill = "white"))
      if (input$prop_gen) { # View PROP
        g + geom_bar(position = "fill")
      } else {
        g + geom_bar()
      }
    }
  })
  
  #VIEW BY (DATE)
  output$plot_by_date <- renderPlot({
    merged_games %>%
      ggplot(aes(x = Date, fill = !!input$by_date_choose)) +
      geom_histogram() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_grid() +
      labs(title = "Chronology of Releases with Known Release Dates") +
      xlab("Year") +
      ylab("Number of Games Released") +
      scale_fill_discrete(labels = c("TurboGrafx-16", "Genesis", "Super Nintendo",
                                     "Saturn", "PlayStation", "Nintendo 64"))
  })
  
  # MONTH / DAY OF THE WEEK
  output$month_week <- renderPlot({
    p1 <- merged_games %>%
      ggplot(aes(x = Month, fill = !!input$month_week_choose)) +
      geom_bar() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid() +
      labs(title = "Number of Releases per Month") +
      ylab("Number of Games Released") +
      scale_x_continuous(breaks = c(1:12))
    p2 <- merged_games %>%
      mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) %>%
      filter(is.na(Weekday) == FALSE) %>%
      ggplot(aes(x = Weekday, fill = !!input$month_week_choose)) +
      geom_bar() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid() +
      labs(title = "Number of Releases Per Day of the Week") +
      scale_x_discrete(labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
      ylab("Number of Games Released")
    ptlist = list(p1, p2)
    grid.arrange(grobs=ptlist, ncol=length(ptlist))
  })
  
  # LAUNCH TITLES
  output$launchtitles <- renderPlot({
    launch_titles %>%
      ggplot(aes(x = Date, fill = !!input$launchtitles_choose)) +
      geom_histogram() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid() +
      labs(title = "Launch Titles by Release") +
      ylab("Number of Games Released")
  })
  
  # DEVELOPER / PUBLISHER BASICS
  # DEVELOPER / PUBLISHER COUNTS
  output$devpub_counts <- renderPlot({
    p1 <- merged_games %>%
      group_by(Developer, System) %>%
      summarise(Count = n()) %>%
      ggplot(aes(x = System)) +
      geom_bar(fill = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid() +
      labs(title = "Number of developers by system") +
      ylab("Number of Developers")
    p2 <- merged_games %>%
      group_by(Publisher, System) %>%
      summarise(Count = n()) %>%
      ggplot(aes(x = System)) +
      geom_bar(fill = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid() +
      labs(title = "Number of publishers by system") +
      ylab("Number of Publishers")
    ptlist = list(p1, p2)
    grid.arrange(grobs=ptlist, ncol=length(ptlist))
  })

  # COMMON DEVELOPERS
  output$common_dev <- renderPlot({
    merged_games %>%
      group_by(Developer) %>%
      mutate(Dev_Count = n()) %>%
      filter(Dev_Count >= 50,
             Developer != is.na(Developer)) %>%
      ggplot(aes(y = reorder(Developer, Dev_Count), fill = !!input$common_dev_choose)) +
      geom_bar() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_vgrid() +
      labs(title = "Number of Games Released for Developers with Over 50 Releases") +
      xlab("Number of Games Released") +
      ylab("")
  })
  
  # COMMON PUBLISHERS
  output$common_pub <- renderPlot({
    merged_games %>%
      group_by(Publisher) %>%
      mutate(Pub_Count = n()) %>%
      filter(Pub_Count >= 100,
             Publisher != is.na(Publisher)) %>%
      ggplot(aes(y = reorder(Publisher, Pub_Count), fill = !!input$common_pub_choose)) +
      geom_bar() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_vgrid() +
      labs(title = "Number of Games Released for Publishers with Over 100 Releases") +
      xlab("Number of Games Released") +
      ylab("")
  })
  
  #EXPLORE YOURSELF
  output$plot_free <- renderPlot({
    merged_games %>% filter(Region %in% !!input$region_free,
                            System %in% !!input$system_free,
                            Generation %in% !!input$gen_free,
                            Year >= input$year_range[1],
                            Year <= input$year_range[2]
                            ) %>%
      ggplot(aes(x = Year, fill = !!input$selectcolor)) +
      geom_bar() + theme(legend.position = "bottom") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid() + 
      ylab("Number of Games Released") +
      scale_x_continuous(breaks = pretty_breaks())
  })
}

shinyApp(ui, server, options = list(height = 700))