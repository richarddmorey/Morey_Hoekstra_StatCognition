<!-- README.md is generated from README.Rmd. Please edit that file -->
Resources for Morey & Hoekstra (2019) <img src="man/figures/logo.png" align="right" alt="" width="120" />
=========================================================================================================

Data, analysis code, and demonstrations for Morey & Hoekstra’s (2019)
statistical cognition experiment, which was run over the Christmas
season in 2018. You can find more details at
<a href="https://richarddmorey.github.io/Morey_Hoekstra_StatCognition/" class="uri">https://richarddmorey.github.io/Morey_Hoekstra_StatCognition/</a>.

-   [Installation](#installation)
-   [Task demonstration](#task-demonstration)
-   [Shiny app](#shiny-app)

Installation
------------

If you’re using a recent version MacOS, you’ll need to install
[Xquartz](https://www.xquartz.org/index.html) for the fonts.

To fully use the package, you’ll probably need to install several
packages that are not on CRAN:

    # install.packages('devtools') # if needed
    devtools::install_github("crsh/papaja")
    devtools::install_github("rstudio/gt")

Then you can install this package with

    devtools::install_github("richarddmorey/Morey_Hoekstra_StatCognition")

Task demonstration
------------------

A short video showing the task can be found under
[`img/vignettes/santa_task_example.mp4`](https://github.com/richarddmorey/Morey_Hoekstra_StatCognition/blob/master/vignettes/img/santa_task_example.mp4)
(click ‘View raw’ to download).

You can find an [online version of the task
demonstration](articles/task_demo.html).

Users of the package can also load a version of the experimental task
from within R:

    task_demo()

Shiny app
---------

Running the included shiny app will allow you to explore individual
participants’ response patterns.

    explore_data()

You can access an already-deployed version of the shiny app at
<a href="https://richarddmorey.shinyapps.io/explore/" class="uri">https://richarddmorey.shinyapps.io/explore/</a>.

The app will let you:

-   Search, sort, and filter the data set by experimental condition,
    response, etc.
-   See the selected participant’s interface at the poiint of decision
    (both experimental samples and random shuffle reports)
-   See an animation showing the selected participant’s behaviour over
    time, up to the point of decision
-   Read the selected participant’s responses to the questions about
    their strategy
-   Search the strategy text using [POSIX regular
    expressions](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html),
    optionally (try “chance” to search for all instances of “chance”;
    “color|colour” will search for mentions of either of the two
    spelling variants of “color”)
