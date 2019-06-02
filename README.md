# Resources for Morey & Hoekstra (2019)

Data, analysis code, and demonstrations for Morey & Hoekstra's (2019) statistical cognition experiment, which was run over the Christmas season in 2018.

**Contents**

* [Installation](#installation)
* [Text and supplements](#text-and-supplements)
* [Task demonstration](#task-demonstration)
* [Task code](#task-code)
* [Shiny app](#shiny-app)
* [Data](#data)
* [Figure list](#figure-list)
* [Compressed data](#compressed-data)
* [Analysis code](#analysis-code)





## Installation 

If you're using a recent version MacOS, you'll need to install [Xquartz](https://www.xquartz.org/index.html) for the fonts.

To fully use the package, you'll probably need to install several packages that are not on CRAN:

``` r
# install.packages('devtools') # if needed
devtools::install_github("crsh/papaja")
devtools::install_github("rstudio/gt")
```

Then you can install this package with

``` r
devtools::install_github("richarddmorey/Morey_Hoekstra_StatCognition")
```


## Text and supplements

The package vignettes offer information about the experiment and results.

| Vignette  | Description  | Source file name  | R code to read  | Web link |
|:---|:---|:---|:---|:---|
| Manuscript  |  Draft of manuscript  | `vignettes/manuscript.Rmd`  | `vignette("manuscript", package = "MoreyHoekstra2019")`  | |
| Supplement A  | Methods details and extra results  | `vignettes/supplementA.Rmd`  | `vignette("supplementA", package = "MoreyHoekstra2019")`  | |
| Supplement B  | Inclusion criteria and demographic/opinion questions  |  `vignettes/supplementB.Rmd` | `vignette("supplementB", package = "MoreyHoekstra2019")`   | |

Vignettes explain the experiment and summarising the data set. Supplements add to the analysis in the main manuscript.

## Task demonstration

A short video showing the task can be found under [`vignettes/santa_task_example.mp4`](https://github.com/richarddmorey/Morey_Hoekstra_StatCognition/blob/master/vignettes/santa_task_example.mp4)  (click 'View raw' to download).

You can find an online version of the task demonstration here: https://richarddmorey.github.io/Morey_Hoekstra_StatCognition/

Users of the package can also load a version of the experimental task from within R:

``` r
MoreyHoekstra2019::task_demo()
```

## Task code

Everything necessary to run a stripped-down version of the task code is under `inst/pkg_html`. This stripped down version runs in the local browser and does not save any data; if you want to save the data you'll need to run it in an environment set up to do that (e.g., Qualtrics, or write your own code depending on how you want to set it up). If you grab all the files in `inst/pkg_html` you'll have everything you need. Run the task by opening  `task.html` in a browser.

The source code for the experimental task can be found in the following files:

| Component  | Description  | File  |
|:--|:--|:--|
| Main control code / text | The HTML text and javascript code to run the task itself.  | `inst/pkg_html/task.html`   |
| Main support functions  | Custom javascript functions related specifically to running the task  | `inst/pkg_html/js/santa_utility.js`  |
| Setup variables  | Javascript objects related to setting up the task (defining toy names, effect sizes, etc)  | `inst/pkg_html/js/task_pars.js`  |
| Style | Settings for task interface components (width of elements, etc) | `inst/pkg_html/css/santa.css` |

jquery and jquery-ui are also required for the task to run properly. Versions of these libraries are included in the respective directories.



## Shiny app

Running the included shiny app will allow you to explore individual participants' response patterns.

``` r
MoreyHoekstra2019::explore_data()
```

## Data

The cleaned data with text responses are made available in the package in the following objects.

Object      | Description
------------|-------------
`christmas_stats_participants` | Summary information about each participant, including text responses 
`christmas_stats_samples` | Every individual sample made by each participant

## Figure list

Figures are exported objects in the package. You can recreate them with the functions below. For an explanation of the arguments to these functions, see the corresponding R help (e.g. `?MoreyHoekstra2019::fig_evidence_p_vals`). 

|Function                                              |Description |Source file                     |
|:-----------------------------------------------------|:-----------|:-------------------------------|
|`MoreyHoekstra2019::fig_evidence_p_vals()`           | Interface with p values overlaid. **Note**: this function writes an image file and returns the file path.           |`fig_evidence_p_vals.R`           |
|`MoreyHoekstra2019::fig_recreate_display()`          | Recreate the participant's interface at the time of their decision. **Note**: this function writes an image file and returns the file path.            |`fig_recreate_display.R`          |
|`MoreyHoekstra2019::fig_desc_evidence()`             | Distributions of evidence / x locations given effect size, sample size, and transform          |`fig_desc_evidence.R`             |
|`MoreyHoekstra2019::fig_desc_evidence_transform()`   | The two transforms (wide and narrow) that yield the evidence / x location            |`fig_desc_evidence.R`   |
|`MoreyHoekstra2019::fig_shuffle_understanding()`     | Participants' reported understanding of the usefulness of the null samples           |`fig_shuffle_understanding.R`     |
|`MoreyHoekstra2019::fig_sampling_behavior()`         | The number of null samples and experimental samples per participant           |`fig_sampling_behavior.R`  
|`MoreyHoekstra2019::fig_confidence()`                | Participants' ratings of confidence in their decision responses          |`fig_confidence.R`                |
|`MoreyHoekstra2019::fig_error_rates()`               | Decision error rates           |`fig_error_rates.R`               |
|`MoreyHoekstra2019::fig_response_evidence()`         | The sensitivity of participants' decisions to the evidence in the display           |`fig_response_evidence.R`         |
|`MoreyHoekstra2019::fig_evidence_scale_logistic_x()`            | Effect of wide/narrow evidence scale on average probability of responding "different", as a function of x location          |`fig_evidence_scale.R`            |
|`MoreyHoekstra2019::fig_evidence_scale_logistic_p()`            | Effect of wide/narrow evidence scale on average probability of responding "different", as a function of *p* value          |`fig_evidence_scale.R`            |
|`MoreyHoekstra2019::fig_evidence_scale_boxes()`      | Effect of wide/narrow evidence scale on number of null and experimental samples           |`fig_evidence_scale.R`      |
|`MoreyHoekstra2019::fig_field()`                     | Participants' reported scientific field           |`fig_field.R`                     |
|`MoreyHoekstra2019::fig_edu_training_edu()`          | Participants' reported level of education           |`fig_edu_training.R`          |
|`MoreyHoekstra2019::fig_edu_training_stats()`        | Participants' reported Number of years of format statistical training           |`fig_edu_training.R`        |
|`MoreyHoekstra2019::fig_field_scientific()`          | Participants' reported field: is it scientific?           |`fig_field_scientific.R`          |
|`MoreyHoekstra2019::fig_how_use_stat()`              | Participants' reported reasons for using statistics in their work           |`fig_how_use_stat.R`              |
|`MoreyHoekstra2019::fig_preferred_method()`          | Participants' reported preferred method of data analysis           |`fig_preferred_method.R`          |
|`MoreyHoekstra2019::fig_sig_testing_opinion()`       | Participants' reported opinion about significance testing           |`fig_sig_testing_opinion.R`       |


## Compressed data

Because the original csv file downloaded from Qualtrics has the timestamped information for each experiment for each participant in it, the file is quite large. It is stored in compressed in a zip file in the subfolder `inst/extdata/` to save space. You can load the original file in R using the command:

``` r
load_complete_dataset()
```

This will unzip the Qualtrics CSV file and check it against an MD5 hash. It will also do some light cleaning (filtering out non-respondents and rejected mobile users and filtering/renaming columns).


## Analysis code

All analysis code is available as package functions, from initial cleaning to figures. 

Function      | Description 
------------|-------------
`load_complete_dataset()` | Reads original data from csv file and performs initial filtering
`compile_complete_dataset()` | Compile the data, separating out the individual participant experimental samples and performing appropriate re-coding, etc.
`load_clean_dataset()` | Main cleaning of the data set to reduce the sample down to only those that will be analyzed
`load_text_responses()` | Load the text responses separately for convenience



