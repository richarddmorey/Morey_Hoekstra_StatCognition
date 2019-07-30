#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(purrr)
library(gt)
library(magrittr)

ima <- MoreyHoekstra2019:::ima
dat <- MoreyHoekstra2019::christmas_stats_participants
samples <- MoreyHoekstra2019::christmas_stats_samples

repl_text = c(
    text_comparison = "Comparison to null",
    text_asymmetry = "Symmetry/asymmetry",
    text_sampling_var = "Shuffles as sampling variability/null,chance distribution",
    text_inc_asymmetry = "Increasing asymmetry",
    text_no_shuffle = "Explicitly said no use of random shuffles",
    text_irrelevant = "Text irrelevant",
    text_missing = "Text missing"
)


suppressWarnings({w_stats = samples %>%
    mutate(ev0 = ev * sgn) %>%
    split(.$id) %>%
    map( ~ wilcox.test(ev0 ~ type, alternative = "less", data = .)) %>%
    map_dbl("p.value")})

w_stats <- w_stats[match(dat$id, names(w_stats))]

dat$wilcoxon = ifelse(w_stats < .5,
                      w_stats*2,
                      (1 - w_stats)/2)

values <- reactiveValues(dat = dat)

shinyServer(function(input, output) {

    output$santaDisplay <- MoreyHoekstra2019::renderSantaDisplay({
        idx = input$dataTable_rows_selected

        if(length(idx)==0 | nrow(values$dat)==0)
            stop("Please select a row in the Data tab.")

        id = values$dat[idx, "id"]

        MoreyHoekstra2019::santaDisplay(id)
    })

    output$distributionsPlot <- renderPlot({
        idx = input$dataTable_rows_selected

        if(length(idx)==0 | nrow(values$dat)==0){
            plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
            text(x = 0.5, y = 0.5, "Please select a row in the Data tab.",
                 cex = 1.6, col = "black")
            return(NULL)
        }

        type = input$type

        id = values$dat[idx, "id"]
        if(type == "expt"){
            d = values$dat[idx, "effect_size"]
            n = NULL
        }else{
            d = 0
            n = 10
        }

        q = values$dat[idx, "evidence_power"]


        MoreyHoekstra2019::fig_desc_evidence(q = q, d = d, n = NULL)
    })

    output$displayPlot <- renderImage({

        idx = input$dataTable_rows_selected

        if(length(idx)==0 | nrow(values$dat)==0){
            fn = tempfile(fileext = ".png")
            png(filename = fn)
            plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
            text(x = 0.5, y = 0.5, "Please select a row in the Data tab.",
                 cex = 1.6, col = "black")
            dev.off()
            return(
                list(src = fn,
                 contentType = 'image/png',
                 width = "90%",
                 alt = "Evidence display plot")
            )
        }

        id = values$dat[idx, "id"]
        type = input$type
        if(input$pvals){
            ps = c(.005, .025, .05, .125, .25, .5, .75, .875, .95, .975, .995)
        }else{
            ps = numeric(0)
        }
        fn = MoreyHoekstra2019::fig_recreate_display(id = id, type = type, ps = ps)

        # Return a list containing the filename
        list(src = fn,
             contentType = 'image/png',
             width = "90%",
             alt = "Evidence display plot")

    })

    output$dataTable <-
        DT::renderDT(
            options = list(dom = 'tp'),
            selection = list(mode = 'single', selected = 1),
            filter = 'top',
            expr = {
                values$dat %>%
                    mutate(
                        response_type = case_when(
                            true_winner != "null" &
                                true_winner != response &
                                response %in% c("jinglies", "sparklies") ~ "sign error",
                            response %in% c("jinglies", "sparklies") &
                                true_winner != "null" ~ "hit",
                            response %in% c("jinglies", "sparklies") &
                                true_winner == "null" ~ "false positive",
                            response %in% c("same", "no_detect") &
                                true_winner == "null" ~ "correct retain",
                            response %in% c("same", "no_detect") &
                                true_winner != "null" ~ "false negative"
                        )
                    ) %>%
                    select(
                        id,
                        effect_size,
                        response,
                        response_type,
                        evidence_power,
                        wilcoxon,
                        n_null,
                        n_expt,
                        contains("text_")
                    )
            }
        )
    output$strategyText <- renderUI({

        idx = input$dataTable_rows_selected
        filt = input$strategyTextFilter
        if(length(idx)==0 | nrow(values$dat)==0)
            return(HTML("Please select a row in the Data tab."))

        id = values$dat[idx, "id"]
        salient = gsub(paste0("(",filt,")"),
                       "<b>\\1</b>",
                       ignore.case = TRUE,
                       values$dat[idx, "salient_factors"])
        strategy = gsub(paste0("(",filt,")"),
                        "<b>\\1</b>",
                        ignore.case = TRUE,
                        values$dat[idx, "expt_strategy"])
        shuffle = gsub(paste0("(",filt,")"),
                       "<b>\\1</b>",
                       ignore.case = TRUE,
                       values$dat[idx, "shuffle_desc"])

        values$dat %>%
            filter(id == !!id) %>%
            select(contains("text_")) %>%
            unlist() -> w_text

        w_text = repl_text[w_text]

        tagList(
            tags$br(),
            tags$div(HTML(paste0("Below are participant <i>",id,"</i>'s answers to the three strategy questions."))),
            tags$br(),
            tags$div(HTML("<i>What facts or observations were most salient in coming to the conclusion that you did (whatever that was)?</i>")),
            tags$div(HTML(salient), style="width: 85%; border: solid 1px black; padding: 10px; color: red;"),
            tags$br(),
            tags$div(HTML("<i>Please describe your experimental strategy, if any, in your own words.</i>")),
            tags$div(HTML(strategy), style="width: 85%; border: solid 1px black; padding: 10px; color: red;"),
            tags$br(),
            tags$div(HTML("<i>Did you make use of the 'random shuffle reports'? If so, how?</i>")),
            tags$div(HTML(shuffle), style="width: 85%; border: solid 1px black; padding: 10px; color: red;"),
            tags$div(
                tags$br(),
                "This text was coded as belonging to the following categories: ",
                tags$ul(
                    shiny::tagList(lapply(w_text, shiny::tags$li))
                )
            )
        )


    })
    observe({
        filt = input$strategyTextFilter
        m <- try(dat %>%
            filter(
                grepl(!!filt, salient_factors, ignore.case = TRUE) |
                    grepl(!!filt, expt_strategy, ignore.case = TRUE) |
                    grepl(!!filt, shuffle_desc, ignore.case = TRUE)
            ), silent = TRUE)
        if(inherits(m, "try-error")){
            #stop("Invalid regular expression in filter.")
        }else{
            values$dat <- m
        }
    })
})
