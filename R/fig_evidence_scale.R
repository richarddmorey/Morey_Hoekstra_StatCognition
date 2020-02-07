



#' Number of samples as a function of sample type, evidence scale
#' and true effect size
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#' @importFrom purrr map map_df
#' @importFrom gridExtra grid.arrange
#' @importFrom MASS rlm
#' @importFrom scales hue_pal
#' @export
#'
#' @examples
#' suppressWarnings(
#'   fig_evidence_scale_boxes()
#' )
fig_evidence_scale_boxes <- function(
        filter_func = function(data, ...) return(data),
        print_plot = TRUE
        ) {
        dat <- MoreyHoekstra2019::christmas_stats_participants %>%
                filter_func() %>%
                mutate(
                        effect_size = abs(effect_size),
                        ln_null = log10(n_null),
                        ln_expt = log10(n_expt)
                )


        cnull <- dat %>%
                split(.$evidence_power) %>%
                map( ~ MASS::rlm(ln_null ~ effect_size, data = .)) %>%
                map_df(coef)


        cexpt <- dat %>%
                split(.$evidence_power) %>%
                map( ~ MASS::rlm(ln_expt ~ effect_size, data = .)) %>%
                map_df(coef)

        # grouped boxplot
        p1 = ggplot(dat,
                    aes(
                            x = effect_size,
                            y = n_null,
                            group = interaction(evidence_power, effect_size),
                            fill = factor(evidence_power)
                    )) +
                geom_boxplot() +
                geom_abline(
                        intercept = cnull$`3`[1],
                        slope = cnull$`3`[2],
                        color = scales::hue_pal()(2)[1]
                ) +
                geom_abline(
                        intercept = cnull$`7`[1],
                        slope = cnull$`7`[2],
                        color = scales::hue_pal()(2)[2]
                ) +
                #geom_point(position=position_dodge(width=0.06),aes(group=interaction(evidence_power, effect_size))) +
                scale_y_continuous(trans = 'log10') +
                theme(legend.position = "none",
                      text = element_text(family = pkg_options("ggplot_family"))) +
                ylab("Number of samples") + ggtitle("Null") +
                xlab("Effect size")

        # grouped boxplot
        p2 = ggplot(dat,
                    aes(
                            x = effect_size,
                            y = n_expt,
                            group = interaction(evidence_power, effect_size),
                            fill = factor(evidence_power)
                    )) +
                geom_boxplot() +
                geom_abline(
                        intercept = cexpt$`3`[1],
                        slope = cexpt$`3`[2],
                        color = scales::hue_pal()(2)[1]
                ) +
                geom_abline(
                        intercept = cexpt$`7`[1],
                        slope = cexpt$`7`[2],
                        color = scales::hue_pal()(2)[2]
                ) +
                scale_y_continuous(trans = 'log10') +
                theme(legend.position = "none",
                      text = element_text(family = pkg_options("ggplot_family"))) +
                ylab("Number of samples") + ggtitle("Experimental") +
                xlab("Effect size")

        if(print_plot)
                gridExtra::grid.arrange(p1, p2, ncol = 2)


        invisible( list(null = p1, expt = p2) )
}


#' Probability of making a "difference" decision as a function of the most extreme x value and evidence scale
#'
#' @param pval vector of p values at which to indicate the scale of the respective null sampling distribution
#' @param seed random seed for jitter of points
#' @param shift_y How far above/below the main graph to put the middle of the "scatter" plots
#' @param around_y How far above/below the middle of the "scatter" plots to put the points
#' @param jitter_y How much to jutter the points in the y direction
#' @param filter_func function to filter the data before plotting
#'
#' @return Invisibly returns the corresponding GLM test result
#' @import dplyr
#' @importFrom purrr map
#' @importFrom showtext showtext_begin showtext_end
#' @importFrom scales hue_pal
#'
#' @export
#'
#' @examples
#' fig_evidence_scale_logistic_x()
fig_evidence_scale_logistic_x <- function(pval = .05,
                                          seed = 1,
                                          shift_y = .20,
                                          around_y = .1,
                                          jitter_y = around_y/4,
                                          filter_func = function(data, ...) return(data)){
        set.seed(seed)

        colors_ggplot = scales::hue_pal()(2)
        colors_ggplot_t = paste0(colors_ggplot, "55")

        showtext::showtext_begin()

        plot(0,0,ylim = c(-.41,1.41), xlim = c(0,1), typ='n', las = 1,
             xlab = "Most extreme evidence / x location",
             ylab = "Prob. Sparklies/Jinglies responses", xaxs = 'i',
             yaxs = 'i', axes=FALSE)
        legend("left",legend = c("Wide (3)","Narrow (7)"), col = colors_ggplot,
               lwd = 2, bty = 'n', lty = 1:2)
        mtext("Most extreme evidence / x location", 3, par()$mgp[1])

        text(1, shift_y + 1 + around_y, "J/S", adj = -.3, xpd = TRUE)
        text(1, -shift_y + around_y, "J/S", adj = -.3, xpd = TRUE)

        text(1, shift_y + 1 - around_y, "N", adj = -.5, xpd = TRUE)
        text(1, -shift_y  - around_y, "N", adj = -.5, xpd = TRUE)


        axis(1)
        axis(3)
        rect(0,0,1,1, xpd = TRUE)
        axis(2, at = seq(0,1,len = 5), las = 1)
        #text(0, 1, "Responded Sparklies/Jinglies", adj=c(-.1, -.5))
        #text(0, 0, "Responded no-detect/same", adj=c(-.1, 1.5))

        MoreyHoekstra2019::christmas_stats_participants %>%
                filter_func() %>% select(id) %>% unlist() -> valid_id

        MoreyHoekstra2019::christmas_stats_samples %>%
                filter(type == "expt" & id %in% valid_id) %>%
                left_join(MoreyHoekstra2019::christmas_stats_participants, by="id") %>%
                mutate(es = abs(effect_size)) %>%
                group_by(id, es, response_alt, evidence_power) %>%
                summarize(ev = max(abs(ev))) -> dat_ev

        dat_ev %>% glm(response_alt ~ ev*factor(evidence_power),
                       family = binomial, data = .) -> glm_obj

        cf = coef(glm_obj)
        intercepts = c(cf[1], cf[1] + cf[3])
        slopes = c(cf[2], cf[2] + cf[4])

        dat_ev %>%
                group_by(evidence_power) %>%
                arrange(ev) %>%
                mutate(cp = cumsum(response_alt)/n()) %>%
                split(.$evidence_power) %>%
                map( ~ {
                        evp = (.$evidence_power[1]==7)
                        points(.$ev, jitter(
                                around_y * c(-1,1)[.$response_alt+1] + shift_y * c(-1,1)[evp + 1] + evp,
                                amount = jitter_y)
                               ,
                               col = colors_ggplot_t[evp+1],
                               pch = 19, xpd = TRUE)
                        xx = seq(.001, .999, len = 100)
                        yy = plogis(intercepts[evp+1] + xx*slopes[evp+1])
                        abline(h = evp + shift_y * c(-1,1)[evp+1] + c(-1,1)*around_y, col = rgb(0,0,0,.5))
                        lines(xx, yy, col = colors_ggplot[evp+1], lwd = 2, lty = evp+1)
                        #yy = evp + shift_y * c(-1,1)[evp+1] +
                        #        around_y * 2 * (.$cp - .5)
                        #lines(.$pval2,
                        #      yy, col = rgb(0,0,0,.5), lty = 2, xpd = TRUE)
                        # Standard errors
                        predict_data <- expand.grid(ev = xx, evidence_power = .$evidence_power[1])
                        preds <- predict(glm_obj,
                                         newdata = predict_data,
                                         type = "link",
                                         se.fit = TRUE)
                        bands <- plogis(preds$fit + outer(preds$se.fit, c(-1,1), "*"))
                        polygon( c(xx, rev(xx) ),
                                c(bands[,1], rev(bands[,2])),
                                border = NA, col = colors_ggplot_t[evp+1]
                        )
                        # Text
                        qq = MoreyHoekstra2019:::evz(qnorm(1 - pval/2), .$evidence_power[1])
                        text(qq, 1, labels = paste0("p = ",pval), srt = 90, adj = c(1.1,1.2),
                             col = colors_ggplot[evp+1])
                        segments(qq, par()$usr[3+evp], qq, 1-evp, col = colors_ggplot[evp+1], lty = evp+1, lwd = 1)

                })

        showtext::showtext_end()

        invisible(glm_obj)
}


#' Probability of making a "difference" decision as a function of the most extreme p value and evidence scale
#'
#' @param pval vector of p values at which to indicate the scale of the respective null sampling distribution
#' @param seed random seed for jitter of points
#' @param shift_y How far above/below the main graph to put the middle of the "scatter" plots
#' @param around_y How far above/below the middle of the "scatter" plots to put the points
#' @param jitter_y How much to jutter the points in the y direction
#' @param filter_func function to filter the data before plotting
#'
#' @return Invisibly returns the corresponding GLM test result
#' @import dplyr
#' @importFrom purrr map
#' @importFrom showtext showtext_begin showtext_end
#' @importFrom scales hue_pal
#' @export
#'
#' @examples
#' fig_evidence_scale_logistic_p()
fig_evidence_scale_logistic_p <- function(pval = .05,
                                          seed = 1,
                                          shift_y = .20,
                                          around_y = .1,
                                          jitter_y = around_y/4,
                                          filter_func = function(data, ...) return(data)){
        set.seed(seed)

        colors_ggplot = scales::hue_pal()(2)
        colors_ggplot_t = paste0(colors_ggplot, "55")

        trans_p <- function(p)
                -evz(qnorm(p/2), q=7)

        showtext::showtext_begin()

        plot(0,0,ylim = c(-.41,1.41), xlim = c(0,1), typ='n', las = 1,
             xlab = "Most extreme p value",
             ylab = "Prob. Sparklies/Jinglies response", xaxs = 'i',
             yaxs = 'i', axes=FALSE)
        legend("left",legend = c("Wide (3)","Narrow (7)"), col = colors_ggplot,
               lwd = 2, bty = 'n', lty = 1:2)
        mtext("Most extreme p value", 3, par()$mgp[1])

        text(1, shift_y + 1 + around_y, "J/S", adj = -.3, xpd = TRUE)
        text(1, -shift_y + around_y, "J/S", adj = -.3, xpd = TRUE)

        text(1, shift_y + 1 - around_y, "N", adj = -.5, xpd = TRUE)
        text(1, -shift_y  - around_y, "N", adj = -.5, xpd = TRUE)

        p_at = c(1,.25,.05,.01,.001,.00005,0)
        p_lab = format(p_at, drop0=TRUE)
        axis(1, at = trans_p(p_at), lab = p_lab)
        axis(3, at = trans_p(p_at), lab = p_lab)

        rect(0,0,1,1, xpd = TRUE)
        axis(2, at = seq(0,1,len = 5), las = 1)
        #text(0, 1, "Responded Sparklies/Jinglies", adj=c(-.1, -.5))
        #text(0, 0, "Responded no-detect/same", adj=c(-.1, 1.5))

        MoreyHoekstra2019::christmas_stats_participants %>%
                filter_func() %>% select(id) %>% unlist() -> valid_id

        MoreyHoekstra2019::christmas_stats_samples %>%
                filter(type == "expt" & id %in% valid_id) %>%
                left_join(MoreyHoekstra2019::christmas_stats_participants, by="id") %>%
                mutate(es = abs(effect_size),
                       pval2 = 2*p.ev(-ev, evidence_power) ) %>%
                group_by(id, es, response_alt, evidence_power) %>%
                summarize(pval2 = trans_p(min(pval2))) -> dat_ev

        dat_ev %>% glm(response_alt ~ pval2*factor(evidence_power),
                       family = binomial, data = .) -> glm_obj

        cf = coef(glm_obj)
        intercepts = c(cf[1], cf[1] + cf[3])
        slopes = c(cf[2], cf[2] + cf[4])

        dat_ev %>%
                group_by(evidence_power) %>%
                arrange(pval2) %>%
                mutate(cp = cumsum(response_alt)/n()) %>%
                split(.$evidence_power) %>%
                map( ~ {
                        evp = (.$evidence_power[1]==7)
                        points(.$pval2, jitter(
                                around_y * c(-1,1)[.$response_alt+1] + shift_y * c(-1,1)[evp + 1] + evp,
                                amount = jitter_y)
                               ,
                               col = colors_ggplot_t[evp+1],
                               pch = 19, xpd = TRUE)
                        xx = seq(.001, .999, len = 100)
                        yy = plogis(intercepts[evp+1] + xx*slopes[evp+1])
                        abline(h = evp + shift_y * c(-1,1)[evp+1] + c(-1,1)*around_y, col = rgb(0,0,0,.5))
                        lines(xx, yy, col = colors_ggplot[evp+1], lwd = 2, lty = evp+1)
                        #yy = evp + shift_y * c(-1,1)[evp+1] +
                        #        around_y * 2 * (.$cp - .5)
                        #lines(.$pval2,
                        #      yy, col = rgb(0,0,0,.5), lty = 2, xpd = TRUE)
                        # Standard errors
                        predict_data <- expand.grid(pval2 = xx, evidence_power = .$evidence_power[1])
                        preds <- predict(glm_obj,
                                         newdata = predict_data,
                                         type = "link",
                                         se.fit = TRUE)
                        bands <- plogis(preds$fit + outer(preds$se.fit, c(-1,1), "*"))
                        polygon( c(xx, rev(xx) ),
                                 c(bands[,1], rev(bands[,2])),
                                 border = NA, col = colors_ggplot_t[evp+1]
                        )
                })

        qq = trans_p(pval)
        text(qq, 1, labels = paste0("p = ",pval), srt = 90, adj = c(1.1,1.2),
             col = "gray")
        segments(qq, par()$usr[3], qq, par()$usr[4], col = "gray", lty = 4, lwd = 1)

        showtext::showtext_end()

        invisible(glm_obj)
}

