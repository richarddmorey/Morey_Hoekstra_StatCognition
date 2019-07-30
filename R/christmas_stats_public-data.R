
#' Cleaned, participant-level data
#'
#' Participant-level data for the Christmas statistical
#' cognition experiment
#'
#' \itemize{
#' \item duration: Qualtrics-reported time taken in the experiment (seconds)
#' \item id: participant id
#' \item consent: Response to informed consent
#' \item times: "Have you participated previously"?
#' \item shuffle: "Do you understand use of shuffle reports?"
#' \item shuffle_other" (text) response for "Other" responses to `shuffle`
#' \item response: primary response: which team is faster?
#' \item confidence: participant's confidence in their `response`
#' \item salient_factors: (text) "What facts or observations were most salient in coming to the conclusion that you did?"
#' \item expt_strategy: (text) "Please describe your experimental strategy, if any, in your own words."
#' \item shuffle_desc: (text) "Did you make use of the "random shuffle reports"? If so, how?"
#' \item is_science: "Is your work in a field that would typically be considered scientific?"
#' \item is_science_other: (text) response for "Other" responses to `is_science`
#' \item education: highest level of formal education in scientific field
#' \item formal_training: years of formal statistical training
#' \item how_use: "How do statistics play a role in your work?" Concatenated string of responses
#' \item field: "In what applied field(s) do you use statistics?" Concatenated string of responses
#' \item preferred: "What sort of inferential procedures would you typically prefer?"
#' \item preferred_other: (text) response for "Other" responses to `preferred`
#' \item sig_testing: "What is your opinion about statistical significance testing?" Concatenated string of responses
#' \item mobile: Qualtrics flag for mobile browsers. Should be blank for all participants
#' \item evidence_power: Randomly-assigned transformation power. 3 or 7
#' \item effect_size: "true" effect size in standard deviation units, hidden from the participant
#' \item finished_practice_timestamp: javascript timestamp reported by browser when participant ended the instructions. Indexed to initial_timestamp
#' \item downloaded_expt: Did the participant download their experimental samples?
#' \item downloaded_null: Did the participant download their null samples?
#' \item initial_timestamp: initial timestamp (should be 0, start of experiment)
#' \item true_winner: Which "team" was the true winner? Corresponds to sign of effect_size
#' \item true_null: Was the effect size 0?
#' \item response_null: Was the participant's response "null"? (same or no detection)
#' \item response_alt: Negation of `response_null`
#' \item n_expt: Number of experimental samples requested
#' \item n_null: Number of random shuffle reports requested
#' \item how_use.analysis: (logical) Did the participant select the corresponding response to `how_use`?
#' \item how_use.develop: (logical) Did the participant select the corresponding response to `how_use`?
#' \item how_use.philosophy: (logical) Did the participant select the corresponding response to `how_use`?
#' \item how_use.method_comment: (logical) Did the participant select the corresponding response to `how_use`?
#' \item how_use.none: (logical) Did the participant select the corresponding response to `how_use`?
#' \item how_use.other: (logical) Did the participant select the corresponding response to `how_use`?
#' \item field.bio: (logical) Did the participant select the corresponding response to `field`?
#' \item field.phys: (logical) Did the participant select the corresponding response to `field`?
#' \item field.soc_beh: (logical) Did the participant select the corresponding response to `field`?
#' \item field.comp_tech: (logical) Did the participant select the corresponding response to `field`?
#' \item field.med: (logical) Did the participant select the corresponding response to `field`?
#' \item field.other: (text) If applicable, text response to "Other" responses for `field`.
#' \item sig_testing.necessary: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.prefer_other: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.misunderstood: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.no_opinion: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.discontinued: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.fatally_flawed: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.do_not_understand: (logical) Did the participant select the corresponding response to `sig_testing`?
#' \item sig_testing.other: (text) If applicable, text response to "Other" responses to `sig_testing`
#' \item text_comparison: (Strategy questions) Response mentioned comparison to the random shuffle reports
#' \item text_asymmetry: (Strategy questions) Response mentioned symmetry/asymmetry in the experimental samples
#' \item text_sampling_var: (Strategy questions) Response mentioned random shuffle reports as a means to assess sampling variability,
#' distribution under the null, chance distribution, etc
#' \item text_inc_asymmetry: (Strategy questions) Response mentioned increasing asymmetry (or lack thereof) as sample size increased
#' \item text_no_shuffle: (Strategy questions) Explicitly said they did not use the shuffle reports
#' \item text_irrelevant: (Strategy questions) Text was irrelevant
#' \item text_missing: (Strategy questions) Text was missing
#' }
#'
#' @name christmas_stats_participants
#' @docType data
#' @author Richard D. Morey \email{richarddmorey@gmail.com}
#' @keywords data
NULL

#' Cleaned, event-level data
#'
#' Event-level data for the Christmas statistical cognition
#' experiment
#'
#' \itemize{
#' \item id: participant id
#' \item ev: (absolute value of) strength of "evidence" in experiment.
#' This is the transformed two-tailed p value.
#' \item sgn: sign of evidence (-1 means favoring left side)
#' \item n_idx: index of sample size selected by participant (0-19)
#' \item n: "true" sample size per group, hidden from the participant
#' \item d: "true" effect size, hidden from the participant (same as effect_size in christmas_stats_participants)
#' \item time: javascript timestamp (ms) reported by browser. This was when the point
#' was place on the interface. Indexed to initial_timestamp
#' \item toy_name: name of toy randomly assigned to participant
#' \item initial_timestamp: (should be 0, start of experiment)
#' \item type: 'expt' for experimental samples, 'null' for
#' samples from random shuffle reports
#' \item initiated_timestamp: javascript timestamp (ms) when the sample request was
#' initiated by the participant. Indexed to initial_timestamp
#' }
#'
#' @name christmas_stats_samples
#' @docType data
#' @author Richard D. Morey \email{richarddmorey@gmail.com}
#' @keywords data
NULL

#' Coding of the text responses
#'
#' Categorization of each participants' strategy-related
#' free-text responses
#'
#' \itemize{
#' \item id: participant id
#' \item text_comparison: Response mentioned comparison to the random shuffle reports
#' \item text_asymmetry: Response mentioned symmetry/asymmetry in the experimental samples
#' \item text_sampling_var: Response mentioned random shuffle reports as a means to assess sampling variability,
#' distribution under the null, chance distribution, etc
#' \item text_inc_asymmetry: Response mentioned increasing asymmetry (or lack thereof) as sample size increased
#' \item text_no_shuffle: Explicitly said they did not use the shuffle reports
#' \item text_irrelevant: Text was irrelevant
#' \item text_missing: Text was missing
#' }
#'
#' @name text_coding
#' @docType data
#' @author Richard D. Morey \email{richarddmorey@gmail.com}
#' @keywords data
NULL
